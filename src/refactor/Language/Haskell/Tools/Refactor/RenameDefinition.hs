{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , MultiWayIf
           , TypeApplications
           , ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           , ViewPatterns
           #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition, renameDefinition', DomainRenameDefinition) where

import Name hiding (Name)
import GHC (Ghc, TyThing(..), lookupName)
import qualified GHC
import OccName
import SrcLoc
import Outputable

import Control.Reference hiding (element)
import qualified Control.Reference as Ref
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import Debug.Trace

type DomainRenameDefinition dom = ( Domain dom, HasNameInfo (SemanticInfo' dom SameInfoNameCls), Data (SemanticInfo' dom SameInfoNameCls)
                                  , HasScopeInfo (SemanticInfo' dom SameInfoNameCls), HasDefiningInfo (SemanticInfo' dom SameInfoNameCls)
                                  , SemanticInfo' dom SameInfoWildcardCls ~ ImplicitFieldInfo, HasModuleInfo (SemanticInfo dom Module) )

renameDefinition' :: forall dom . DomainRenameDefinition dom => RealSrcSpan -> String -> Refactoring dom
renameDefinition' sp str mod mods
  = case (getNodeContaining sp (snd mod) :: Maybe (Ann SimpleName dom SrcTemplateStage)) >>= (fmap getName . (semanticsName =<<) . (^? semantics)) of 
      Just name -> do let sameNames = bindsWithSameName name (snd mod ^? biplateRef) 
                      renameDefinition name sameNames str mod mods
        where bindsWithSameName :: GHC.Name -> [Ann FieldWildcard dom SrcTemplateStage] -> [GHC.Name]
              bindsWithSameName name wcs = catMaybes $ map ((lookup name) . (^. semantics&implicitFieldBindings)) wcs
      Nothing -> refactError "No name is selected"

renameDefinition :: DomainRenameDefinition dom => GHC.Name -> [GHC.Name] -> String -> Refactoring dom
renameDefinition toChangeOrig toChangeWith newName mod mods
    = do nameCls <- classifyName toChangeOrig
         (changedModules,defFound) <- runStateT (catMaybes <$> mapM (renameInAModule toChangeOrig toChangeWith newName) (mod:mods)) False
         if | not (nameValid nameCls newName) -> refactError "The new name is not valid"
            | not defFound -> refactError "The definition to rename was not found"
            | otherwise -> return changedModules
  where     
    renameInAModule :: DomainRenameDefinition dom => GHC.Name -> [GHC.Name] -> String -> ModuleDom dom -> StateT Bool Refactor (Maybe (ModuleDom dom))
    renameInAModule toChangeOrig toChangeWith newName (name, mod)
      = mapStateT (localRefactoringRes (\f (a,s) -> (fmap (\(n,r) -> (n, f r)) a,s)) mod) $
          do (res, isChanged) <- runStateT (biplateRef !~ changeName toChangeOrig toChangeWith newName $ mod) False
             if isChanged then return $ Just (name, res)
                          else return Nothing

    changeName :: DomainRenameDefinition dom => GHC.Name -> [GHC.Name] -> String -> Ann SimpleName dom SrcTemplateStage 
                                                         -> StateT Bool (StateT Bool (LocalRefactor dom)) (Ann SimpleName dom SrcTemplateStage)
    changeName toChangeOrig toChangeWith str name 
      = if | maybe False (`elem` toChange) actualName
               && semanticsDefining (name ^. semantics) == False
               && any @[] ((str ==) . occNameString . getOccName) (semanticsScope (name ^. semantics) ^? Ref.element 0 & traversal)
             -> refactError "The definition clashes with an existing one" -- name clash with an external definition
           | maybe False (`elem` toChange) actualName
             -> do put True -- state that something is changed in the local state
                   when (actualName == Just toChangeOrig) 
                     $ lift $ modify (|| semanticsDefining (name ^. semantics)) -- state that the definition is renamed in the global state
                   return $ element & unqualifiedName .= mkNamePart str $ name -- found the changed name (or a name that have to be changed too)
           | let namesInScope = semanticsScope (name ^. semantics)
              in case semanticsName (name ^. semantics) of 
                   Just (getName -> exprName) -> str == occNameString (getOccName exprName) && sameNamespace toChangeOrig exprName
                                                   && conflicts toChangeOrig exprName namesInScope
                   Nothing -> False -- ambiguous names
             -> refactError "The definition clashes with an existing one" -- local name clash
           | otherwise -> return name -- not the changed name, leave as before
      where toChange = toChangeOrig : toChangeWith
            actualName = fmap getName (semanticsName (name ^. semantics))

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope) 
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)
