{-# LANGUAGE LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Control.Reference hiding (element)
import Data.Maybe
import Data.List
import Data.Char
import Data.Map as Map hiding (map, filter)
import Data.IORef
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Data.StructuralTraversal
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Avail as GHC
import GHC as GHC
import GhcMonad as GHC
import ApiAnnotation as GHC
import RdrName as GHC
import Name as GHC hiding (varName)
import Id as GHC
import SrcLoc as GHC
import FastString as GHC
import Module as GHC
import BasicTypes as GHC
import HsSyn as GHC
import HscTypes as GHC
import Outputable as GHC
import TyCon as GHC
import ConLike as GHC
import DataCon as GHC
import Bag as GHC
import Var as GHC
import PatSyn as GHC
import Type as GHC
import Unique as GHC
import CoAxiom as GHC

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), AnnList(..), RangeWithName, RangeWithType, RangeInfo
                                  , SemanticInfo(..), semanticInfo, sourceInfo, semantics, annotation, nameInfo, nodeSpan)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

addTypeInfos :: LHsBinds Id -> Ann AST.Module RangeWithName -> Ghc (Ann AST.Module RangeWithType)
addTypeInfos bnds mod = evalStateT (traverseDown (return ()) (return ()) replaceNodeInfo mod) []
  where replaceNodeInfo :: RangeWithName -> StateT [GHC.Var] Ghc RangeWithType
        replaceNodeInfo = semanticInfo !~ replaceSemanticInfo
        replaceSemanticInfo NoSemanticInfo = return NoSemanticInfo
        replaceSemanticInfo (ScopeInfo sc) = return $ ScopeInfo sc
        replaceSemanticInfo (DefinitionInfo (Just n)) = getType n >> return (DefinitionInfo (Just n))
        replaceSemanticInfo (DefinitionInfo Nothing) = return $ DefinitionInfo Nothing
        replaceSemanticInfo (AmbiguousNameInfo sc d rdr l) = return $ NameInfo sc d (locMapping ! l)
        replaceSemanticInfo (ModuleInfo mod) = return (ModuleInfo mod)
        replaceSemanticInfo (NameInfo sc def ni) = NameInfo sc def <$> getType' ni
        replaceSemanticInfo (ImportInfo mod access used) = ImportInfo mod <$> mapM getType' access <*> mapM getType' used
        
        getType' :: GHC.Name -> StateT [GHC.Id] Ghc GHC.Id
        getType' name = fromMaybe (error $ "Type of name '" ++ showSDocUnsafe (ppr name) ++ "' cannot be found") <$> getType name
        getType name 
          = lift (lookupName name) >>= \case
              Just (AnId id) -> do modify (universeBi (varType id) ++)
                                   return (Just id)
              Just (AConLike (RealDataCon dc)) -> return $ Just $ mkVanillaGlobal name (dataConUserType dc)
              Just (AConLike (PatSynCon ps)) -> return $ Just $ mkVanillaGlobal name (createPatSynType ps)
              Just (ATyCon tc) -> do modify (++ getTypeVariables tc)
                                     return $ Just $ mkVanillaGlobal name (tyConKind tc)
              Nothing -> case Map.lookup name mapping of 
                           Just id -> do modify (universeBi (varType id) ++)
                                         return (Just id)
                           -- here we compare on occurrence names, because GHC does some re-naming
                           -- this is not a problem, because every time the closest name will be added
                           Nothing -> gets (find (\v -> GHC.varName v == name))
        mapping = Map.fromList $ map (\id -> (getName id, id)) $ extractTypes bnds
        locMapping = Map.fromList $ map (\(L l id) -> (l, id)) $ extractExprIds bnds
        createPatSynType patSyn = case patSynSig patSyn of (_, _, _, _, args, res) -> mkFunTys args res
        getTypeVariables tc = tyConTyVars tc ++ concat (typeVarsInEqs tc)

extractTypes :: LHsBinds Id -> [Id]
extractTypes = concatMap universeBi . bagToList

extractExprIds :: LHsBinds Id -> [Located Id]
        -- expressions like HsRecFld are removed from the typechecked representation, they are replaced by HsVar
extractExprIds = catMaybes . map (\case (L l (HsVar (L _ n))) -> Just (L l n); _ -> Nothing) . concatMap universeBi . bagToList


trfModule :: Located (HsModule RdrName) -> Trf (Ann AST.Module RangeInfo)
trfModule = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
  \(HsModule name exports imports decls deprec _) -> 
    AST.Module <$> trfFilePragmas
               <*> trfModuleHead name exports deprec
               <*> trfImports imports
               <*> trfDecls decls
       
trfModuleRename :: Module -> Ann AST.Module RangeInfo -> (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString) -> Located (HsModule RdrName) -> Trf (Ann AST.Module RangeWithName)
trfModuleRename mod rangeMod (gr,imports,exps,_) 
  = addModuleInfo mod <=< (trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
      \hsMod@(HsModule name exports _ decls deprec _) -> 
        setOriginalNames originalNames
          $ AST.Module <$> trfFilePragmas
                       <*> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                       _                       -> Nothing) deprec
                       <*> (orderAnnList <$> (trfImports imports))
                       <*> trfDeclsGroup gr)
  where addModuleInfo :: Module -> Ann AST.Module RangeWithName -> Trf (Ann AST.Module RangeWithName)
        addModuleInfo m = AST.semantics != ModuleInfo m

        originalNames = Map.fromList $ catMaybes $ map getSourceAndInfo (rangeMod ^? biplateRef) 
        getSourceAndInfo :: Ann AST.SimpleName RangeInfo -> Maybe (SrcSpan, RdrName)
        getSourceAndInfo n = (,) <$> (n ^? annotation&sourceInfo&nodeSpan) <*> (n ^? semantics&nameInfo)

        
trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Maybe (Located WarningTxt) -> Trf (AnnMaybe AST.ModuleHead r) 
trfModuleHead (Just mn) exports modPrag
  = makeJust <$> (annLoc (tokensLoc [AnnModule, AnnWhere])
                         (AST.ModuleHead <$> trfModuleName mn 
                                         <*> trfExportList (srcSpanEnd $ getLoc mn) exports
                                         <*> trfModulePragma modPrag))
trfModuleHead _ Nothing _ = nothing "" "" moduleHeadPos
  where moduleHeadPos = after AnnClose >>= \case loc@(RealSrcLoc _) -> return loc
                                                 _ -> atTheStart

trfFilePragmas :: RangeAnnot a => Trf (AnnList AST.FilePragma a)
trfFilePragmas = do pragmas <- asks pragmaComms
                    languagePragmas <- mapM trfLanguagePragma (fromMaybe [] $ (Map.lookup "LANGUAGE") pragmas)
                    optionsPragmas <- mapM trfOptionsPragma (fromMaybe [] $ (Map.lookup "OPTIONS_GHC") pragmas)
                    makeList "" atTheStart $ pure $ orderDefs $ languagePragmas ++ optionsPragmas

trfLanguagePragma :: RangeAnnot a => Located String -> Trf (Ann AST.FilePragma a)
trfLanguagePragma lstr@(L l str) = annLoc (pure l) (AST.LanguagePragma <$> makeList ", " (pure $ srcSpanStart $ getLoc $ last pragmaElems) 
                                                                                         (mapM (trfLoc (pure . AST.LanguageExtension)) extensions))
  where pragmaElems = splitLocated lstr
        extensions = init $ drop 2 pragmaElems

trfOptionsPragma :: RangeAnnot a => Located String -> Trf (Ann AST.FilePragma a)
trfOptionsPragma (L l str) = annLoc (pure l) (AST.OptionsPragma <$> annCont (pure $ AST.StringNode str))

trfModulePragma :: RangeAnnot a => Maybe (Located WarningTxt) -> Trf (AnnMaybe AST.ModulePragma a)
trfModulePragma = trfMaybeDefault " " "" (trfLoc $ \case WarningTxt _ txts -> AST.ModuleWarningPragma <$> trfAnnList " " trfText' txts
                                                         DeprecatedTxt _ txts -> AST.ModuleDeprecatedPragma <$> trfAnnList " " trfText' txts) 
                                  (before AnnWhere)

trfText' :: RangeAnnot a => StringLiteral -> Trf (AST.StringNode a)
trfText' = pure . AST.StringNode . unpackFS . sl_fs



trfExportList :: TransformName n r => SrcLoc -> Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ExportSpecList r)
trfExportList loc = trfMaybeDefault " " "" (trfLoc trfExportList') (pure loc)

trfExportList' :: TransformName n r => [LIE n] -> Trf (AST.ExportSpecList r)
trfExportList' exps = AST.ExportSpecList <$> (makeList ", " (after AnnOpenP) (orderDefs . catMaybes <$> (mapM trfExport exps)))
  
trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.ExportSpec r))
trfExport = trfMaybeLoc $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleName n)
  other -> do trf <- trfIESpec' other
              fmap AST.DeclExport <$> (sequence $ fmap (annCont . return) trf)

trfImports :: TransformName n r => [LImportDecl n] -> Trf (AnnList AST.ImportDecl r)
trfImports (filter (not . ideclImplicit . unLoc) -> imps) 
  = AnnList <$> importDefaultLoc <*> mapM trfImport imps
  where importDefaultLoc = toIndentedListAnnot (if Data.List.null imps then "\n" else "") "" "\n" . srcSpanEnd 
                             <$> (combineSrcSpans <$> asks (srcLocSpan . srcSpanStart . contRange) 
                                                  <*> (srcLocSpan . srcSpanEnd <$> tokenLoc AnnWhere))
trfImport :: forall n r . TransformName n r => LImportDecl n -> Trf (Ann AST.ImportDecl r)
trfImport = (addImportData (SemanticsPhantom :: SemanticsPhantom n) <=<) $ trfLoc $ \(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding) ->
  let -- default positions of optional parts of an import declaration
      annBeforeQual = if isSrc then AnnClose else AnnImport
      annBeforeSafe = if isQual then AnnQualified else annBeforeQual
      annBeforePkg = if isSafe then AnnSafe else annBeforeSafe
      atAsPos = if isJust declHiding then before AnnOpenP else atTheEnd
  in AST.ImportDecl 
       <$> (if isSrc then makeJust <$> annLoc (tokensLoc [AnnOpen, AnnClose]) (pure AST.ImportSource)
                     else nothing " " "" (after AnnImport))
       <*> (if isQual then makeJust <$> (annLoc (tokenLoc AnnQualified) (pure AST.ImportQualified)) 
                      else nothing " " "" (after annBeforeQual))
       <*> (if isSafe then makeJust <$> (annLoc (tokenLoc AnnSafe) (pure AST.ImportSafe)) 
                      else nothing " " "" (after annBeforeSafe))
       <*> maybe (nothing " " "" (after annBeforePkg)) 
                 (\str -> makeJust <$> (annLoc (tokenLoc AnnPackageName) (pure (AST.StringNode (unpackFS $ sl_fs str))))) pkg
       <*> trfModuleName name 
       <*> maybe (nothing " " "" atAsPos) (\mn -> makeJust <$> (trfRenaming mn)) declAs
       <*> trfImportSpecs declHiding
  where trfRenaming mn
          = annLoc (tokensLoc [AnnAs,AnnVal])
                   (AST.ImportRenaming <$> (annLoc (tokenLoc AnnVal) 
                                           (trfModuleName' mn)))  
  
trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybe AST.ImportSpec r)
trfImportSpecs (Just (True, l)) 
  = makeJust <$> trfLoc (\specs -> AST.ImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs (Just (False, l)) 
  = makeJust <$> trfLoc (\specs -> AST.ImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.IESpec r)) 
trfIESpec = trfMaybeLoc trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.IESpec r))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (makeJust <$> (annLoc (tokenLoc AnnDotdot) (pure AST.SubSpecAll))))
trfIESpec' (IEThingWith n _ ls _)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (makeJust <$> between AnnOpenP AnnCloseP 
                                                  (annCont $ AST.SubSpecList <$> makeList ", " (after AnnOpenP) (mapM trfName ls))))
trfIESpec' _ = pure Nothing
  
 