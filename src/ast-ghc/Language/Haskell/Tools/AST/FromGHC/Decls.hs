{-# LANGUAGE LambdaCase 
           , ViewPatterns
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.Decls where

import GHC
import RdrName as GHC
import Class as GHC
import HsSyn as GHC
import SrcLoc as GHC
import HsDecls as GHC
import Name as GHC
import OccName as GHC
import ApiAnnotation as GHC
import FastString as GHC
import BasicTypes as GHC
import Bag as GHC
import ForeignCall as GHC
import Outputable as GHC
import Unique as GHC

import Control.Monad.Reader
import Control.Reference
import Data.Maybe
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data

import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Binds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), AnnList(..), RangeWithName, getRange)
import qualified Language.Haskell.Tools.AST as AST

import Debug.Trace

trfDecls :: TransformName n r => [LHsDecl n] -> Trf (AnnList AST.Decl r)
-- TODO: filter documentation comments
trfDecls decls = addToScope decls $ makeIndentedListNewlineBefore atTheEnd (mapM trfDecl decls)

trfDeclsGroup :: forall n r . TransformName n r => HsGroup n -> Trf (AnnList AST.Decl r)
trfDeclsGroup (HsGroup vals splices tycls insts derivs fixities defaults foreigns warns anns rules vects docs) 
  = addAllToScope $ makeIndentedListNewlineBefore atTheEnd (fmap (orderDefs . concat) $ sequence $
      [ trfBindOrSig vals
      , concat <$> mapM (mapM (trfDecl . (fmap TyClD)) . group_tyclds) tycls
      , mapM (trfDecl . (fmap SpliceD)) splices
      , mapM (trfDecl . (fmap InstD)) insts
      , mapM (trfDecl . (fmap DerivD)) derivs
      , mapM (trfDecl . (fmap (SigD . FixSig))) (mergeFixityDefs fixities)
      , mapM (trfDecl . (fmap DefD)) defaults
      , mapM (trfDecl . (fmap ForD)) foreigns
      , mapM (trfDecl . (fmap WarningD)) warns
      , mapM (trfDecl . (fmap AnnD)) anns
      , mapM (trfDecl . (fmap RuleD)) rules
      , mapM (trfDecl . (fmap VectD)) vects
      -- , mapM (trfDecl . (fmap DocD)) docs
      ])
  where trfBindOrSig :: HsValBinds n -> Trf [Ann AST.Decl r]
        trfBindOrSig (getBindsAndSigs -> (sigs, binds))
          = (++) <$> mapM (trfLoc trfVal) (bagToList binds)
                 <*> mapM (trfLoc trfSig) sigs
        addAllToScope = addToScope vals . addToCurrentScope tycls . addToCurrentScope foreigns
           
           
trfDecl :: TransformName n r => Located (HsDecl n) -> Trf (Ann AST.Decl r)
trfDecl = addDefInfo $ trfLoc $ \case
  TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars kindSig _)) 
    -> AST.ClosedTypeFamilyDecl <$> focusAfter AnnType (createDeclHead name tyVars) 
                                <*> trfFamilyKind kindSig 
                                <*> trfTypeEqs typeEqs
  TyClD (FamDecl fd) -> AST.TypeFamilyDecl <$> annCont (trfTypeFam' fd)
  TyClD (SynDecl name vars rhs _) 
    -> AST.TypeDecl <$> between AnnType AnnEqual (createDeclHead name vars) <*> trfType rhs
  TyClD (DataDecl name vars (HsDataDefn nd ctx _ kind cons derivs) _ _) 
    -> do let ctxTok = case nd of DataType -> AnnData
                                  NewType -> AnnNewtype
              consLoc = focusBeforeIfPresent AnnDeriving atTheEnd
          whereLoc <- tokenLoc AnnWhere
          if isGoodSrcSpan whereLoc then trfGADT nd name vars ctx kind cons derivs ctxTok consLoc
                                    else trfDataDef nd name vars ctx cons derivs ctxTok consLoc
  TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs docs _) 
    -> AST.ClassDecl <$> trfCtx (after AnnClass) ctx 
                     <*> betweenIfPresent AnnClass AnnWhere (createDeclHead name vars)
                     <*> trfFunDeps funDeps 
                     <*> createClassBody sigs defs typeFuns typeFunDefs
  InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))
    -> AST.InstDecl <$> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap 
                    <*> trfInstanceRule (hsib_body typ)
                    <*> trfInstBody binds sigs typefam datafam
  InstD (DataFamInstD (DataFamInstDecl con pats (HsDataDefn nd _ _ _ cons derivs) _))
    -> AST.DataInstDecl <$> trfDataKeyword nd
                        <*> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats)
                        <*> makeList " | " (after AnnEqual) (mapM trfConDecl cons)
                        <*> trfMaybe "" "" trfDerivings derivs
  InstD (TyFamInstD (TyFamInstDecl (L l (TyFamEqn con pats rhs)) _))
    -> AST.TypeInstDecl <$> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats) <*> trfType rhs
  ValD bind -> trfVal bind
  SigD sig -> trfSig sig
  DerivD (DerivDecl t overlap) -> AST.DerivDecl <$> trfMaybe "" "" trfOverlap overlap <*> trfInstanceRule (hsib_body t)
  -- TODO: INLINE, SPECIALIZE, MINIMAL, VECTORISE pragmas, Warnings, Annotations, rewrite rules, role annotations
  RuleD (HsRules _ rules) -> AST.PragmaDecl <$> annCont (AST.RulePragma <$> makeIndentedList (before AnnClose) (mapM trfRewriteRule rules))
  RoleAnnotD (RoleAnnotDecl name roles) -> AST.RoleDecl <$> trfSimpleName name <*> makeList " " atTheEnd (mapM trfRole roles)
  DefD (DefaultDecl types) -> AST.DefaultDecl . nonemptyAnnList <$> mapM trfType types
  ForD (ForeignImport name (hsib_body -> typ) _ (CImport ccall safe _ _ _)) 
    -> AST.ForeignImport <$> trfCallConv ccall <*> trfSafety (getLoc ccall) safe <*> define (trfName name) <*> trfType typ
  ForD (ForeignExport name (hsib_body -> typ) _ (CExport (L l (CExportStatic _ _ ccall)) _)) 
    -> AST.ForeignExport <$> annLoc (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
  SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.SpliceDecl <$> (annCont $ trfSplice' spl)
  AnnD (HsAnnotation stxt subject expr) 
    -> AST.PragmaDecl <$> annCont (AST.AnnPragma <$> trfAnnotationSubject stxt subject (srcSpanStart $ getLoc expr) <*> trfExpr expr)
  d -> error ("Illegal declaration: " ++ showSDocUnsafe (ppr d) ++ " (ctor: " ++ show (toConstr d) ++ ")")

addDefInfo :: forall n r . TransformName n r => (Located (HsDecl n) -> Trf (Ann AST.Decl r)) -> Located (HsDecl n) -> Trf (Ann AST.Decl r)
addDefInfo f d 
  = (AST.annotation .- addSemanticInfo (AST.DefinitionInfo (listToMaybe $ hsGetNames d) :: AST.SemanticInfo n))
      <$> (do tvs <- lift $ getTypeVariablesOf d
              addTypeVars tvs (f d))

trfGADT nd name vars ctx kind cons derivs ctxTok consLoc
  = AST.GDataDecl <$> trfDataKeyword nd
                  <*> trfCtx (after ctxTok) ctx
                  <*> betweenIfPresent ctxTok AnnEqual (createDeclHead name vars)
                  <*> trfKindSig kind
                  <*> makeIndentedListBefore " where " consLoc (mapM trfGADTConDecl cons)
                  <*> trfMaybe "" "" trfDerivings derivs

trfDataDef nd name vars ctx cons derivs ctxTok consLoc
  = AST.DataDecl <$> trfDataKeyword nd
                 <*> trfCtx (after ctxTok) ctx
                 <*> betweenIfPresent ctxTok AnnEqual (createDeclHead name vars)
                 <*> makeListBefore "=" " | " consLoc (mapM trfConDecl cons)
                 <*> trfMaybe "" "" trfDerivings derivs

trfVal :: TransformName n r => HsBindLR n n -> Trf (AST.Decl r)
trfVal (PatSynBind psb) = AST.PatternSynonymDecl <$> annCont (trfPatternSynonym psb)
trfVal bind = do tvs <- lift $ getTypeVariablesOf bind
                 addTypeVars tvs (AST.ValueBinding <$> (annCont $ trfBind' bind))
                

trfSig :: TransformName n r => Sig n -> Trf (AST.Decl r)
trfSig (ts @ (TypeSig n _)) = do tvs <- lift $ getTypeVariablesOf ts
                                 addTypeVars tvs (AST.TypeSigDecl <$> (annCont $ trfTypeSig' ts))
trfSig (FixSig fs) = AST.FixityDecl <$> (annCont $ trfFixitySig fs)
trfSig (PatSynSig id typ) 
  = AST.PatTypeSigDecl <$> annCont (AST.PatternTypeSignature <$> trfName id <*> trfType (hsib_body typ))
trfSig (InlineSig name (InlinePragma _ Inlinable _ phase _)) 
  = AST.PragmaDecl <$> annCont (AST.InlinablePragma <$> trfPhase (pure $ srcSpanStart $ getLoc name) phase <*> trfName name)
trfSig (InlineSig name (InlinePragma src inl _ phase cl)) 
  = do rng <- asks contRange
       let parts = map getLoc $ splitLocated (L rng src)
       AST.PragmaDecl <$> annCont ((case inl of Inline -> AST.InlinePragma; NoInline -> AST.NoInlinePragma) 
                                     <$> trfConlike parts cl 
                                     <*> trfPhase (pure $ srcSpanStart (getLoc name)) phase 
                                     <*> trfName name)
trfSig (SpecSig name (map hsib_body -> types) (inl_act -> phase)) 
  = AST.PragmaDecl <$> annCont (AST.SpecializePragma <$> trfPhase (pure $ srcSpanStart (getLoc name)) phase 
                                                     <*> trfName name 
                                                     <*> (orderAnnList <$> trfAnnList ", " trfType' types))
trfSig s = error ("Illegal signature: " ++ showSDocUnsafe (ppr s) ++ " (ctor: " ++ show (toConstr s) ++ ")")

trfConlike :: RangeAnnot a => [SrcSpan] -> RuleMatchInfo -> Trf (AnnMaybe AST.ConlikeAnnot a)
trfConlike parts ConLike = makeJust <$> annLoc (pure $ parts !! 2) (pure AST.ConlikeAnnot)
trfConlike parts FunLike = nothing " " "" (pure $ srcSpanEnd $ parts !! 1)

trfConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.ConDecl r)
trfConDecl = trfLoc trfConDecl'

trfConDecl' :: TransformName n r => ConDecl n -> Trf (AST.ConDecl r)
trfConDecl' (ConDeclH98 { con_name = name, con_details = PrefixCon args })
  = AST.ConDecl <$> define (trfName name) <*> makeList " " atTheEnd (mapM trfType args)
trfConDecl' (ConDeclH98 { con_name = name, con_details = RecCon (unLoc -> flds) })
  = AST.RecordDecl <$> define (trfName name) <*> (between AnnOpenC AnnCloseC $ trfAnnList ", " trfFieldDecl' flds)
trfConDecl' (ConDeclH98 { con_name = name, con_details = InfixCon t1 t2 })
  = AST.InfixConDecl <$> trfType t1 <*> define (trfName name) <*> trfType t2

trfGADTConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.GadtConDecl r)
trfGADTConDecl = trfLoc $ \(ConDeclGADT { con_names = names, con_type = hsib_body -> typ })
  -> AST.GadtConDecl <$> define (trfAnnList ", " trfName' names) 
                     <*> trfGadtConType typ

trfGadtConType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.GadtConType r)
trfGadtConType = trfLoc $ \case 
  HsFunTy (cleanHsType . unLoc -> HsRecTy flds) resType 
    -> AST.GadtRecordType <$> between AnnOpenC AnnCloseC (trfAnnList ", " trfFieldDecl' flds) 
                          <*> trfType resType
  typ -> AST.GadtNormalType <$> annCont (trfType' typ)

trfFieldDecl :: TransformName n r => Located (ConDeclField n) -> Trf (Ann AST.FieldDecl r)
trfFieldDecl = trfLoc trfFieldDecl'

trfFieldDecl' :: TransformName n r => ConDeclField n -> Trf (AST.FieldDecl r)
trfFieldDecl' (ConDeclField names typ _) = AST.FieldDecl <$> (define $ nonemptyAnnList <$> mapM (trfName . getFieldOccName) names) <*> trfType typ

trfDerivings :: TransformName n r => Located [LHsSigType n] -> Trf (Ann AST.Deriving r)
trfDerivings = trfLoc $ \case
  [hsib_body -> typ@(unLoc -> HsTyVar cls)] -> AST.DerivingOne <$> trfInstanceHead typ
  derivs -> AST.Derivings <$> trfAnnList ", " trfInstanceHead' (map hsib_body derivs)
  
trfInstanceRule :: TransformName n r => Located (HsType n) -> Trf (Ann AST.InstanceRule r)
trfInstanceRule = trfLoc (trfInstanceRule' . cleanHsType)

trfInstanceRule' :: TransformName n r => HsType n -> Trf (AST.InstanceRule r)
trfInstanceRule' (HsForAllTy bndrs (unLoc -> HsQualTy ctx typ))
  = AST.InstanceRule <$> (makeJust <$> annLoc (pure $ collectLocs bndrs) (trfBindings bndrs)) 
                     <*> trfCtx (after AnnDot) ctx
                     <*> trfInstanceHead typ
trfInstanceRule' (HsQualTy ctx typ) = AST.InstanceRule <$> nothing "" " . " atTheStart 
                                                       <*> trfCtx atTheStart ctx
                                                       <*> trfInstanceHead typ
trfInstanceRule' (HsParTy typ) = AST.InstanceParen <$> trfInstanceRule typ
trfInstanceRule' (HsTyVar tv) = instanceHead $ annCont (AST.InstanceHeadCon <$> trfName tv)
trfInstanceRule' (HsAppTy t1 t2) = instanceHead $ annCont (AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2)
trfInstanceRule' t = error (showSDocUnsafe $ ppr t)

instanceHead :: RangeAnnot r => Trf (Ann AST.InstanceHead r) -> Trf (AST.InstanceRule r)
instanceHead hd = AST.InstanceRule <$> (nothing "" " . " atTheStart) <*> (nothing " " "" atTheStart) <*> hd
                            
makeInstanceRuleTyVars :: TransformName n r => Located n -> HsImplicitBndrs n [LHsType n] -> Trf (Ann AST.InstanceRule r)
makeInstanceRuleTyVars n vars = annCont
  $ AST.InstanceRule <$> nothing "" " . " atTheStart
                     <*> nothing " " "" atTheStart
                     <*> foldl (\c t -> annLoc (pure $ combineSrcSpans (getLoc n) (getLoc t)) $ AST.InstanceHeadApp <$> c <*> (trfType t))
                               (copyAnnot AST.InstanceHeadCon (trfName n))
                               (hsib_body vars)

trfInstanceHead :: TransformName n r => Located (HsType n) -> Trf (Ann AST.InstanceHead r)
trfInstanceHead = trfLoc trfInstanceHead'

trfInstanceHead' :: TransformName n r => HsType n -> Trf (AST.InstanceHead r)
trfInstanceHead' = trfInstanceHead'' . cleanHsType where
  trfInstanceHead'' (HsForAllTy [] (unLoc -> t)) = trfInstanceHead' t
  trfInstanceHead'' (HsTyVar tv) = AST.InstanceHeadCon <$> trfName tv
  trfInstanceHead'' (HsAppTy t1 t2) = AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
  trfInstanceHead'' (HsParTy typ) = AST.InstanceHeadParen <$> trfInstanceHead typ
  trfInstanceHead'' (HsOpTy t1 op t2) 
    = AST.InstanceHeadApp <$> (annLoc (pure $ combineSrcSpans (getLoc t1) (getLoc op))
                                      (AST.InstanceHeadInfix <$> trfType t1 <*> trfName op)) 
                          <*> trfType t2
  trfInstanceHead'' t = error ("Illegal instance head: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
 
trfTypeEqs :: TransformName n r => Maybe [Located (TyFamInstEqn n)] -> Trf (AnnList AST.TypeEqn r)
trfTypeEqs Nothing = makeList "\n" (after AnnWhere) (pure [])
trfTypeEqs (Just eqs) = makeNonemptyList "\n" (sequence (zipWith trfTypeEq [0..] eqs))

trfTypeEq :: TransformName n r => Int -> Located (TyFamInstEqn n) -> Trf (Ann AST.TypeEqn r)
trfTypeEq i = trfLoc $ \(TyFamEqn name pats rhs) 
  -> do tyVars <- case hsGetNames name of [n] -> lift (lookupName n) >>= \case Just (ATyCon tc) -> trace ("\n+++" ++ showSDocUnsafe (ppr tc)) $ return $ typeVarsInEqs tc !! i
                                                                               _                -> return []
                                          _   -> return []
        addTypeVars tyVars (AST.TypeEqn <$> define (focusBefore AnnEqual (combineTypes name (hsib_body pats))) <*> trfType rhs)
  where combineTypes :: TransformName n r => Located n -> [LHsType n] -> Trf (Ann AST.Type r)
        combineTypes name (lhs : rhs : rest) | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
          = annCont $ AST.TyInfix <$> trfType lhs <*> trfOperator name <*> trfType rhs
        combineTypes name pats = wrapTypes (annLoc (pure $ getLoc name) (AST.TyVar <$> trfName name)) pats

        wrapTypes :: TransformName n r => Trf (Ann AST.Type r) -> [LHsType n] -> Trf (Ann AST.Type r)
        wrapTypes base pats 
          = foldl (\t p -> do typ <- t
                              annLoc (pure $ combineSrcSpans (getRange $ _annotation typ) (getLoc p)) 
                                     (AST.TyApp <$> pure typ <*> trfType p)) base pats
                 
trfFunDeps :: TransformName n r => [Located (FunDep (Located n))] -> Trf (AnnMaybe AST.FunDeps r)
trfFunDeps [] = nothing "| " "" $ focusBeforeIfPresent AnnWhere atTheEnd
trfFunDeps fundeps = makeJust <$> annLoc (combineSrcSpans (collectLocs fundeps) <$> tokenLoc AnnVbar) 
                                         (AST.FunDeps <$> trfAnnList ", " trfFunDep' fundeps)
  
trfFunDep' :: TransformName n r => FunDep (Located n) -> Trf (AST.FunDep r)
trfFunDep' (lhs, rhs) = AST.FunDep <$> trfAnnList ", " trfName' lhs <*> trfAnnList ", " trfName' rhs

createDeclHead :: TransformName n r => Located n -> LHsQTyVars n -> Trf (Ann AST.DeclHead r)
createDeclHead name (hsq_explicit -> lhs : rhs : rest)
  | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
  -- infix declaration
  = wrapDeclHead rest
      $ annLoc (addParenLocs $ getLoc lhs `combineSrcSpans` getLoc rhs) 
               (AST.DHInfix <$> define (trfTyVar lhs) <*> trfOperator name <*> define (trfTyVar rhs))
createDeclHead name vars = wrapDeclHead (hsq_explicit vars) (define $ copyAnnot AST.DeclHead (trfName name))

wrapDeclHead :: TransformName n r => [LHsTyVarBndr n] -> Trf (Ann AST.DeclHead r) -> Trf (Ann AST.DeclHead r)
wrapDeclHead vars base
  = foldl (\t p -> do typ <- t 
                      annLoc (addParenLocs $ combineSrcSpans (getRange $ _annotation typ) (getLoc p)) 
                             (AST.DHApp typ <$> define (trfTyVar p))
          ) base vars

-- | Get the parentheses directly before and after (for parenthesized application)
addParenLocs :: SrcSpan -> Trf SrcSpan
addParenLocs sp 
  = let possibleSpan = mkSrcSpan (updateCol (subtract 1) (srcSpanStart sp)) (updateCol (+1) (srcSpanEnd sp))
     in local (\s -> s { contRange = possibleSpan })
              (combineSrcSpans <$> (combineSrcSpans sp <$> tokenLoc AnnOpenP) <*> tokenLocBack AnnCloseP)
      
         
createClassBody :: TransformName n r => [LSig n] -> LHsBinds n -> [LFamilyDecl n] 
                               -> [LTyFamDefltEqn n] -> Trf (AnnMaybe AST.ClassBody r)
createClassBody sigs binds typeFams typeFamDefs 
  = do isThereWhere <- isGoodSrcSpan <$> (tokenLoc AnnWhere)
       if isThereWhere 
         then makeJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                                  (AST.ClassBody <$> makeList "" (after AnnWhere) 
                                                                 (orderDefs . concat <$> sequenceA allDefs))
         else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc typeFams ++ map getLoc typeFamDefs
        allDefs = [getSigs, getBinds, getFams, getFamDefs]
        getSigs = mapM trfClassElemSig sigs
        getBinds = mapM (copyAnnot AST.ClsDef . trfBind) (bagToList binds)
        getFams = mapM (copyAnnot AST.ClsTypeFam . trfTypeFam) typeFams
        getFamDefs = mapM trfTypeFamDef typeFamDefs
       
trfClassElemSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.ClassElement r)
trfClassElemSig = trfLoc $ \case
  TypeSig names typ -> AST.ClsSig <$> (annCont $ AST.TypeSignature <$> define (makeNonemptyList ", " (mapM trfName names)) 
                                  <*> trfType (hswc_body $ hsib_body typ))
  ClassOpSig True [name] typ -> AST.ClsDefSig <$> trfName name <*> trfType (hsib_body typ)
  ClassOpSig False names typ -> AST.ClsSig <$> (annCont $ AST.TypeSignature <$> define (makeNonemptyList ", " (mapM trfName names)) 
                                           <*> trfType (hsib_body typ))
  MinimalSig _ formula -> AST.ClsMinimal <$> trfMinimalFormula formula
  s -> error ("Illegal signature: " ++ showSDocUnsafe (ppr s) ++ " (ctor: " ++ show (toConstr s) ++ ")")
         
trfTypeFam :: TransformName n r => Located (FamilyDecl n) -> Trf (Ann AST.TypeFamily r)
trfTypeFam = trfLoc trfTypeFam'

trfTypeFam' :: TransformName n r => FamilyDecl n -> Trf (AST.TypeFamily r)
trfTypeFam' (FamilyDecl DataFamily name tyVars kindSig _)
  = AST.DataFamily <$> (case unLoc kindSig of KindSig _ -> between AnnData AnnDcolon; _ -> id) (createDeclHead name tyVars) 
                   <*> trfFamilyKind kindSig
trfTypeFam' (FamilyDecl OpenTypeFamily name tyVars kindSig injectivity)
  = AST.TypeFamily <$> (case unLoc kindSig of KindSig _ -> between AnnType AnnDcolon; _ -> id) (createDeclHead name tyVars) 
                   <*> trfFamilyResultSig kindSig injectivity

trfTypeFamDef :: TransformName n r => Located (TyFamDefltEqn n) -> Trf (Ann AST.ClassElement r)
trfTypeFamDef = trfLoc $ \(TyFamEqn con pats rhs) 
  -> AST.ClsTypeDef <$> between AnnType AnnEqual (createDeclHead con pats) <*> trfType rhs
          
trfInstBody :: TransformName n r => LHsBinds n -> [LSig n] -> [LTyFamInstDecl n] -> [LDataFamInstDecl n] -> Trf (AnnMaybe AST.InstBody r)
trfInstBody binds sigs fams dats = do
    wh <- tokenLoc AnnWhere
    if isGoodSrcSpan wh then
      makeJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                          (AST.InstBody <$> (makeList "" (after AnnWhere) 
                                                         (orderDefs . concat <$> sequenceA allDefs)))
    else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc fams ++ map getLoc dats
        allDefs = [getSigs, getBinds, getFams, getDats]
        getSigs = mapM trfClassInstSig sigs
        getBinds = mapM (copyAnnot AST.InstBodyNormalDecl . trfBind) (bagToList binds)
        getFams = mapM trfInstTypeFam fams
        getDats = mapM trfInstDataFam dats
          
trfClassInstSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.InstBodyDecl r)
trfClassInstSig = trfLoc $ \case
  TypeSig names typ -> AST.InstBodyTypeSig <$> (annCont $ AST.TypeSignature <$> makeNonemptyList ", " (mapM trfName names) 
                                           <*> trfType (hswc_body $ hsib_body typ))
  ClassOpSig _ names typ -> AST.InstBodyTypeSig <$> (annCont $ AST.TypeSignature <$> define (makeNonemptyList ", " (mapM trfName names)) 
                                                <*> trfType (hsib_body typ))
  SpecInstSig _ typ -> AST.SpecializeInstance <$> trfType (hsib_body typ)
  s -> error ("Illegal class instance signature: " ++ showSDocUnsafe (ppr s) ++ " (ctor: " ++ show (toConstr s) ++ ")")
          
trfInstTypeFam :: TransformName n r => Located (TyFamInstDecl n) -> Trf (Ann AST.InstBodyDecl r)
trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = copyAnnot AST.InstBodyTypeDecl (trfTypeEq 0 eqn)

trfInstDataFam :: TransformName n r => Located (DataFamInstDecl n) -> Trf (Ann AST.InstBodyDecl r)
trfInstDataFam = trfLoc $ \case 
  (DataFamInstDecl tc (hsib_body -> pats) (HsDataDefn dn ctx _ _ cons derivs) _) 
    -> AST.InstBodyDataDecl 
         <$> trfDataKeyword dn 
         <*> annLoc (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
                    (AST.InstanceRule <$> nothing "" " . " atTheStart
                                      <*> trfCtx atTheStart ctx 
                                      <*> foldr (\t r -> annLoc (combineSrcSpans (getLoc t) . getRange . _annotation <$> r) 
                                                                (AST.InstanceHeadApp <$> r <*> (trfType t))) 
                                                (copyAnnot AST.InstanceHeadCon (trfName tc)) pats)
         <*> trfAnnList "" trfConDecl' cons
         <*> trfMaybe " deriving " "" trfDerivings derivs
          
trfPatternSynonym :: forall n r . TransformName n r => PatSynBind n n -> Trf (AST.PatternSynonym r)
trfPatternSynonym (PSB id _ lhs def dir)
  = let sep = case dir of ImplicitBidirectional -> AnnEqual
                          _                     -> AnnLarrow
        rhsLoc = combineSrcSpans (getLoc def) <$> tokenLoc sep
        -- we use the selector name instead of the pattern variable name
        rewrites = case lhs of RecordPatSyn flds -> map (\r -> (unLoc (recordPatSynPatVar r), unLoc (recordPatSynSelectorId r))) flds
                               _                 -> []
        changedRhs = biplateRef .- (\n -> case lookup n rewrites of Just x -> x; Nothing -> n) $ def
     in AST.PatternSynonym <$> trfPatSynLhs id lhs
                           <*> annLoc rhsLoc (trfPatSynRhs dir changedRhs)

  where trfPatSynLhs :: TransformName n r => Located n -> HsPatSynDetails (Located n) -> Trf (Ann AST.PatSynLhs r)
        trfPatSynLhs id (PrefixPatSyn args)
          = annLoc (pure $ foldLocs (getLoc id : map getLoc args)) $ AST.NormalPatSyn <$> trfName id <*> trfAnnList " " trfName' args
        trfPatSynLhs op (InfixPatSyn lhs rhs)
          = annLoc (pure $ getLoc lhs `combineSrcSpans` getLoc rhs) $ AST.InfixPatSyn <$> trfName lhs <*> trfOperator op <*> trfName rhs
        trfPatSynLhs id (RecordPatSyn flds)
          = annLoc (mkSrcSpan (srcSpanStart (getLoc id)) <$> before AnnEqual) 
              $ AST.RecordPatSyn <$> trfName id <*> trfAnnList ", " trfName' (map recordPatSynSelectorId flds)

        trfPatSynRhs :: TransformName n r => HsPatSynDir n -> Located (Pat n) -> Trf (AST.PatSynRhs r)
        trfPatSynRhs ImplicitBidirectional pat = AST.BidirectionalPatSyn <$> trfPattern pat <*> nothing " where " "" atTheEnd
        trfPatSynRhs (ExplicitBidirectional mg) pat = AST.BidirectionalPatSyn <$> trfPattern pat <*> (makeJust <$> trfPatSynWhere mg)
        trfPatSynRhs Unidirectional pat = AST.OneDirectionalPatSyn <$> trfPattern pat
        trfPatSynWhere :: TransformName n r => MatchGroup n (LHsExpr n) -> Trf (Ann AST.PatSynWhere r)
        trfPatSynWhere (MG { mg_alts = alts }) = annLoc (pure $ getLoc alts) (AST.PatSynWhere <$> makeIndentedList (after AnnWhere) (mapM (trfMatch (unLoc id)) (unLoc alts)))

trfFamilyKind :: TransformName n r => Located (FamilyResultSig n) -> Trf (AnnMaybe AST.KindConstraint r)
trfFamilyKind (unLoc -> fr) = case fr of
  NoSig -> nothing "" " " atTheEnd
  KindSig k -> trfKindSig (Just k)

trfFamilyResultSig :: TransformName n r => Located (FamilyResultSig n) -> Maybe (LInjectivityAnn n) -> Trf (AnnMaybe AST.TypeFamilySpec r)
trfFamilyResultSig (L l fr) Nothing = case fr of 
  NoSig -> nothing "" " " atTheEnd
  KindSig k -> makeJust <$> (annLoc (pure l) $ AST.TypeFamilyKind <$> trfKindSig' k)
trfFamilyResultSig _ (Just (L l (InjectivityAnn n deps))) 
  = makeJust <$> (annLoc (pure l) $ AST.TypeFamilyInjectivity <$> (annCont $ AST.InjectivityAnn <$> trfName n <*> trfAnnList ", " trfName' deps))

trfAnnotationSubject :: TransformName n r => SourceText -> AnnProvenance n -> SrcLoc -> Trf (Ann AST.AnnotationSubject r)
trfAnnotationSubject stxt subject payloadEnd
  = do payloadStart <- advanceStr stxt <$> atTheStart
       case subject of ValueAnnProvenance name@(L l _) -> annLoc (pure l) (AST.NameAnnotation <$> trfName name)
                       TypeAnnProvenance name@(L l _) -> annLoc (pure $ mkSrcSpan payloadStart (srcSpanEnd l)) 
                                                                (AST.TypeAnnotation <$> trfName name)
                       ModuleAnnProvenance -> annLoc (pure $ mkSrcSpan payloadStart payloadEnd) (pure AST.ModuleAnnotation)
