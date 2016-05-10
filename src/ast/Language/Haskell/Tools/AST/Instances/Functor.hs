{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveFunctor #-}
module Language.Haskell.Tools.AST.Instances.Functor where

import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.TH
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Stmts
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
instance (Functor e) => Functor (Ann e) where
  fmap f (Ann a e) = Ann (f a) (fmap f e)

instance (Functor e) => Functor (AnnMaybe e) where
  fmap f (AnnMaybe a mb) = AnnMaybe (f a) (fmap (fmap f) mb)

instance (Functor e) => Functor (AnnList e) where
  fmap f (AnnList a ls) = AnnList (f a) (map (fmap f) ls)

-- Modules
deriving instance Functor Module
deriving instance Functor ModuleHead
deriving instance Functor ExportSpecList
deriving instance Functor ExportSpec
deriving instance Functor IESpec
deriving instance Functor SubSpec
deriving instance Functor ModulePragma
deriving instance Functor ImportDecl
deriving instance Functor ImportSpec
deriving instance Functor ImportQualified
deriving instance Functor ImportSource
deriving instance Functor ImportSafe
deriving instance Functor TypeNamespace
deriving instance Functor ImportRenaming

-- Declarations
deriving instance Functor Decl
deriving instance Functor ClassBody
deriving instance Functor GadtDeclList
deriving instance Functor ClassElement
deriving instance Functor DeclHead
deriving instance Functor InstBody
deriving instance Functor InstBodyDecl
deriving instance Functor GadtDecl
deriving instance Functor GadtField
deriving instance Functor FunDeps
deriving instance Functor FunDep
deriving instance Functor ConDecl
deriving instance Functor FieldDecl
deriving instance Functor Deriving
deriving instance Functor InstanceRule
deriving instance Functor InstanceHead
deriving instance Functor TypeEqn
deriving instance Functor KindConstraint
deriving instance Functor TyVar
deriving instance Functor Type
deriving instance Functor Kind
deriving instance Functor Context
deriving instance Functor Assertion
deriving instance Functor Expr
deriving instance Functor expr => Functor (Stmt' expr)
deriving instance Functor CompStmt
deriving instance Functor ValueBind
deriving instance Functor Pattern
deriving instance Functor PatternField
deriving instance Functor Splice
deriving instance Functor QQString
deriving instance Functor Match
deriving instance Functor expr => Functor (Alt' expr)
deriving instance Functor Rhs
deriving instance Functor GuardedRhs
deriving instance Functor FieldUpdate
deriving instance Functor Bracket
deriving instance Functor TopLevelPragma
deriving instance Functor Rule
deriving instance Functor Annotation
deriving instance Functor MinimalFormula
deriving instance Functor ExprPragma
deriving instance Functor SourceRange
deriving instance Functor Number
deriving instance Functor QuasiQuote
deriving instance Functor RhsGuard
deriving instance Functor LocalBind
deriving instance Functor LocalBinds
deriving instance Functor FixitySignature
deriving instance Functor TypeSignature
deriving instance Functor ListCompBody
deriving instance Functor TupSecElem
deriving instance Functor TypeFamily
deriving instance Functor expr => Functor (CaseRhs' expr)
deriving instance Functor expr => Functor (GuardedCaseRhs' expr)
deriving instance Functor PatternSynonym
deriving instance Functor PatSynRhs
deriving instance Functor PatSynWhere
deriving instance Functor PatternTypeSignature
deriving instance Functor Role
deriving instance Functor Cmd
deriving instance Functor CmdStmt
deriving instance Functor LanguageExtension

-- Literal
deriving instance Functor Literal
deriving instance Functor Promoted

-- Base
deriving instance Functor Operator
deriving instance Functor Name
deriving instance Functor SimpleName
deriving instance Functor UnqualName
deriving instance Functor StringNode
deriving instance Functor DataOrNewtypeKeyword
deriving instance Functor DoKind
deriving instance Functor TypeKeyword
deriving instance Functor OverlapPragma
deriving instance Functor CallConv
deriving instance Functor ArrowAppl
deriving instance Functor Safety
deriving instance Functor Assoc
deriving instance Functor Precedence
deriving instance Functor PhaseControl
deriving instance Functor PhaseNumber
deriving instance Functor PhaseInvert