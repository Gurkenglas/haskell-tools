{-# LANGUAGE StandaloneDeriving
           , DeriveGeneric
           , LambdaCase
           , ScopedTypeVariables
           , BangPatterns
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor where

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.PlaceComments
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.RangeDebug
import Language.Haskell.Tools.Refactor.RangeDebug.Instances
import Language.Haskell.Tools.Refactor.ASTDebug
import Language.Haskell.Tools.Refactor.ASTDebug.Instances

import GHC hiding (loadModule)
import Panic (handleGhcException)
import Outputable
import BasicTypes
import Bag
import Var
import SrcLoc
import Module
import FastString
import HscTypes
import GHC.Paths ( libdir )
 
import Data.List
import Data.List.Split
import GHC.Generics hiding (moduleName)
import Data.StructuralTraversal
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Time.Clock
import Data.IORef
import Data.Either.Combinators
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.Refactor.OrganizeImports
import Language.Haskell.Tools.Refactor.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.GenerateExports
import Language.Haskell.Tools.Refactor.RenameDefinition
import Language.Haskell.Tools.Refactor.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase

import Language.Haskell.TH.LanguageExtensions
 
import DynFlags
import StringBuffer            

import Debug.Trace
    

type TemplateWithNames = NodeInfo (SemanticInfo GHC.Name) SourceTemplate
type TemplateWithTypes = NodeInfo (SemanticInfo GHC.Id) SourceTemplate

data RefactorCommand = NoRefactor 
                     | OrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
    deriving Show

performCommand :: RefactorCommand -> Ann AST.Module TemplateWithTypes -> Ghc (Either String (Ann AST.Module TemplateWithTypes))
performCommand rf mod = runRefactor mod $ selectCommand rf
  where selectCommand  NoRefactor = return
        selectCommand OrganizeImports = organizeImports
        selectCommand GenerateExports = generateExports 
        selectCommand (GenerateSignature sp) = generateTypeSignature' sp
        selectCommand (RenameDefinition sp str) = renameDefinition' sp str
        selectCommand (ExtractBinding sp str) = extractBinding' sp str

readCommand :: String -> String -> RefactorCommand
readCommand fileName s = case splitOn " " s of 
  [""] -> NoRefactor
  ("OrganizeImports":_) -> OrganizeImports
  ("GenerateExports":_) -> GenerateExports
  ["GenerateSignature", sp] -> GenerateSignature (readSrcSpan fileName sp)
  ["RenameDefinition", sp, name] -> RenameDefinition (readSrcSpan fileName sp) name
  ["ExtractBinding", sp, name] -> ExtractBinding (readSrcSpan fileName sp) name
  
readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)

onlineRefactor :: String -> FilePath -> String -> IO (Either String String)
onlineRefactor command workingDir moduleStr
  = do withBinaryFile fileName WriteMode (`hPutStr` moduleStr)
       modOpts <- runGhc (Just libdir) $ ms_hspp_opts <$> loadModule workingDir moduleName
       if | xopt Cpp modOpts -> return (Left "The use of C preprocessor is not supported, please turn off Cpp extension")
          | xopt TemplateHaskell modOpts -> return (Left "The use of Template Haskell is not supported yet, please turn off TemplateHaskell extension")
          | xopt EmptyCase modOpts -> return (Left "The ranges in the AST are not correct for empty cases, therefore the EmptyCase extension is disabled")
          | otherwise -> do 
              res <- performRefactor command workingDir moduleName
              removeFile fileName
              return res
  where moduleName = "Test"
        fileName = workingDir </> (moduleName ++ ".hs")

onlineASTView :: FilePath -> String -> IO (Either String String)
onlineASTView workingDir moduleStr
  = do withBinaryFile fileName WriteMode (`hPutStr` moduleStr)
       modOpts <- runGhc (Just libdir) $ ms_hspp_opts <$> loadModule workingDir moduleName
       if | xopt Cpp modOpts -> return (Left "The use of C preprocessor is not supported, please turn off Cpp extension")
          | xopt TemplateHaskell modOpts -> return (Left "The use of Template Haskell is not supported yet, please turn off TemplateHaskell extension")
          | xopt EmptyCase modOpts -> return (Left "The ranges in the AST are not correct for empty cases, therefore the EmptyCase extension is disabled")
          | otherwise -> do 
              res <- astView workingDir moduleName
              removeFile fileName
              return (Right res)
  where moduleName = "Test"
        fileName = workingDir </> (moduleName ++ ".hs")


performRefactor :: String -> String -> String -> IO (Either String String)
performRefactor command workingDir target = 
  runGhc (Just libdir) $
    (mapRight prettyPrint <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact = performCommand (readCommand (workingDir </> (map (\case '.' -> '\\'; c -> c) target ++ ".hs")) command)

astView :: String -> String -> IO String
astView workingDir target = 
  runGhc (Just libdir) $
    (astDebug <$> (parseTyped =<< loadModule workingDir target))

loadModule :: String -> String -> Ghc ModSummary
loadModule workingDir moduleName 
  = do dflags <- getSessionDynFlags
       -- don't generate any code
       setSessionDynFlags 
         $ flip gopt_set Opt_KeepRawTokenStream
         $ flip gopt_set Opt_NoHsMain
         $ dflags { importPaths = [workingDir]
                  , hscTarget = HscInterpreted
                  , ghcLink = LinkInMemory
                  , ghcMode = CompManager 
                  }
       target <- guessTarget moduleName Nothing
       setTargets [target]
       load LoadAllTargets
       getModSummary $ mkModuleName moduleName
    
parseTyped :: ModSummary -> Ghc (Ann AST.Module TemplateWithTypes)
parseTyped modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (getNormalComments $ snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
                   runTrf (fst annots) (getPragmaComments $ snd annots)
                     $ trfModuleRename (ms_mod $ modSum) parseTrf
                         (fromJust $ tm_renamed_source tc) 
                         (pm_parsed_source p)))

parseRenamed :: ModSummary -> Ghc (Ann AST.Module TemplateWithNames)
parseRenamed modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (getNormalComments $ snd annots) 
    <$> (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
            runTrf (fst annots) (getPragmaComments $ snd annots)
              $ trfModuleRename (ms_mod $ modSum) parseTrf
                  (fromJust $ tm_renamed_source tc) 
                  (pm_parsed_source p))
    
-- | Should be only used for testing
demoRefactor :: String -> String -> String -> IO ()
demoRefactor command workingDir moduleName = 
  runGhc (Just libdir) $ do
    modSum <- loadModule workingDir moduleName
    p <- parseModule modSum
    t <- typecheckModule p
        
    let r = tm_renamed_source t
    let annots = pm_annotations $ tm_parsed_module t

    -- liftIO $ putStrLn $ show annots
    -- liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn $ show (typecheckedSource t)
    liftIO $ putStrLn "=========== parsed:"
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    liftIO $ putStrLn $ rangeDebug parseTrf
    liftIO $ putStrLn "=========== typed:"
    transformed <- addTypeInfos (typecheckedSource t) =<< (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModuleRename (ms_mod $ modSum) parseTrf (fromJust $ tm_renamed_source t) (pm_parsed_source p))
    liftIO $ putStrLn $ rangeDebug transformed
    liftIO $ putStrLn "=========== ranges fixed:"
    let commented = fixRanges $ placeComments (getNormalComments $ snd annots) transformed
    liftIO $ putStrLn $ rangeDebug commented
    liftIO $ putStrLn "=========== cut up:"
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ templateDebug cutUp
    liftIO $ putStrLn "=========== sourced:"
    let sourced = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) cutUp
    liftIO $ putStrLn $ sourceTemplateDebug sourced
    liftIO $ putStrLn "=========== pretty printed:"
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    transformed <- performCommand (readCommand (fromJust $ ml_hs_file $ ms_location modSum) command) sourced
    case transformed of 
      Right correctlyTransformed -> do
        liftIO $ putStrLn "=========== transformed AST:"
        liftIO $ putStrLn $ sourceTemplateDebug correctlyTransformed
        liftIO $ putStrLn "=========== transformed & prettyprinted:"
        let prettyPrinted = prettyPrint correctlyTransformed
        liftIO $ putStrLn prettyPrinted
        liftIO $ putStrLn "==========="
      Left transformProblem -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn transformProblem
        liftIO $ putStrLn "==========="
    
printSemaInfo :: (StructuralTraversable node, Outputable n) => node (NodeInfo (SemanticInfo n) src) -> Ghc (node (NodeInfo (SemanticInfo n) src))
printSemaInfo = traverseUp (return ()) (return ()) (\n@(NodeInfo s _) -> liftIO (print s) >> return n)
      
deriving instance Generic SrcSpan
deriving instance (Generic sema, Generic src) => Generic (NodeInfo sema src)
deriving instance Generic RangeTemplate
deriving instance Generic (SemanticInfo n)
deriving instance Generic SourceTemplate
deriving instance Generic SpanInfo
