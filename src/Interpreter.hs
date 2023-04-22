module Interpreter where
import Data.Map as Map
import Data.String
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import qualified AbsSoya as Gram

type Loc = Int
data Env = Env { env :: Map String Loc, funs :: Map String Gram.FnDef, types :: Map String Gram.Type }
type Store = Map Loc Val

type InterpretMonad a = ExceptT String (ReaderT Env (StateT Store IO)) a

addLocToEnv :: String -> Loc -> Env -> Env
addLocToEnv var loc e = e { env = Map.insert var loc (env e) }

newloc :: InterpretMonad Loc
newloc = do
  store <- lift get
  let loc = Map.size store + 1
  return loc

data Val
  = VInt Integer
 | VBool Bool
 | VStr String
 | VTuple [Val]
 | VNone -- może wystarczy jeden None jak zapiszę typ w Env
 deriving (Show, Eq)

-- Interpret expressions
eval :: Gram.Expr -> InterpretMonad Val

eval (Gram.ELitInt _ integer) = return (VInt integer)
eval (Gram.ELitTrue _) = return (VBool True)
eval (Gram.ELitFalse _) = return (VBool False)
eval (Gram.EString _ string) = return (VStr string)

eval (Gram.EAdd _ e1 op e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (VInt ev1, VInt ev2) -> 
      case op of
        Gram.Plus _ -> return $ VInt (ev1 + ev2)
        Gram.Minus _ -> return $ VInt (ev1 - ev2)
    (VStr es1, VStr es2) -> 
      case op of
        Gram.Plus _ -> return $ VStr (es1 ++ es2) -- + sign on str is concatenation
        Gram.Minus _ -> return $ VStr (Prelude.filter (`notElem` es2) es1) -- minus sign on strings = filter one strings with letters from second

eval (Gram.EMul _ e1 op e2) =
  case op of
    Gram.Times _ -> do
      (VInt ev1) <- eval e1
      (VInt ev2) <- eval e2
      return $ VInt (ev1 * ev2)
    Gram.Div _ -> do
      (VInt ev1) <- eval e1
      (VInt ev2) <- eval e2
      return $ VInt (ev1 `div` ev2)
    Gram.Mod _ -> do
      (VInt ev1) <- eval e1
      (VInt ev2) <- eval e2
      return $ VInt (ev1 `mod` ev2)
  
eval (Gram.EVar _ (Gram.Ident id)) = do
  -- TODO: do it better
  maybeloc <- asks (Map.lookup id . env)
  store <- lift get
  case maybeloc of
    Just loc -> case Map.lookup loc store of
      Just val -> return val
      Nothing -> throwError $ "Variable " ++ id ++ " is uninitialized."
    Nothing -> throwError $ "Variable " ++ id ++ " is not in scope."

--- Execute Statement
execStmt :: [Gram.Stmt] -> InterpretMonad ()
execStmt [] = return ()
execStmt ((Gram.Empty _):r) = execStmt r

execStmt ((Gram.Print _ e):r) = do
  result <- eval e
  showVal result
  execStmt r

execStmt ((Gram.AssStmt _ target source):r) = do
  case target of
    (Gram.TargetId _ (Gram.Ident var)) -> do -- when target is an Identifier
      case source of
        (Gram.SourceType _ typ) -> -- when source is type, e.g "int, bool"
          let val = VNone in do
            loc <- newloc
            modify (Map.insert loc val)
            local (addLocToEnv var loc) (execStmt r)
        (Gram.SourceExpr _ expr) -> do -- when source is expression, e.g "3+3"
          loc <- newloc
          val <- eval expr
          modify (Map.insert loc val)
          local (addLocToEnv var loc) (execStmt r)


showVal :: Val -> InterpretMonad ()
showVal (VInt x) = do
  liftIO $ putStrLn (show x)
  return ()

showVal (VBool b) = do
  liftIO $ putStrLn (show b)
  return ()

showVal (VStr s) = do
  liftIO $ putStrLn s
  return ()

showVal (VNone) = do
  liftIO $ putStrLn "None"
  return ()

-- - Execute Program
execProgram :: Gram.Program -> InterpretMonad ()
execProgram (Gram.Prog pos stmts) = execStmt stmts

--- Run program
runInterpreter :: Gram.Program -> IO ()
runInterpreter prog =
    let (env, st) = (Env { env = fromList [], funs = fromList [], types = fromList [] },
                     fromList [])
    in do
      (res, a) <- runStateT (runReaderT (runExceptT (execProgram prog)) env) st;
      case res of
        Left e -> putStrLn $ "runtime error: " ++ e
        Right _ -> return ()
      