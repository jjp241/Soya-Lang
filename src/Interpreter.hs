module Interpreter where
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.String
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import qualified AbsSoya as Gram

type Loc = Int
data Type = Int | Str | Bool | None | Tuple [Type] | List Type | Fun Type [Type] deriving (Eq, Show)
data Function = Function { funName :: String, funArgs :: [Gram.Arg], funRetType :: Type, funBody :: Gram.Block, declEnv :: Env} deriving (Eq, Show)
data Env = Env { env :: Map String Loc, funs :: Map String Function, types :: Map String Type } deriving (Eq, Show)
type Store = Map Loc Val

type InterpretMonad a = ExceptT String (ReaderT Env (StateT Store IO)) a

addLocToEnv :: String -> Loc -> Type -> Env -> Env
addLocToEnv var loc typ e = e { env = Map.insert var loc (env e), types = Map.insert var typ (types e) }

addFunToEnv :: String -> Function -> Env -> Env
addFunToEnv var fun e = e { funs = Map.insert var fun (funs e) }

addValToStore :: Loc -> Val -> Store -> Store
addValToStore loc val s = Map.insert loc val s


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

-- Unpack Val
unpackInt :: Val -> Integer
unpackInt (VInt i) = i

-- typeof
typeof :: Val -> Type
typeof (VInt _) = Int
typeof (VBool _) = Bool
typeof (VStr _) = Str
typeof (VTuple types) = Tuple (Prelude.map typeof types)
typeof (VNone) = None

-- Gram.Type -> Type
gtypeToType :: Gram.Type -> Type
gtypeToType (Gram.Int _) = Int
gtypeToType (Gram.Str _) = Str
gtypeToType (Gram.Bool _) = Bool
gtypeToType (Gram.List _ t) = List (gtypeToType t)
gtypeToType (Gram.Tuple _ types) = Tuple (Prelude.map gtypeToType types)


-- Interpret expressions
eval :: Gram.Expr -> InterpretMonad Val

eval (Gram.ELitInt _ integer) = return (VInt integer)
eval (Gram.ELitTrue _) = return (VBool True)
eval (Gram.ELitFalse _) = return (VBool False)
eval (Gram.EString _ string) = return (VStr string)
eval (Gram.ELitNone _) = return VNone

eval (Gram.ERel _ e1 op e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (VInt ev1, VInt ev2) -> 
      case op of
        Gram.LTH _ -> return $ VBool (ev1 < ev2)
        Gram.LE _ -> return $ VBool (ev1 <= ev2)
        Gram.GTH _ -> return $ VBool (ev1 > ev2)
        Gram.GE _ -> return $ VBool (ev1 >= ev2)
        Gram.EQU _ -> return $ VBool (ev1 == ev2)
        Gram.NE _ -> return $ VBool (ev1 /= ev2)
    (VBool ev1, VBool ev2) -> 
      case op of
        Gram.LTH _ -> return $ VBool (ev1 < ev2)
        Gram.LE _ -> return $ VBool (ev1 <= ev2)
        Gram.GTH _ -> return $ VBool (ev1 > ev2)
        Gram.GE _ -> return $ VBool (ev1 >= ev2)
        Gram.EQU _ -> return $ VBool (ev1 == ev2)
        Gram.NE _ -> return $ VBool (ev1 /= ev2)
    _ -> throwError $ "Cannot compare non-integers and non-strings"

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
    _ -> throwError $ "Cannot add and subtract non-integers and non-strings"

eval (Gram.EMul _ e1 op e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt n1, VInt n2) -> case op of
      Gram.Times _ -> return $ VInt (n1 * n2)
      Gram.Div _ -> if n2 == 0
                      then throwError "Division by zero"
                      else return $ VInt (n1 `div` n2)
      Gram.Mod _ -> if n2 == 0
                      then throwError "Modulo by zero"
                      else return $ VInt (n1 `mod` n2)
    _ -> throwError "Cannot perform */mod on non-integers"
  
eval (Gram.EVar _ (Gram.Ident id)) = do
  maybeLoc <- asks (Map.lookup id . env)
  case maybeLoc of
    Just loc -> do
      maybeVal <- gets (Map.lookup loc)
      case maybeVal of
        Just val -> return val
        Nothing -> throwError $ "Variable " ++ id ++ " is uninitialized."
    Nothing -> throwError $ "Variable " ++ id ++ " is not in scope."

-- wywołanie funkcji
-- TODO

eval (Gram.EApp l (Gram.Ident funname) exprs) = do
  maybeFun <- asks (Map.lookup funname . funs)
  case maybeFun of
    Nothing -> throwError $ "Function " ++ funname ++ " is not defined."
    Just fun -> do
      let args = funArgs fun
      let retType = funRetType fun
      let body = funBody fun
      let dEnv = declEnv fun
      -- If there are less arguments than parameters, fill the rest with None
      let exprs' = exprs ++ (Prelude.replicate (length args - length exprs) (Gram.ELitNone l))
      newEnv <- addArgsToEnv (zip args exprs')
      -- join newEnv with dEnv and run execBlock
      let joinedEnv = Env { env = Map.union (env newEnv) (env dEnv),
                            types = Map.union (types newEnv) (types dEnv),
                            funs = Map.union (funs newEnv) (funs dEnv) }
      let joinedEnv' = addFunToEnv funname (Function funname args retType body newEnv) joinedEnv
      result <- local (const joinedEnv') (execBlock body)
      return result
      where
        addArgsToEnv :: [(Gram.Arg, Gram.Expr)] -> InterpretMonad Env
        addArgsToEnv [] = return Env { env = fromList [], types = fromList [], funs = fromList [] }
        addArgsToEnv ((Gram.ArType _ (Gram.Ident id) gtype, expr):r) = do
          loc <- newloc
          val <- eval expr
          -- add loc-value to store
          modify (addValToStore loc val)
          env <- addArgsToEnv r
          return (addLocToEnv id loc (gtypeToType gtype) env)
        addArgsToEnv ((Gram.ArValue _ (Gram.Ident id) defExpr, givenExpr):r) = do
          loc <- newloc
          val <- eval givenExpr -- TODO trzeba sprawdzic typy
          defVal <- eval defExpr
          let typ = typeof defVal
          if val == VNone
            then do
              modify (addValToStore loc defVal)
            else do
              modify (addValToStore loc val)
          env <- addArgsToEnv r
          return (addLocToEnv id loc typ env)
        addArgsToEnv ((Gram.ArRef _ (Gram.Ident id) gtype, expr):r) = do
          case expr of
            (Gram.EVar _ (Gram.Ident id')) -> do
              maybeLoc <- asks (Map.lookup id' . env) -- TODO: dużo błędów do sprawdzenia
              maybeType <- asks (Map.lookup id' . types)
              case (maybeLoc, maybeType) of
                (Just loc, Just typ) -> do
                  env <- addArgsToEnv r
                  return (addLocToEnv id loc typ env)
                _ -> throwError $ "Passed variable " ++ id' ++ " is not in scope."
            _ -> throwError "Cannot pass non-identifier as reference argument"

--- Execute Statement

execBlock :: Gram.Block -> InterpretMonad Val
execBlock (Gram.Blk _ stmts) = execStmt stmts

execStmt :: [Gram.Stmt] -> InterpretMonad Val
execStmt [] = return VNone
execStmt ((Gram.Empty _):r) = execStmt r

execStmt ((Gram.Ret _ expr):r) = do
  result <- eval expr
  return result

execStmt ((Gram.Print _ e):r) = do
  result <- eval e
  showVal result
  execStmt r

execStmt ((Gram.AssStmt _ target source):r) = do
  case target of
    (Gram.TargetId _ (Gram.Ident var)) -> do -- when target is an Identifier
      -- check if it is in scope
      maybeloc <- asks (Map.lookup var . env)
      case maybeloc of
        Nothing ->  -- Not in scope - declaration!
          case source of
            (Gram.SourceType _ gtype) -> do-- when source is type, e.g "int, bool"
              loc <- newloc
              modify (Map.insert loc VNone)
              local (addLocToEnv var loc (gtypeToType gtype)) (execStmt r)
            (Gram.SourceExpr _ expr) -> do -- when source is expression, e.g "3+3"
              loc <- newloc
              val <- eval expr
              modify (Map.insert loc val)
              local (addLocToEnv var loc (typeof val)) (execStmt r)
        -- In scope - assignment!
        Just loc ->
          case source of
            (Gram.SourceType _ typ) -> -- when source is type, e.g "int, bool"
              throwError $ "Cannot assign type to visible variable " ++ var
            (Gram.SourceExpr _ expr) -> do -- when source is expression, e.g "3+3"
              varType <- asks (fromJust . Map.lookup var . types)
              val <- eval expr
              -- check if type of val is the same as type of val
              if (varType == typeof val) then do
                modify (Map.insert loc val)
                execStmt r
              else
                throwError $ "Cannot assign " ++ (show val) ++ " to variable " ++ var ++ " of type " ++ (show varType)
    (Gram.DummyTarget _) -> do -- when target is DummyTarget
      case source of
        (Gram.SourceType _ typ) -> -- when source is type, e.g "int, bool"
          throwError $ "Cannot assign type to dummy target"
        (Gram.SourceExpr _ expr) -> do -- when source is expression, e.g "3+3"
          eval expr
          execStmt r


execStmt ((Gram.BStmt _ block):r) = do
  execBlock block
  execStmt r

--- function definition
{-
  type Stmt = Stmt' BNFC'Position
data Stmt' a
    = Empty a
    | BStmt a (Block' a)
    | AssStmt a (Target' a) (Source' a)
    | VoidCall a Ident [Expr' a]
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) (Block' a)
    | CondElse a (Expr' a) (Block' a) (Block' a)
    | While a (Expr' a) (Block' a)
    | Break a
    | Cont a
    | For a Ident (Expr' a) (Expr' a) (Block' a)
    | ForEach a Ident (Expr' a) (Block' a)
    | Append a Ident (Expr' a)
    | Pop a Ident
    | DeclFunc a (FnDef' a)
    | Print a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Target = Target' BNFC'Position
data Target' a
    = TargetId a Ident
    | TargetList a Ident (Expr' a)
    | DummyTarget a
    | TupleTarget a [Target' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Source = Source' BNFC'Position
data Source' a = SourceExpr a (Expr' a) | SourceType a (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type FnDef = FnDef' BNFC'Position
data FnDef' a = FuncStmt a Ident [Arg' a] (Type' a) (Block' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)
-}

-- Cond
execStmt ((Gram.Cond _ expr block):r) = do
  val <- eval expr
  case val of
    (VBool True) -> execBlock block
    (VBool False) -> execStmt r
    _ -> throwError "Condition is not a boolean" 

execStmt ((Gram.CondElse _ expr block1 block2):r) = do
  val <- eval expr
  case val of
    (VBool True) -> execBlock block1
    (VBool False) -> execBlock block2
    _ -> throwError "Condition is not a boolean"

-- data Function = Function { funName :: String, funArgs :: [Arg], funRetType :: Type, funBody :: Block } deriving (Eq, Show)

execStmt ((Gram.DeclFunc _ (Gram.FuncStmt _ (Gram.Ident funName) args retType body)):r) = do
  -- check if function is already defined
  maybeFun <- asks (Map.lookup funName . funs)
  case maybeFun of
    Just _ -> throwError $ "Function " ++ funName ++ " is already defined."
    Nothing -> do
      -- create new function
      -- dEnv should be initialized as current environment
      dEnv <- ask
      let fun = Function { funName = funName, funArgs = args, funRetType = gtypeToType retType, funBody = body, declEnv = dEnv }
      -- add function to environment
      local (addFunToEnv funName fun) (execStmt r)


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
execProgram :: Gram.Program -> InterpretMonad Val
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
      