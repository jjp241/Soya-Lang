module TypeChecker where
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.String
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import qualified AbsSoya as Gram
-- TODO: import grammar

data Type = Int | Str | Bool | None | Tuple [Type] | List Type | Fun Type [Type] deriving (Eq, Show)
data TEnv = TEnv { types :: Map String Type, in_function :: Maybe String } deriving (Eq, Show)
type Pos = Gram.BNFC'Position

type TypeMonad a = ExceptT String (Reader TEnv) a

gtypeToType :: Gram.Type -> Type
gtypeToType (Gram.Int _) = Int
gtypeToType (Gram.Str _) = Str
gtypeToType (Gram.Bool _) = Bool
gtypeToType (Gram.List _ t) = List (gtypeToType t)
gtypeToType (Gram.Tuple _ types) = Tuple (Prelude.map gtypeToType types)

addVariableType :: String -> Type -> TEnv -> TEnv
addVariableType name typ env = env { types = Map.insert name typ (types env) }

inFunction :: String -> TEnv -> TEnv
inFunction name env = env { in_function = Just name }

----------------- Expressions -----------------
checkExpr :: Gram.Expr -> TypeMonad Type
checkExpr (Gram.ELitInt _ _) = return Int
checkExpr (Gram.ELitTrue _) = return Bool
checkExpr (Gram.ELitFalse _) = return Bool
checkExpr (Gram.EString _ _) = return Str

checkExpr (Gram.ERel pos e1 _ e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Bool
    (Bool, Bool) -> return Bool
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show t1 ++ " and " ++ show t2 ++ " are not comparable"

checkExpr (Gram.EAdd pos e1 _ e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Int
    (Str, Str) -> return Str
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show t1 ++ " and " ++ show t2 ++ " are not addable"

checkExpr (Gram.EMul pos e1 _ e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Int
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show t1 ++ " and " ++ show t2 ++ " are not multipliable"

checkExpr (Gram.EVar pos (Gram.Ident name)) = do
  maybeType <- asks (Map.lookup name . types)
  case maybeType of
    Just t -> return t
    Nothing -> throwError $ "Type error in " ++ show pos ++ ": " ++ name ++ " is not defined"

checkExpr (Gram.EApp pos (Gram.Ident name) exprs) = do
  maybeFunType <- asks (Map.lookup name . types)
  case maybeFunType of
    Nothing -> throwError $ "Type error in " ++ show pos ++ ": " ++ name ++ " is not defined"
    Just (Fun retType argTypes) -> do
      -- check if number of arguments is not too big
      if Prelude.length exprs > Prelude.length argTypes
        then throwError $ "Type error in " ++ show pos ++ ": " ++ name ++ " takes maximum " ++ show (Prelude.length argTypes) ++ " arguments, but " ++ show (Prelude.length exprs) ++ " were given"
        else do
          -- check if types of arguments are correct
          exprTypes <- mapM checkExpr exprs
          -- because we don't have to pass all arguments, we have to check only the given ones
          let argTypesToCheck = Prelude.take (Prelude.length exprs) argTypes
          if exprTypes == argTypesToCheck
            then return retType
            else throwError $ "Type error in " ++ show pos ++ ": " ++ name ++ " takes arguments of types " ++ show argTypes ++ ", but " ++ show exprTypes ++ " were given"
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ name ++ " is not a function"

----------------- Arguments -----------------
getArgumentTypes :: [Gram.Arg] -> TypeMonad [(String, Type)]
getArgumentTypes [] = return []
getArgumentTypes ((Gram.ArType _ (Gram.Ident name) gtype):r) = do
  rest <- getArgumentTypes r
  return ((name, gtypeToType gtype):rest)
getArgumentTypes ((Gram.ArValue _ (Gram.Ident name) expr):r) = do
  rest <- getArgumentTypes r
  exprType <- checkExpr expr
  return ((name, exprType):rest)
getArgumentTypes ((Gram.ArRef _ (Gram.Ident name) gtype):r) = do
  rest <- getArgumentTypes r
  return ((name, gtypeToType gtype):rest)

addArgumentTypesToEnv :: [(String, Type)] -> TEnv -> TEnv
addArgumentTypesToEnv [] env = env
addArgumentTypesToEnv ((name, typ):r) env = addArgumentTypesToEnv r (addVariableType name typ env)

----------------- Statements -----------------
checkStmts :: [Gram.Stmt] -> TypeMonad ()
checkStmts [] = return ()
checkStmts ((Gram.Empty _):r) = checkStmts r


checkStmts ((Gram.Print pos e):r) = do
  -- Everything is printable
  checkExpr e
  checkStmts r


checkStmts ((Gram.AssStmt pos target source):r) = do 
  case target of
    (Gram.TargetId pos1 (Gram.Ident var)) -> do -- when target is an Identifier
      -- check if it is in scope
      maybeType <- asks (Map.lookup var . types)
      case maybeType of
        Nothing ->  -- Not in scope - declaration!
          case source of
            (Gram.SourceType pos2 gtype) -> do -- when source is type, e.g "int, bool"
              local (addVariableType var (gtypeToType gtype)) (checkStmts r)
            (Gram.SourceExpr pos2 expr) -> do -- when source is expression, e.g "3+3"
              exprType <- checkExpr expr
              if exprType == None
                then throwError $ "Type error in " ++ show pos ++ ": " ++ show exprType ++ " cannot be assigned to variable " ++ var
                else local (addVariableType var exprType) (checkStmts r)
        -- In scope - assignment!
        Just loc ->
          case source of
            (Gram.SourceType _ typ) -> -- when source is type, e.g "int, bool"
              throwError $ "Cannot assign type " ++ (show typ) ++ " to variable " ++ var ++ " of type " ++ (show loc)
            (Gram.SourceExpr _ expr) -> do -- when source is expression, e.g "3+3"
              maybeVarType <- asks (Map.lookup var . types)
              case maybeVarType of
                Nothing -> throwError $ "Variable " ++ var ++ " is not defined"
                Just varType -> do
                  exprType <- checkExpr expr
                  if varType == exprType
                    then checkStmts r
                    else throwError $ "Cannot assign type " ++ (show exprType) ++ " to variable " ++ var ++ " of type " ++ (show varType)
    (Gram.DummyTarget _) -> do -- when target is DummyTarget
      case source of
        (Gram.SourceType pos2 gtype) -> 
          throwError $ "Cannot assign type " ++ (show gtype) ++ " to dummy target"
        (Gram.SourceExpr pos2 expr) -> do -- when source is expression, e.g "3+3"
          checkExpr expr
          checkStmts r
      checkStmts r
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show target ++ " is not a valid target"


-- return statement
checkStmts ((Gram.Ret pos expr):r) = do
  maybeInFun <- asks in_function
  case maybeInFun of
    Nothing -> throwError $ "Return statement outside of function"
    Just inFun -> do
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> throwError $ "Function " ++ inFun ++ " is not defined"
        Just (Fun retType _) -> do
          exprType <- checkExpr expr
          if retType == exprType
            then checkStmts r
            else throwError $ "Cannot return type " ++ (show exprType) ++ " from function " ++ inFun ++ " of return type " ++ (show retType)
        _ -> throwError $ "Function " ++ inFun ++ " is not defined"


checkStmts ((Gram.VRet pos):r) = do
  maybeInFun <- asks in_function
  case maybeInFun of
    Nothing -> throwError $ "Return statement outside of function"
    Just inFun -> do
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> throwError $ "Function " ++ inFun ++ " is not defined"
        Just (Fun retType _) -> do
          if retType == None
            then checkStmts r
            else throwError $ "Cannot return type " ++ (show None) ++ " from function " ++ inFun ++ " of return type " ++ (show retType)
        _ -> throwError $ "Function " ++ inFun ++ " is not defined"


checkStmts ((Gram.DeclFunc pos (Gram.FuncStmt _ (Gram.Ident funName) args retType (Gram.Blk _ bodyStmts))):r) = do
  -- check if function is already defined
  maybeFunType <- asks (Map.lookup funName . types)
  case maybeFunType of
    Just _ -> throwError $ "Function " ++ funName ++ " is already defined."
    Nothing -> do
      -- create new function - get argument types and return type
      argumentTypes <- getArgumentTypes args
      let funType = Fun (gtypeToType retType) (Prelude.map snd argumentTypes)
      -- check if function body is correct, addVariableType and inFunction, and addArgumentTypesToEnv
      local (addVariableType funName funType . inFunction funName . addArgumentTypesToEnv argumentTypes) (checkStmts bodyStmts)
      -- run rest of the program
      local (addVariableType funName funType) (checkStmts r)


checkStmts ((Gram.DeclFunc pos (Gram.VoidFuncStmt _ (Gram.Ident funName) args (Gram.Blk _ bodyStmts))):r) = do
  -- check if function is already defined
  maybeFunType <- asks (Map.lookup funName . types)
  case maybeFunType of
    Just _ -> throwError $ "Function " ++ funName ++ " is already defined."
    Nothing -> do
      -- create new function - get argument types and return type
      argumentTypes <- getArgumentTypes args
      let funType = Fun None (Prelude.map snd argumentTypes)
      -- check if function body is correct, addVariableType and inFunction, and addArgumentTypesToEnv
      local (addVariableType funName funType . inFunction funName . addArgumentTypesToEnv argumentTypes) (checkStmts bodyStmts)
      -- run rest of the program
      local (addVariableType funName funType) (checkStmts r)


checkStmts ((Gram.Cond pos expr (Gram.Blk _ bodyStmts)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> checkStmts bodyStmts >> checkStmts r
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show eType ++ " is not a boolean"

checkStmts ((Gram.CondElse pos expr (Gram.Blk _ bodyStmts1) (Gram.Blk _ bodyStmts2)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> checkStmts bodyStmts1 >> checkStmts bodyStmts2 >> checkStmts r
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show eType ++ " is not a boolean"

checkStmts ((Gram.VoidCall pos (Gram.Ident funName) exprs):r) = do
  resType <- checkExpr (Gram.EApp pos (Gram.Ident funName) exprs)
  checkStmts r

----------------- Program -----------------
checkProgram :: Gram.Program -> TypeMonad ()
checkProgram (Gram.Prog pos stmts) = checkStmts stmts

--- Run program
runChecker :: Gram.Program -> Either String ()
runChecker prog =
    let env = TEnv { types = fromList [], in_function = Nothing }
    in runReader (runExceptT (checkProgram prog)) env
