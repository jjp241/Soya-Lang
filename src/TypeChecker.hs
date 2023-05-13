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
data TEnv = TEnv { types :: Map String Type,
                   mutable :: Map String Bool,
                   in_function :: Maybe String,
                   in_loop :: Bool } deriving (Eq, Show)
type Pos = Gram.BNFC'Position

type TypeMonad a = ExceptT String (Reader TEnv) a

gtypeToType :: Gram.Type -> Type
gtypeToType (Gram.Int _) = Int
gtypeToType (Gram.Str _) = Str
gtypeToType (Gram.Bool _) = Bool
gtypeToType (Gram.List _ t) = List (gtypeToType t)
gtypeToType (Gram.Tuple _ types) = Tuple (Prelude.map gtypeToType types)

addVariableType :: String -> Type -> TEnv -> TEnv
-- by default variables are mutable
addVariableType name typ env = env { types = Map.insert name typ (types env), mutable = Map.insert name True (mutable env) }

addImmutableVariableType :: String -> Type -> TEnv -> TEnv
addImmutableVariableType name typ env = env { types = Map.insert name typ (types env), mutable = Map.insert name False (mutable env) }

inFunction :: String -> TEnv -> TEnv
inFunction name env = env { in_function = Just name }

inLoop :: Bool -> TEnv -> TEnv
inLoop b env = env { in_loop = b }

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


checkExpr (Gram.ENewList pos exprs) = do
  exprTypes <- mapM checkExpr exprs
  if Prelude.length exprTypes == 0
    then throwError $ "Type error in " ++ show pos ++ ": cannot create empty list"
    else do
      let firstType = Prelude.head exprTypes
      if all (== firstType) exprTypes
        then return (List firstType)
        else throwError $ "Type error in " ++ show pos ++ ": " ++ show exprTypes ++ " are not of the same type"


checkExpr (Gram.EGetElem pos (Gram.Ident var) expr) = do
  maybeVarType <- asks (Map.lookup var . types)
  case maybeVarType of
    Just (List t) -> do
      exprType <- checkExpr expr
      if exprType == Int
        then return t
        else throwError $ "Type error in " ++ show pos ++ ": " ++ show exprType ++ " is not an integer"
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ var ++ " is not a list or tuple"

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
checkStmts [] = do
  -- if I'm in a function, check if it is a void function
  maybeInFun <- asks in_function
  case maybeInFun of
    Nothing -> return ()
    Just inFun -> do
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> throwError $ "Function " ++ inFun ++ " is not defined"
        Just (Fun retType _) -> do
          if retType == None
            then return ()
            else throwError $ "Function " ++ inFun ++ " should return type " ++ (show retType) ++ ", but it doesn't"

checkStmts ((Gram.Empty _):r) = checkStmts r


checkStmts ((Gram.Print pos e):r) = do
  -- Everything is printable
  checkExpr e
  checkStmts r


checkStmts ((Gram.AssStmt pos target source):r) = do 
  case source of
    -- right side: type => Declaration of new variable
    (Gram.SourceType pos2 gtype) -> do 
      case target of
        (Gram.TargetId pos1 (Gram.Ident var)) -> do -- when target is an Identifier
          local (addVariableType var (gtypeToType gtype)) (checkStmts r)
        (Gram.DummyTarget _) -> do -- when target is DummyTarget
          throwError $ "Type error in " ++ show pos ++ ": cannot assign type " ++ (show gtype) ++ " to dummy target"
        _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show target ++ " is not a valid target"

    -- right side: expression => Assignment to variable if exists, declaration of new variable if not
    (Gram.SourceExpr pos2 expr) -> do 
      exprType <- checkExpr expr
      case target of
        (Gram.TargetId pos1 (Gram.Ident var)) -> do -- when target is an Identifier
          -- check if variable is mutable
          isMutable <- asks (Map.lookup var . mutable)
          case isMutable of
            Just True -> do
              maybeVarType <- asks (Map.lookup var . types)
              case maybeVarType of
                Just varType -> do
                  if varType == exprType
                    then checkStmts r
                    else throwError $ "Type error in " ++ show pos ++ ": " ++ show exprType ++ " cannot be assigned to variable " ++ var
                -- create new variable
                Nothing -> local (addVariableType var exprType) (checkStmts r)
            Just False -> throwError $ "Cannot assign to immutable variable " ++ var
            -- create new variable
            Nothing -> local (addVariableType var exprType) (checkStmts r)
        (Gram.DummyTarget _) -> do -- when target is DummyTarget, do nothing
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
            then return ()
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
  argumentTypes <- getArgumentTypes args
  let funType = Fun (gtypeToType retType) (Prelude.map snd argumentTypes)
  -- check if function body is correct, addVariableType and inFunction, and addArgumentTypesToEnv
  local (addVariableType funName funType . inFunction funName . addArgumentTypesToEnv argumentTypes) (checkStmts bodyStmts)
  -- run rest of the program
  local (addVariableType funName funType) (checkStmts r)


checkStmts ((Gram.DeclFunc pos (Gram.VoidFuncStmt _ (Gram.Ident funName) args (Gram.Blk _ bodyStmts))):r) = do
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


checkStmts ((Gram.While pos expr (Gram.Blk pos2 bodyStmts)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> do
      local (inLoop True) (checkStmts bodyStmts)
      checkStmts r
    _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show eType ++ " is not a boolean"


checkStmts ((Gram.For pos (Gram.Ident var) expr1 expr2 (Gram.Blk pos2 bodyStmts)):r) = do
  eType1 <- checkExpr expr1
  eType2 <- checkExpr expr2
  -- var should not be in scope
  maybeVarType <- asks (Map.lookup var . types)
  case maybeVarType of
    Just _ -> throwError $ "Variable " ++ var ++ " is already defined"
    Nothing -> do
      case (eType1, eType2) of
        (Int, Int) -> do
          local (addImmutableVariableType var Int . inLoop True) (checkStmts bodyStmts)
          checkStmts r
        _ -> throwError $ "Type error in " ++ show pos ++ ": " ++ show eType1 ++ " and " ++ show eType2 ++ " are not integers"


checkStmts ((Gram.Break pos):r) = do
  inLoop <- asks in_loop
  if inLoop
    then checkStmts r
    else throwError $ "Break statement outside of while loop"


checkStmts ((Gram.Cont pos):r) = do
  inLoop <- asks in_loop
  if inLoop
    then checkStmts r
    else throwError $ "Continue statement outside of while loop"


----------------- Program -----------------
checkProgram :: Gram.Program -> TypeMonad ()
checkProgram (Gram.Prog pos stmts) = checkStmts stmts

--- Run program
runChecker :: Gram.Program -> Either String ()
runChecker prog =
    let env = TEnv { types = fromList [],
                     mutable = fromList [],
                     in_function = Nothing,
                     in_loop = False}
    in runReader (runExceptT (checkProgram prog)) env
