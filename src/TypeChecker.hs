module TypeChecker where
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.String
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import qualified AbsSoya as Gram

type Pos = Gram.BNFC'Position
data Type = Int | Str | Bool | None | Tuple [Type] | List Type | Fun Type [Type] deriving (Eq, Show)
data TEnv = TEnv { types :: Map String Type,
                   mutable :: Map String Bool,
                   in_function :: Maybe String,
                   func_pos :: Maybe Pos,
                   in_loop :: Bool } deriving (Eq, Show)

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

funcPos :: Pos -> TEnv -> TEnv
funcPos pos env = env { func_pos = Just pos }

-- function to print error message
tError :: String -> Pos -> TypeMonad a
tError msg pos = throwError $ "[TYPE ERROR at " ++ show pos ++ "]: " ++ msg

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
    _ -> tError ("Cannot compare " ++ show t1 ++ " and " ++ show t2) pos


checkExpr (Gram.EAdd pos e1 _ e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Int
    (Str, Str) -> return Str
    _ -> tError ("Cannot add " ++ show t1 ++ " and " ++ show t2) pos


checkExpr (Gram.EMul pos e1 _ e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Int
    _ -> tError ("Cannot multiply " ++ show t1 ++ " and " ++ show t2) pos


checkExpr (Gram.EVar pos (Gram.Ident name)) = do
  maybeType <- asks (Map.lookup name . types)
  case maybeType of
    Just t -> return t
    Nothing -> tError (name ++ " is not defined") pos


checkExpr (Gram.EApp pos (Gram.Ident name) exprs) = do
  maybeFunType <- asks (Map.lookup name . types)
  case maybeFunType of
    Nothing -> tError (name ++ " is not defined") pos
    Just (Fun retType argTypes) -> do
      -- check if number of arguments is not too big
      if Prelude.length exprs > Prelude.length argTypes
        then tError ("Too many arguments in " ++ name) pos
        else do
          -- check if types of arguments are correct
          exprTypes <- mapM checkExpr exprs
          -- because we don't have to pass all arguments, we have to check only the given ones
          let argTypesToCheck = Prelude.take (Prelude.length exprs) argTypes
          if exprTypes == argTypesToCheck
            then return retType
            else tError ("Wrong types of arguments in " ++ name ++ ": " ++ show exprTypes ++ " instead of " ++ show argTypesToCheck) pos


checkExpr (Gram.ENewList pos exprs) = do
  exprTypes <- mapM checkExpr exprs
  if Prelude.length exprTypes == 0
    then tError ("Cannot create empty list") pos
    else do
      let firstType = Prelude.head exprTypes
      if all (== firstType) exprTypes
        then return (List firstType)
        else tError ("Cannot create list of different types: " ++ show exprTypes) pos


checkExpr (Gram.EGetElem pos (Gram.Ident var) expr) = do
  maybeVarType <- asks (Map.lookup var . types)
  case maybeVarType of
    Just (List t) -> do
      exprType <- checkExpr expr
      if exprType == Int
        then return t
        else tError ("Cannot get element of list with index of type " ++ show exprType) pos
    _ -> tError (var ++ " is not a list") pos

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
      funcPos <- fromJust <$> asks func_pos
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> tError ("Unexpected error! No function " ++ inFun ++ " details found!") funcPos
        Just (Fun retType _) -> do
          if retType == None
            then return ()
            else tError ("Function " ++ inFun ++ " should return type " ++ (show retType) ++ " but returns nothing") funcPos


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
          tError ("cannot assign type " ++ (show gtype) ++ " to dummy target") pos
        _ -> tError ("cannot assign type " ++ (show gtype) ++ " to " ++ (show target)) pos

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
                    else tError ("cannot assign type " ++ (show exprType) ++ " to " ++ var ++ " of type " ++ (show varType)) pos
                -- create new variable
                Nothing -> local (addVariableType var exprType) (checkStmts r)
            Just False -> tError ("cannot assign to immutable variable " ++ var) pos
            -- create new variable
            Nothing -> local (addVariableType var exprType) (checkStmts r)
        (Gram.DummyTarget _) -> do -- when target is DummyTarget, do nothing
          checkStmts r
        _ -> tError ("cannot assign type " ++ (show exprType) ++ " to " ++ (show target)) pos


-- return statement
checkStmts ((Gram.Ret pos expr):r) = do
  maybeInFun <- asks in_function
  case maybeInFun of
    Nothing -> tError ("Return statement outside of function") pos
    Just inFun -> do
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> tError ("Function " ++ inFun ++ " is not defined") pos
        Just (Fun retType _) -> do
          exprType <- checkExpr expr
          if retType == exprType
            then return ()
            else tError ("Cannot return type " ++ (show exprType) ++ " from function " ++ inFun ++ " of return type " ++ (show retType)) pos


checkStmts ((Gram.VRet pos):r) = do
  maybeInFun <- asks in_function
  case maybeInFun of
    Nothing -> tError ("Return statement outside of function") pos
    Just inFun -> do
      maybeFunType <- asks (Map.lookup inFun . types)
      case maybeFunType of
        Nothing -> tError ("Function " ++ inFun ++ " is not defined") pos
        Just (Fun retType _) -> do
          if retType == None
            then checkStmts r
            else tError ("Cannot return nothing from function " ++ inFun ++ " of return type " ++ (show retType)) pos


checkStmts ((Gram.DeclFunc pos (Gram.FuncStmt _ (Gram.Ident funName) args retType (Gram.Blk _ bodyStmts))):r) = do
  argumentTypes <- getArgumentTypes args
  let funType = Fun (gtypeToType retType) (Prelude.map snd argumentTypes)
  -- check if function body is correct, addVariableType and inFunction, and addArgumentTypesToEnv
  local (addVariableType funName funType . inFunction funName . funcPos pos . addArgumentTypesToEnv argumentTypes) (checkStmts bodyStmts)
  local (addVariableType funName funType) (checkStmts r)


checkStmts ((Gram.DeclFunc pos (Gram.VoidFuncStmt _ (Gram.Ident funName) args (Gram.Blk _ bodyStmts))):r) = do
  argumentTypes <- getArgumentTypes args
  let funType = Fun None (Prelude.map snd argumentTypes)
  -- check if function body is correct, addVariableType and inFunction, and addArgumentTypesToEnv
  local (addVariableType funName funType . inFunction funName . funcPos pos . addArgumentTypesToEnv argumentTypes) (checkStmts bodyStmts)
  local (addVariableType funName funType) (checkStmts r)


checkStmts ((Gram.Cond pos expr (Gram.Blk _ bodyStmts)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> checkStmts bodyStmts >> checkStmts r
    _ -> tError (show eType ++ " is not a boolean") pos


checkStmts ((Gram.CondElse pos expr (Gram.Blk _ bodyStmts1) (Gram.Blk _ bodyStmts2)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> checkStmts bodyStmts1 >> checkStmts bodyStmts2 >> checkStmts r
    _ -> tError (show eType ++ " is not a boolean") pos


checkStmts ((Gram.VoidCall pos (Gram.Ident funName) exprs):r) = do
  resType <- checkExpr (Gram.EApp pos (Gram.Ident funName) exprs)
  checkStmts r


checkStmts ((Gram.While pos expr (Gram.Blk pos2 bodyStmts)):r) = do
  eType <- checkExpr expr
  case eType of
    Bool -> do
      local (inLoop True) (checkStmts bodyStmts)
      checkStmts r
    _ -> tError (show eType ++ " is not a boolean") pos


checkStmts ((Gram.For pos (Gram.Ident var) expr1 expr2 (Gram.Blk pos2 bodyStmts)):r) = do
  eType1 <- checkExpr expr1
  eType2 <- checkExpr expr2

  case (eType1, eType2) of
    (Int, Int) -> do
      local (addImmutableVariableType var Int . inLoop True) (checkStmts bodyStmts)
      checkStmts r
    _ -> tError (show eType1 ++ " and " ++ show eType2 ++ " are not integers") pos


checkStmts ((Gram.Break pos):r) = do
  inLoop <- asks in_loop
  if inLoop
    then checkStmts r
    else tError "Break statement outside of while loop" pos


checkStmts ((Gram.Cont pos):r) = do
  inLoop <- asks in_loop
  if inLoop
    then checkStmts r
    else tError "Continue statement outside of while loop" pos


----------------- Program -----------------
checkProgram :: Gram.Program -> TypeMonad ()
checkProgram (Gram.Prog pos stmts) = checkStmts stmts

--- Run program
runChecker :: Gram.Program -> Either String ()
runChecker prog =
    let env = TEnv { types = fromList [],
                     mutable = fromList [],
                     in_function = Nothing,
                     func_pos = Nothing,
                     in_loop = False}
    in runReader (runExceptT (checkProgram prog)) env
