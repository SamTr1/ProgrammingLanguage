module Lang where

import Parsing
    ( Alternative(many, (<|>)),
      Parser(runParser),
      failure,
      identifier,
      natural,
      symbol )

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type Identifier = String

data Value = NumV Int
           | BoolV Bool
           | FnV [Identifier] Expr Env -- Parameters, Body, Closure
           deriving (Show,Eq)

type Env = [(Identifier,Value)]

data Expr = NumE Int
          | TrueE
          | FalseE
          | Op Operator Expr Expr
          | If Expr Expr Expr
          | FnDef [Identifier] Expr -- Parameters, Body
          | FnApp Expr [Expr]       -- Function expression
          | Id Identifier
          deriving (Show, Eq)

data Operator = Plus
              | Mult
              | Equal
              | LessThan
              deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
 
parens :: Parser a -> Parser a
parens p = do symbol "("
              exp <- p
              symbol ")"
              return exp

pExpr :: Parser Expr
pExpr = parens (pBuiltIn <|> pFnApp) <|> pLiteral <|> pId
  where pFnApp   = FnApp <$> pExpr <*> many pExpr
        pLiteral = pNum <|> pTrue <|> pFalse
        pNum     = NumE <$> natural
        pTrue    = symbol "#t" >> return TrueE
        pFalse   = symbol "#f" >> return FalseE
        pId      = Id <$> identifier

pBuiltIn :: Parser Expr
pBuiltIn =
  do sym <- identifier
     case sym of
       "if"     -> If <$> pExpr <*> pExpr <*> pExpr
       "lambda" -> FnDef <$> parens (many identifier) <*> pExpr
       "not"    -> If <$> pExpr <*> return FalseE <*> return TrueE
       "cond"   -> pCondCases
       _        -> if sym `elem` binarySymbols
                   then binaryParseTable sym <$> pExpr <*> pExpr
                   else failure

pCondCases :: Parser Expr
pCondCases =
  do symbol "("
     cnd <- pExpr
     thn <- pExpr
     symbol ")"
     if cnd == Id "else"
     then return thn
     else If cnd thn <$> pCondCases

binarySymbols :: [String]
binarySymbols = ["+", "*", "=", "<", "-", "and", "or", "<=", ">=", ">"]

binaryParseTable :: String -> Expr -> Expr -> Expr
binaryParseTable sym l r = case sym of
  "+"   -> Op Plus     l r
  "*"   -> Op Mult     l r
  "="   -> Op Equal    l r
  "<"   -> Op LessThan l r
  "-"   -> Op Plus l (Op Mult r (NumE (-1)))
  "and" -> If l r FalseE
  "or"  -> If l TrueE r
  "<="  -> If (Op LessThan l r) TrueE (Op Equal l r)
  ">="  -> If (Op LessThan r l) TrueE (Op Equal l r)
  ">"   -> Op LessThan r l

parseString :: String -> Expr
parseString s = case runParser pExpr s of
  [(x,"")] -> x
  _        -> error "*** not parsable"

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

opTable :: Operator -> Value -> Value -> Value
opTable Plus     (NumV x) (NumV y) = NumV  (x + y)
opTable Mult     (NumV x) (NumV y) = NumV  (x * y)
opTable LessThan (NumV x) (NumV y) = BoolV (x < y)
opTable Equal    x        y        = BoolV (x == y)
opTable op x y = error ("*** " ++ show op ++ "is incompatible with "
                               ++ show x ++ " and " ++ show y)

interp :: Env -> Expr -> Value
interp env (NumE n) = NumV n
interp env TrueE = BoolV True
interp env FalseE = BoolV False
interp env (Op op e1 e2) =
     let v1 = interp env e1 
         v2 = interp env e2 
     in opTable op v1 v2
interp env (If e1 e2 e3) =
  let v1 = interp env e1 
  in case v1 of
    BoolV True -> interp env e2
    BoolV False -> interp env e3
    _ -> error "condition must be a boolean"
interp env (FnDef params body) = FnV params body env
interp env (FnApp f args) = 
  let fn = interp env f
      argValues = map (interp env) args
  in case fn of
    FnV params body closure ->
      let newEnv = zip params argValues ++ closure
      in interp newEnv body
    _ -> error "application of non-function"
interp env (Id id) = 
  case lookup id env of
    Just v -> v 
    Nothing ->error ("Undefined variable" ++ id) 


