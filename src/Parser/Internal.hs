{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}

-- | This module defines the parsers for all AST nodes.

module Parser.Internal
  ( parseFile
  , model

  , globalDef
  , constantDef
  , constantType
  , formulaDef
  , labelDef
  , moduleDef

  , moduleBody
  , moduleRenaming

  , varDecl
  , varType

  , stmt
  , update
  , assign
  , expr

  , reservedNames
  , reservedOpNames
  ) where

import Control.Lens hiding ( assign )

import Data.Text.Lazy ( Text, pack )
import Data.Text.Lens

import Text.Parsec hiding ( Reply(..) )
import Text.Parsec.Error ( errorMessages, showErrorMessages )
import Text.Parsec.Expr
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Token as T

import Error
import SrcLoc
import Syntax

parseFile :: MonadError Error m => Parser a -> SourceName -> Text -> m a
parseFile p src input = case parse (whiteSpace *> p <* eof) src input of
    Right x  -> return x
    Left err -> throwError (toError err)

-- | Converts a 'ParseError' to an 'Error'.
toError :: ParseError -> Error
toError = Error <$> toSrcLoc . errorPos <*> toSyntaxError

-- | Converts Parsec's 'SourcePos' to a 'SrcLoc'.
toSrcLoc :: SourcePos -> SrcLoc
toSrcLoc = SrcLoc <$> pack . sourceName <*> sourceLine <*> sourceColumn

-- | Converts Parsec's error message to an 'ErrorDesc'.
toSyntaxError :: ParseError -> ErrorDesc
toSyntaxError = SyntaxError . pack .
    showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEOF .
    errorMessages
  where
    msgOr         = "or"
    msgUnknown    = "unkown parse error"
    msgExpecting  = "expecting"
    msgUnExpected = "unexpected"
    msgEOF        = "end of input"

reservedNames, reservedOpNames :: [String]
reservedNames =
    [ "mdp", "dtmc", "ctmc", "module", "endmodule", "global", "const"
    , "formula", "label", "rewards", "endrewards", "init", "bool", "int"
    , "double", "true", "false" , "min", "max"
    ]
reservedOpNames =
    [ "/", "*", "-", "+", "=", "!=", ">", "<", ">=", "<=", "&", "|", "!"
    , "->", ".."
    ]

languageDef :: T.GenLanguageDef Text () Identity
languageDef = T.LanguageDef
    { T.commentStart    = "/*"
    , T.commentEnd      = "*/"
    , T.commentLine     = "//"
    , T.nestedComments  = True
    , T.identStart      = letter <|> char '_'
    , T.identLetter     = alphaNum <|> char '_'
    , T.opStart         = oneOf "/*-+=!><&|.?"
    , T.opLetter        = oneOf "=.>?"
    , T.reservedNames   = reservedNames
    , T.reservedOpNames = reservedOpNames
    , T.caseSensitive   = True
    }

lexer :: T.GenTokenParser Text () Identity
lexer = T.makeTokenParser languageDef

integer :: Integral a => Parser a
integer = fromInteger <$> T.integer lexer

float :: Parser Double
float = T.float lexer

bool :: Parser Bool
bool = False <$ reserved "false"
   <|> True  <$ reserved "true"

identifier :: Parser Text
identifier = pack <$> T.identifier lexer

reserved, reservedOp :: String -> Parser ()
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

semi, colon :: Parser String
semi  = T.semi lexer
colon = T.colon lexer

parens, brackets, doubleQuotes :: forall a. Parser a -> Parser a
parens       = T.parens lexer
brackets     = T.brackets lexer
doubleQuotes = let quote = symbol "\"" in between quote quote

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

-- | The loc combinator annotates the value parsed by @p@ with its location
-- in the source file.
loc :: Parser (SrcLoc -> a) -> Parser a
loc p = do
    pos <- getPosition
    let name   = sourceName pos^.packed
        line   = sourceLine pos
        column = sourceColumn pos
    ($ SrcLoc name line column) <$> p

model :: Parser LModel
model = Model <$> modelType
              <*> many (choice [ moduleDef
                               , globalDef
                               , constantDef
                               , formulaDef
                               , labelDef
                               , rewardsDef
                               ])

modelType :: Parser ModelType
modelType =  MDP  <$ reserved "mdp"
         <|> DTMC <$ reserved "dtmc"
         <|> CTMC <$ reserved "ctmc"
         <?> "automaton type"

globalDef :: Parser LDefinition
globalDef = GlobalDef <$> (reserved "global" *> varDecl)
         <?> "variable declaration"

constantDef :: Parser LDefinition
constantDef = ConstDef <$>
    loc (Constant <$> (reserved "const" *>
                       option IntConstType constantType)
                  <*> (identifier <* reservedOp "=")
                  <*> (expr <* semi)) <?> "constant definition"

constantType :: Parser ConstType
constantType =  BoolConstType   <$ reserved "bool"
            <|> IntConstType    <$ reserved "int"
            <|> DoubleConstType <$ reserved "double"
            <?> "type"

formulaDef :: Parser LDefinition
formulaDef = FormulaDef <$>
    loc (Formula <$> (reserved "formula" *> identifier)
                 <*> (reservedOp "=" *> expr <* semi)) <?> "formula definition"

labelDef :: Parser LDefinition
labelDef = LabelDef <$>
    loc (Label <$> (reserved "label" *> doubleQuotes identifier)
               <*> (reservedOp "=" *> expr <* semi)) <?> "label definition"

rewardsDef :: Parser LDefinition
rewardsDef = RewardsDef <$>
    loc (Rewards <$> (reserved "rewards" *> doubleQuotes identifier)
                 <*> (many reward <* reserved "endrewards"))
                        <?> "reward structure"

reward :: Parser LReward
reward =  loc (Reward <$> action <*> (expr <* colon) <*> (expr <* semi))
      <?> "reward"
  where
    action =
        optionMaybe (brackets identifier)

moduleDef :: Parser LDefinition
moduleDef = do
    reserved "module"
    name <- identifier
    (moduleRenaming name <|> moduleBody name) <* reserved "endmodule"

moduleBody :: Name -> Parser LDefinition
moduleBody name = fmap ModuleDef . loc $
    Module name <$> many varDecl <*> many stmt

moduleRenaming :: Name -> Parser LDefinition
moduleRenaming name = fmap RenameDef . loc $
    Renaming name <$> (reservedOp "=" *> identifier)
                  <*> brackets (commaSep1 renaming)
  where
    renaming = (,) <$> (identifier <* reservedOp "=") <*> identifier

varDecl :: Parser LVarDecl
varDecl = loc (VarDecl <$> identifier <* colon
                       <*> varType
                       <*> optionMaybe (reserved "init" *> expr) <* semi)
       <?> "variable declaration"

varType :: Parser LVarType
varType =  BoolVarType <$ reserved "bool"
       <|> brackets (IntVarType <$> expr <* reservedOp ".." <*> expr)
       <?> "type"

stmt :: Parser LStmt
stmt = loc (Stmt <$> brackets (optionMaybe identifier)
                 <*> expr <* reservedOp "->"
                 <*> updates)
    <?> "statement"
  where
    updates =  [] <$ try (reserved "true" <* semi)
           <|> try (singleUpdate <* semi)
           <|> update `sepBy1` reservedOp "+" <* semi
    singleUpdate = fmap (:[]) . loc $
        Update Nothing <$> assign `sepBy1` reservedOp "&"

update :: Parser LUpdate
update = loc (Update <$> probability <*> assignmentList) <?> "stochastic update"
  where
    probability    = optionMaybe $ try (expr <* colon)
    assignmentList =  [] <$ reserved "true"
                  <|> assign `sepBy1` reservedOp "&"

assign :: Parser LAssign
assign =  loc (parens $ Assign <$> identifier <* symbol "'"
                                              <* reservedOp "="
                               <*> expr)
      <?> "assignment"

expr :: Parser LExpr
expr = do
    e <- simpleExpr
    option e . loc $ CondExpr e <$> (reservedOp "?" *> expr) <*> (colon *> expr)
  where
    simpleExpr = buildExpressionParser exprOpTable term <?> "expression"

term :: Parser LExpr
term =  parens expr
    <|> loc (choice [ BoolExpr <$> bool
                    , DecimalExpr <$> try float
                    , IntegerExpr <$> integer
                    , try (FuncExpr <$> function <*> parens (commaSep1 expr))
                    , IdentExpr <$> identifier
                    ])
    <?> "literal, variable or expression"

function :: Parser Function
function = choice
    [ FuncMin   <$ reserved "min"
    , FuncMax   <$ reserved "max"
    , FuncFloor <$ symbol "floor"
    , FuncCeil  <$ symbol "ceil"
    , FuncPow   <$ symbol "pow"
    , FuncMod   <$ symbol "mod"
    , FuncLog   <$ symbol "log"
    ]

exprOpTable :: OperatorTable Text () Identity LExpr
exprOpTable = -- operators listed in descending precedence, operators in same group have the same precedence
    [ [ unaryOp "-" $ ArithUnOp Neg
      ]
    , [ "*"  --> ArithBinOp Mul
      , "/"  --> ArithBinOp Div
      ]
    , [ "+"  --> ArithBinOp Add
      , "-"  --> ArithBinOp Sub
      ]
    , [ "="  --> EqBinOp Eq
      , "!=" --> EqBinOp Neq
      , ">"  --> RelBinOp Gt
      , "<"  --> RelBinOp Lt
      , ">=" --> RelBinOp Gte
      , "<=" --> RelBinOp Lte
      ]
    , [ unaryOp "!" $ LogicUnOp LNot
      ]
    , [ "&"  --> LogicBinOp LAnd
      ]
    , [ "|"  --> LogicBinOp LOr
      ]
    ]
  where
    unaryOp s     = unary $ reservedOp s
    (-->) s = binary AssocLeft $ reservedOp s

unary :: ParsecT s u m a -> UnOp -> Operator s u m (Expr b)
unary p unOp = Prefix (unaryExpr unOp <$ p)

binary :: Assoc -> ParsecT s u m a -> BinOp -> Operator s u m (Expr b)
binary assoc p binOp = Infix (binaryExpr binOp <$ p) assoc

