{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The Ast module defines the abstract syntax tree of SPSL.

module Syntax
  ( module Syntax.Operators

  , Name

  , HasExprs(..)
  , HasIdents(..)

  , Model(..)
  , LModel
  , ModelType(..)
  , Definition(..)
  , LDefinition
  , _ModuleDef
  , _RenameDef
  , _GlobalDef
  , _ConstDef
  , _FormulaDef
  , _RewardsDef
  , _LabelDef
  , _InitDef
  , Module(..)
  , LModule
  , Renaming(..)
  , LRenaming
  , VarDecl(..)
  , LVarDecl
  , VarType(..)
  , LVarType
  , Constant(..)
  , LConstant
  , ConstType(..)
  , Formula(..)
  , LFormula
  , Label(..)
  , LLabel
  , Rewards(..)
  , LRewards
  , Reward(..)
  , LReward
  , Init(..)
  , LInit
  , Stmt(..)
  , LStmt
  , Probability
  , Update(..)
  , LUpdate
  , Assign(..)
  , LAssign
  , Expr(..)
  , LExpr
  , Function(..)

  , exprAnnot

  , _IdentExpr

  , binaryExpr
  , unaryExpr
  , intExpr
  , (?), ThenElse(..)
  , eq, neq, gt, lt, gte, lte, land
  ) where

import Control.Lens

import Data.String
import Data.Text.Lazy ( Text, pack )

import Text.PrettyPrint.Leijen.Text hiding ( (<$>) )

import SrcLoc

import Syntax.Operators

-- | An identifier name.
type Name = Text

-- | Any AST node @n@ that 'HasExprs' provides a 'Traversal' of all
-- 'Expr's contained in that node and all nodes below.
class HasExprs n where
    exprs :: Traversal' (n a) (Expr a)

-- | Any AST node @n@ that 'HasIdents' provides a 'Traversal' of all
-- identifiers contained in that node and all nodes below.
class HasIdents n where
    idents :: Traversal' n Name

data Model a = Model !ModelType [Definition a] deriving (Eq, Functor, Show)

type LModel = Model SrcLoc

instance HasExprs Model where
    exprs f (Model modelT defs) = Model modelT <$> traverse (exprs f) defs

-- | The model type.
data ModelType = MDP | DTMC | CTMC deriving (Eq, Show)

data Definition a
  = ModuleDef   (Module a)
  | RenameDef   (Renaming a)
  | GlobalDef   (VarDecl a)
  | ConstDef    (Constant a)
  | FormulaDef  (Formula a)
  | LabelDef    (Label a)
  | RewardsDef  (Rewards a)
  | InitDef       (Init a)
  deriving (Eq, Functor, Show)

type LDefinition = Definition SrcLoc

instance HasExprs Definition where
    exprs f def = case def of
        ModuleDef   m -> ModuleDef   <$> exprs f m
        RenameDef   r -> pure $ RenameDef r
        GlobalDef   g -> GlobalDef   <$> exprs f g
        ConstDef    c -> ConstDef    <$> exprs f c
        FormulaDef  d -> pure $ FormulaDef d
        LabelDef    l -> LabelDef    <$> exprs f l
        RewardsDef  r -> RewardsDef  <$> exprs f r
        InitDef     i -> InitDef     <$> exprs f i

data Module a = Module
  { modName  :: !Name
  , modVars  :: [VarDecl a]
  , modStmts :: [Stmt a]
  , modAnnot :: !a
  } deriving (Eq, Functor, Show)

type LModule = Module SrcLoc

instance HasExprs Module where
    exprs f (Module name decls stmts a) =
        Module name <$> traverse (exprs f) decls
                    <*> traverse (exprs f) stmts
                    <*> pure a

instance HasIdents (Module a) where
    idents f (Module name decls stmts a) =
        Module name <$> traverse (idents f) decls
                    <*> traverse (idents f) stmts
                    <*> pure a

data Renaming a = Renaming
  { rnName      :: !Name
  , rnSource    :: !Name
  , rnRenamings :: [(Name, Name)]
  , rnAnnot     :: !a
  } deriving (Eq, Functor, Show)

type LRenaming = Renaming SrcLoc

data VarDecl a = VarDecl
  { declName   :: !Name
  , declType   :: !(VarType a)
  , declInit   :: Maybe (Expr a)
  , declAnnot  :: !a
  } deriving (Eq, Functor, Show)

type LVarDecl = VarDecl SrcLoc

instance HasExprs VarDecl where
    exprs f (VarDecl name t e a) =
        VarDecl name t <$> traverse (exprs f) e <*> pure a

instance HasIdents (VarDecl a) where
    idents f (VarDecl name t e a) =
        VarDecl <$> f name <*> idents f t <*> traverse (idents f) e <*> pure a

data VarType a
  = BoolVarType
  | IntVarType (Expr a) (Expr a)
  deriving (Eq, Functor, Show)

type LVarType = VarType SrcLoc

instance HasIdents (VarType a) where
    idents f vt = case vt of
        BoolVarType            -> pure BoolVarType
        IntVarType lower upper ->
            IntVarType <$> idents f lower
                       <*> idents f upper

data Constant a = Constant
  { constType  :: !ConstType
  , constName  :: !Name
  , constValue :: Maybe (Expr a)
  , constAnnot :: !a
  } deriving (Eq, Functor, Show)

type LConstant = Constant SrcLoc

instance HasExprs Constant where
    exprs f (Constant ct name e a) =
        Constant ct name <$> traverse f e <*> pure a

data ConstType
  = BoolConstType
  | IntConstType
  | DoubleConstType
  deriving (Eq, Show)

data Formula a = Formula
  { frmName  :: !Name
  , frmExpr  :: Expr a
  , frmAnnot :: !a
  } deriving (Eq, Functor, Show)

type LFormula = Formula SrcLoc

data Label a = Label
  { lblName  :: !Name
  , lblExpr  :: Expr a
  , lblAnnot :: !a
  } deriving (Eq, Functor, Show)

type LLabel = Label SrcLoc

instance HasExprs Label where
    exprs f (Label name e a) = Label name <$> f e <*> pure a

data Rewards a = Rewards
  { rwsName    :: Maybe Name
  , rwsRewards :: [Reward a]
  , rwsAnnot   :: !a
  } deriving (Eq, Functor, Show)

type LRewards = Rewards SrcLoc

instance HasExprs Rewards where
    exprs f (Rewards name rws a) =
        Rewards name <$> traverse (exprs f) rws <*> pure a

data Reward a = Reward
  { rwAction :: Maybe (Maybe Name)
  , rwGuard  :: Expr a
  , rwReward :: Expr a
  , rwAnnot  :: !a
  } deriving (Eq, Functor, Show)

type LReward = Reward SrcLoc

instance HasExprs Reward where
    exprs f (Reward name g e a) = Reward name <$> f g <*> f e <*> pure a

data Init a = Init
  { initExpr  :: Expr a
  , initAnnot :: !a
  } deriving (Eq, Functor, Show)

type LInit = Init SrcLoc

instance HasExprs Init where
    exprs f (Init e a) = Init <$> f e <*> pure a

data Stmt a = Stmt
  { stmtAction :: Maybe Name
  , stmtGuard  :: Expr a
  , stmtUpdate :: [Update a]
  , stmtAnnot  :: !a
  } deriving (Eq, Functor, Show)

type LStmt = Stmt SrcLoc

instance HasExprs Stmt where
    exprs f (Stmt action grd upds a) =
        Stmt action <$> f grd
                    <*> traverse (exprs f) upds
                    <*> pure a

instance HasIdents (Stmt a) where
    idents f (Stmt action grd upds a) =
        Stmt <$> traverse f action
             <*> idents f grd
             <*> traverse (idents f) upds
             <*> pure a

type Probability a = Maybe (Expr a)

data Update a = Update
  { updProb   :: Probability a
  , updAssign :: [Assign a]
  , updAnnot  :: !a
  } deriving (Eq, Functor, Show)

type LUpdate = Update SrcLoc

instance HasExprs Update where
    exprs f (Update prob asgns a) =
        Update <$> traverse f prob <*> traverse (exprs f) asgns <*> pure a

instance HasIdents (Update a) where
    idents f (Update prob asgns a) =
        Update <$> traverse (idents f) prob
               <*> traverse (idents f) asgns
               <*> pure a

data Assign a = Assign
  { asVar   :: !Name
  , asExpr  :: Expr a
  , asAnnot :: !a
  } deriving (Eq, Functor, Show)

type LAssign = Assign SrcLoc

instance HasExprs Assign where
    exprs f (Assign name e a) = Assign name <$> f e <*> pure a

instance HasIdents (Assign a) where
    idents f (Assign name e a) = Assign <$> f name <*> idents f e <*> pure a

data Expr a
  = BinaryExpr !BinOp (Expr a) (Expr a) !a
  | UnaryExpr !UnOp (Expr a) !a
  | FuncExpr !Function [Expr a] !a
  | IdentExpr !Name !a
  | DecimalExpr !Double !a
  | IntegerExpr !Integer !a
  | BoolExpr !Bool !a
  | CondExpr (Expr a) (Expr a) (Expr a) !a
  deriving (Eq, Functor, Show)

type LExpr = Expr SrcLoc

instance Num (Expr ()) where
    (IntegerExpr 0 _) + e = e
    e + (IntegerExpr 0 _) = e
    (DecimalExpr 0 _) + e = e
    e + (DecimalExpr 0 _) = e
    lhs + rhs             = binaryExpr (ArithBinOp Add) lhs rhs

    (IntegerExpr 1 _) * e = e
    e * (IntegerExpr 1 _) = e
    (DecimalExpr 1 _) * e = e
    e * (DecimalExpr 1 _) = e
    (IntegerExpr 0 _) * _ = 0
    _ * (IntegerExpr 0 _) = 0
    (DecimalExpr 0 _) * _ = DecimalExpr 0.0 ()
    _ * (DecimalExpr 0 _) = DecimalExpr 0.0 ()
    lhs * rhs             = binaryExpr (ArithBinOp Mul) lhs rhs

    e - (IntegerExpr 0 _) = e
    (IntegerExpr 0 _) - e = negate e
    e - (DecimalExpr 0 _) = e
    (DecimalExpr 0 _) - e = negate e
    lhs - rhs             = binaryExpr (ArithBinOp Sub) lhs rhs

    negate (IntegerExpr i _) = IntegerExpr (negate i) ()
    negate (DecimalExpr d _) = DecimalExpr (negate d) ()
    negate e                 = unaryExpr (ArithUnOp Neg) e

    abs e = e `lt` 0 ? negate e :? e

    signum e = e `lt` 0 ? -1 :? (e `gt` 0 ? 1 :? 0)

    fromInteger = intExpr

instance IsString (Expr ()) where
    fromString = flip IdentExpr () . pack

instance Plated (Expr a) where
    plate f e = case e of
        BinaryExpr binOp lhs rhs a ->
            BinaryExpr binOp <$> f lhs <*> f rhs <*> pure a
        UnaryExpr unOp e' a        -> UnaryExpr unOp <$> f e' <*> pure a
        FuncExpr func args a       ->
            FuncExpr func <$> traverse f args <*> pure a
        CondExpr cond te ee a      ->
            CondExpr <$> f cond <*> f te <*> f ee <*> pure a
        _                          -> pure e

instance HasExprs Expr where -- this instance is actually unnecessary but added for completeness
    exprs f = f

instance HasIdents (Expr a) where
    idents f (IdentExpr name a) = IdentExpr <$> f name <*> pure a
    idents f e                  = plate (idents f) e

data Function
  = FuncMin
  | FuncMax
  | FuncFloor
  | FuncCeil
  | FuncPow
  | FuncMod
  | FuncLog
  deriving (Eq, Show)

exprAnnot :: Expr a -> a
exprAnnot e = case e of
    BinaryExpr _ _ _ a -> a
    UnaryExpr _ _ a    -> a
    FuncExpr _ _ a     -> a
    IdentExpr _ a      -> a
    DecimalExpr _ a    -> a
    IntegerExpr _ a    -> a
    BoolExpr _ a       -> a
    CondExpr _ _ _ a   -> a

-- | This 'Prism' provides a 'Traversal' for 'IdentExpr's.
_IdentExpr :: Prism' (Expr a) (Name, a)
_IdentExpr = prism' (uncurry IdentExpr) f
  where
    f (IdentExpr name a) = Just (name, a)
    f _                  = Nothing

-- | Smart constructor for 'BinaryExpr' which attaches the annotation of
-- the left inner expression @l@ to the newly created expression.
binaryExpr :: BinOp -> Expr a -> Expr a -> Expr a
binaryExpr binOp lhs rhs = BinaryExpr binOp lhs rhs (exprAnnot lhs)

-- | Smart constructor for 'UnaryExpr' which attaches the annotation of the
-- inner expression @e@ to the newly created expression.
unaryExpr :: UnOp -> Expr a -> Expr a
unaryExpr unOp e = UnaryExpr unOp e (exprAnnot e)

-- | Generates an 'IntegerExpr'.
intExpr :: Integer -> Expr ()
intExpr = flip IntegerExpr ()

infixl 0 ?
infixl 1 :?

data ThenElse a = Expr a :? Expr a

(?) :: Expr a -> ThenElse a -> Expr a
cond ? (te :? ee) = CondExpr cond te ee $ exprAnnot cond

eq, neq, gt, lt, gte, lte :: Expr a -> Expr a -> Expr a
eq   = binaryExpr (EqBinOp Eq)
neq  = binaryExpr (EqBinOp Neq)
gt   = binaryExpr (RelBinOp Gt)
lt   = binaryExpr (RelBinOp Lt)
gte  = binaryExpr (RelBinOp Gte)
lte  = binaryExpr (RelBinOp Lte)

land :: Expr a -> Expr a -> Expr a
BoolExpr True _ `land` r  = r
l `land` BoolExpr True _  = l
BoolExpr False a `land` _ = BoolExpr False a
_ `land` BoolExpr False a = BoolExpr False a
l `land` r                = binaryExpr (LogicBinOp LAnd) l r

makePrisms ''Definition

instance Pretty (Model a) where
    pretty (Model modelT defs) =
        pretty modelT <> line <> line <>
        vsep (punctuate line $ map pretty defs) <> line

instance Pretty ModelType where
    pretty modelT = case modelT of
        MDP  -> "mdp"
        DTMC -> "dtmc"
        CTMC -> "ctmc"

instance Pretty (Definition a) where
    pretty def = case def of
        ModuleDef   m -> pretty m
        RenameDef   r -> pretty r
        GlobalDef   g -> "global" <+> pretty g
        ConstDef    c -> pretty c
        FormulaDef  f -> pretty f
        LabelDef    l -> pretty l
        RewardsDef  r -> pretty r
        InitDef     i -> pretty i

instance Pretty (Module a) where
    pretty (Module name decls stmts _) =
        prettyModuleHeader name <> line <>
        indent 4 body <> line <> "endmodule"
      where
        body = vsep (map pretty decls) <> line <> line <>
               vsep (map pretty stmts)

instance Pretty (Renaming a) where
    pretty (Renaming name src rns _) =
        prettyModuleHeader name <+> equals <+> text src <+>
        brackets (space <>
                  (sep . punctuate (comma <> space) $ map renaming rns) <>
                  space) <+>
        "endmodule"
      where
        renaming (old, new) = text old <+> equals <+> text new

prettyModuleHeader :: Name -> Doc
prettyModuleHeader name = "module" <+> text name

instance Pretty (VarDecl a) where
    pretty (VarDecl name t e _) =
        text name <+> colon <+> pretty t <>
        maybe empty ((" init" <+>) . pretty) e <> semi

instance Pretty (VarType a) where
    pretty vt = case vt of
        BoolVarType            -> "bool"
        IntVarType lower upper ->
            brackets (pretty lower <+> ".." <+> pretty upper)

instance Pretty (Constant a) where
    pretty (Constant ct name val _) =
        "const" <+> pretty ct <+> text name <> prettyVal val <> semi
      where
        prettyVal Nothing  = empty
        prettyVal (Just e) = space <> equals <+> pretty e

instance Pretty ConstType where
    pretty ct = case ct of
        BoolConstType   -> "bool"
        IntConstType    -> "int"
        DoubleConstType -> "double"

instance Pretty (Formula a) where
    pretty (Formula name e _) =
        "formula" <+> text name <+> equals <+> pretty e <> semi

instance Pretty (Label a) where
    pretty (Label name e _) =
        "label" <+> dquotes (text name) <+> equals <+> pretty e <> semi

instance Pretty (Rewards a) where
    pretty (Rewards name rws _) =
        "rewards" <+> maybe empty (dquotes . text) name <> line <>
        indent 4 (vsep (fmap pretty rws)) <> line <>
        "endrewards"

instance Pretty (Reward a) where
    pretty (Reward action g e _) =
        maybe empty (brackets . pretty) action <+>
        pretty g <+> colon <+> pretty e <> semi

instance Pretty (Init a) where
    pretty (Init e _) =
        "init" <> line <> indent 4 (pretty e) <> line <> "endinit"

instance Pretty (Stmt a) where
    pretty (Stmt action grd upds _) =
        brackets (maybe empty text action) <+> pretty grd <+> "->" <+>
        pretty upds <> semi

instance Pretty (Update a) where
    pretty (Update e asgns _) = prob e <> pretty asgns
      where
        prob = maybe empty ((<> colon) . pretty)

    prettyList []   = "true"
    prettyList upds = hsep . punctuate (space <> "+") $ map pretty upds

instance Pretty (Assign a) where
    pretty (Assign name e _) =
            parens (text name <> squote <+> equals <+> pretty e)
    prettyList []    = "true"
    prettyList asgns = hsep . punctuate (space <> "&") $ map pretty asgns

instance Pretty (Expr a) where
    pretty = prettyExpr 0

prettyExpr :: Int -> Expr a -> Doc
prettyExpr prec e = case e of
    BinaryExpr binOp lhs rhs _ ->
        let prec' = binOpPrec binOp
        in  parens' (prec >= prec') $
                prettyExpr prec' lhs <+> pretty binOp <+> prettyExpr prec' rhs
    UnaryExpr unOpT e' _ ->
        let prec' = unOpPrec unOpT
        in parens' (prec >= prec') $ pretty unOpT <> prettyExpr prec' e'
    FuncExpr func args _  ->
        pretty func <> parens (hcat . punctuate comma $ map pretty args)
    IdentExpr v _         -> text v
    DecimalExpr d _       -> double d
    IntegerExpr i _       -> integer i
    BoolExpr True _       -> "true"
    BoolExpr False _      -> "false"
    CondExpr cond te ee _ -> parens' (prec > 0) $
        prettyExpr 1 cond <+> char '?' <+>
        prettyExpr 1 te <+> colon <+> prettyExpr 1 ee
  where
    parens' True  = parens
    parens' False = id

instance Pretty Function where
    pretty func = case func of
        FuncMin   -> "min"
        FuncMax   -> "max"
        FuncFloor -> "floor"
        FuncCeil  -> "ceil"
        FuncPow   -> "pow"
        FuncMod   -> "mod"
        FuncLog   -> "log"

