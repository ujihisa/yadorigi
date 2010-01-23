
module Yadorigi.Parser.DataTypes where

import Text.Parsec

import Data.List

-- Data Types

data Position = Position Int Int deriving Show

data LayoutInfo = LayoutInfo Bool Int

data Literal
    = LiteralInt Int
    | LiteralFloat Float
    | LiteralChar Char
    | LiteralString String deriving Eq

data ScopedName = ScopedName [String] String deriving Eq


data Token' = Token' SourcePos Token

type TokenStream = [Token']

data Token
    = EmptyToken
    | LiteralToken Literal
    | NameToken ScopedName
    | OpToken ScopedName
    | ReservedToken String deriving (Eq,Show)


data PatternMatch = PatternMatch Position PrimPatternMatch

data PrimPatternMatch
    = DCPrimPattern ScopedName [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern ScopedName PatternMatch PatternMatch {- infix data constructor pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch)
        {- bind pattern (including wild card pattern and as pattern) -}
    | BracketPrimPattern PatternMatch {- Bracket Pattern -}
    | PrimPatternWithType PatternMatch DataType  {- pattern with data type information -}


data Guard = Guard Expr Expr

type ExprOrGuard = Either Expr [Guard]

data Lambda = Lambda Position [PatternMatch] Expr

data PrimLet = PrimLet PatternMatch ExprOrGuard

data CasePattern = CasePattern PatternMatch ExprOrGuard

data Expr = Expr Position PrimExpr

data PrimExpr
    = LiteralPrimExpr Literal {- literal expression -}
    | NamePrimExpr ScopedName {- name expression -}
    | ApplyFunctionPrimExpr Expr Expr {- apply function expression -}
    | InfixPrimExpr ScopedName Expr Expr {- infix expression -}
    | NegativePrimExpr Expr {- negative expression -}
    | BracketPrimExpr Expr {- bracket expression -}
    | ListPrimExpr [Expr] {- list expression -}
    | LambdaPrimExpr [Lambda] {- lambda expression -}
    | LetPrimExpr [PrimLet] Expr {- let expression -}
    | IfPrimExpr Expr Expr Expr {- if expression -}
    | CasePrimExpr Expr [CasePattern] {- case Expression -}
    | PrimExprWithType Expr DataType {- expression with data type information -}


data DataType
    = DataType Position [TypeClassInfo] PrimDataType

data TypeClassInfo
    = TypeClassInfo ScopedName PrimDataType

data PrimDataType
    = VariableType String
    | ComposedDataType ScopedName [PrimDataType]
    | ListType PrimDataType
    | FunctionType PrimDataType PrimDataType


data Statement
    = TypeSignatureStatement String DataType
    | BindStatement String [PatternMatch] Expr

-- Output Format

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

instance Show ScopedName where
    show (ScopedName scope name) = concatMap (++".") scope++name

instance Show Token' where
    show (Token' pos t) = show pos++" "++show t

instance Show PatternMatch where
    show (PatternMatch pos pattern) = show pattern

instance Show PrimPatternMatch where
    show (DCPrimPattern name list) = "("++show name++concatMap ((' ':).show) list++")"
    show (LiteralPrimPattern literal) = show literal
    show (DCOpPrimPattern name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (ListPrimPattern list) = show list
    show (BindPrimPattern str Nothing) = str
    show (BindPrimPattern str (Just pattern)) = "("++str++"@"++show pattern++")"
    show (BracketPrimPattern pattern) = "("++show pattern++")"
    show (PrimPatternWithType pattern typeName) = "("++show pattern++show typeName++")"

instance Show Lambda where
    show (Lambda pos params expr) = (concat $ intersperse " " $ map show params)++" -> "++show expr

instance Show PrimLet where
    show (PrimLet pattern expr) = show pattern++" = "++show expr

instance Show Guard where
    show (Guard cond expr) = "| "++show cond++" "++show expr

instance Show CasePattern where
    show (CasePattern pattern (Left expr)) = show pattern++" "++show expr
    show (CasePattern pattern (Right list)) = show pattern++" "++show list

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name) = show name
    show (ApplyFunctionPrimExpr func param) = "("++show func++" "++show param++")"
    show (InfixPrimExpr name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (NegativePrimExpr expr) = "-"++show expr
    show (BracketPrimExpr expr) = "("++show expr++")"
    show (ListPrimExpr list) = show list
    show (LambdaPrimExpr list) = "(\\"++(concat $ intersperse " | " $ map show list)++")"
    show (LetPrimExpr list expr) = "{let "++show list++" "++show expr++"}"
    show (IfPrimExpr c t f) = "{if "++show c++" "++show t++" "++show f++"}"
    show (CasePrimExpr expr list) = "{case "++show expr++" "++show list++"}"
    show (PrimExprWithType expr dataType) = "("++show expr++"::"++show dataType++")"

instance Show DataType where
    show (DataType _ [] dataType) = show dataType
    show (DataType _ [typeClassInfo] dataType) = show typeClassInfo++" => "++show dataType
    show (DataType _ typeClassInfo dataType) =
        "("++(concat $ intersperse "," $ map show typeClassInfo)++") => "++show dataType

instance Show TypeClassInfo where
    show (TypeClassInfo typeClass typeName) = show typeClass++" "++show typeName

instance Show PrimDataType where
    show (VariableType str) = str
    show (ComposedDataType name []) = show name
    show (ComposedDataType name params) = "("++show name++concatMap ((' ':).show) params++")"
    show (ListType param) = "["++show param++"]"
    show (FunctionType t1 t2) = "("++show t1++" -> "++show t2++")"

-- Composed Data Constructors

dcPattern :: Position -> ScopedName -> [PatternMatch] -> PatternMatch
dcPattern pos name = PatternMatch pos.DCPrimPattern name

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> ScopedName -> PatternMatch -> PatternMatch -> PatternMatch
dcOpPattern pos name pat = PatternMatch pos.DCOpPrimPattern name pat

listPattern :: Position -> [PatternMatch] -> PatternMatch
listPattern pos = PatternMatch pos.ListPrimPattern

bindPattern :: Position -> String -> PatternMatch
bindPattern pos str = PatternMatch pos $ BindPrimPattern str Nothing

asPattern :: Position -> String -> PatternMatch -> PatternMatch
asPattern pos str = PatternMatch pos.BindPrimPattern str.Just

bracketPattern :: Position -> PatternMatch -> PatternMatch
bracketPattern pos = PatternMatch pos.BracketPrimPattern

patternWithType :: Position -> PatternMatch -> DataType -> PatternMatch
patternWithType pos pat = PatternMatch pos.PrimPatternWithType pat


literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> ScopedName -> Expr
nameExpr pos = Expr pos.NamePrimExpr

applyFunctionExpr :: Position -> Expr -> Expr -> Expr
applyFunctionExpr pos func = Expr pos.ApplyFunctionPrimExpr func

infixExpr :: Position -> ScopedName -> Expr -> Expr -> Expr
infixExpr pos name expr = Expr pos.InfixPrimExpr name expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

bracketExpr :: Position -> Expr -> Expr
bracketExpr pos = Expr pos.BracketPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

lambdaExpr :: Position -> [Lambda] -> Expr
lambdaExpr pos = Expr pos.LambdaPrimExpr

letExpr :: Position -> [PrimLet] -> Expr -> Expr
letExpr pos list = Expr pos.LetPrimExpr list

ifExpr :: Position -> Expr -> Expr -> Expr -> Expr
ifExpr pos c t = Expr pos.IfPrimExpr c t

caseExpr :: Position -> Expr -> [CasePattern] -> Expr
caseExpr pos expr = Expr pos.CasePrimExpr expr

exprWithType :: Position -> Expr -> DataType -> Expr
exprWithType pos expr = Expr pos.PrimExprWithType expr


