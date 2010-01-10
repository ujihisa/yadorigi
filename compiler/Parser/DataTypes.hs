
module Yadorigi.Parser.DataTypes where

import Data.List

-- Data Types

data Position = Position Int Int

data PlusPos body = PlusPos Position body

data LayoutInfo = LayoutInfo Bool Int


data Literal = LiteralInt Int | LiteralFloat Float | LiteralChar Char | LiteralString String


data PatternMatch = PatternMatch Position PrimPatternMatch

data PrimPatternMatch
    = DCPrimPattern String [String] [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern String [String] PatternMatch PatternMatch {- infix data constructor pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch)
        {- bind pattern (including wild card pattern and as pattern) -}
    | BracketPrimPattern PatternMatch {- Bracket Pattern -}


data LambdaExpr = LambdaExpr Position [PatternMatch] Expr

data LetOne = LetOne PatternMatch Expr

data CaseGuard = CaseGuard Expr Expr

data CasePattern = CasePattern PatternMatch (Either Expr [CaseGuard])

data Expr = Expr Position PrimExpr

data PrimExpr
    = LiteralPrimExpr Literal {- literal expression -}
    | NamePrimExpr String [String] {- name expression -}
    | ApplyFunctionPrimExpr Expr Expr {- apply function expression -}
    | InfixPrimExpr String [String] Expr Expr {- infix expression -}
    | NegativePrimExpr Expr {- negative expression -}
    | BracketPrimExpr Expr {- bracket expression -}
    | ListPrimExpr [Expr] {- list expression -}
    | LambdaPrimExpr [LambdaExpr] {- lambda expression -}
    | LetPrimExpr [LetOne] Expr {- let expression -}
    | IfPrimExpr Expr Expr Expr {- if expression -}
    | CasePrimExpr Expr [CasePattern] {- case Expression -}
    | PrimExprWithDataType Expr DataType {- expression with data type information -}


data DataType
    = DataType Position [TypeClassInfo] PrimDataType

data TypeClassInfo
    = TypeClassInfo String [String]

data PrimDataType
    = VariableType String
    | ComposedDataType String [String] [PrimDataType]
    | ListType PrimDataType
    | FunctionType PrimDataType PrimDataType

-- Output Format

instance (Show body) => Show (PlusPos body) where
    show (PlusPos (Position line column) body) = show line++","++show column++" "++show body

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

instance Show PatternMatch where
    show (PatternMatch pos pattern) = show pattern

instance Show PrimPatternMatch where
    show (DCPrimPattern str [] list) = "("++str++concatMap ((' ':).show) list++")"
    show (DCPrimPattern str modname list) =
        "("++str++"#"++(concat $ intersperse "." modname)++concatMap ((' ':).show) list++")"
    show (LiteralPrimPattern literal) = show literal
    show (DCOpPrimPattern str [] expr1 expr2) = "("++show expr1++" "++str++" "++show expr2++")"
    show (DCOpPrimPattern str modname expr1 expr2) =
        "("++show expr1++" "++str++"#"++(concat $ intersperse "." modname)++" "++show expr2++")"
    show (ListPrimPattern list) = show list
    show (BindPrimPattern str Nothing) = str
    show (BindPrimPattern str (Just pattern)) = str++"@"++show pattern
    show (BracketPrimPattern pattern) = "("++show pattern++")"

instance Show LambdaExpr where
    show (LambdaExpr pos params expr) = (concat $ intersperse " " $ map show params)++" -> "++show expr

instance Show LetOne where
    show (LetOne pattern expr) = show pattern++" = "++show expr

instance Show CaseGuard where
    show (CaseGuard cond expr) = "| "++show cond++" "++show expr

instance Show CasePattern where
    show (CasePattern pattern (Left expr)) = show pattern++" "++show expr
    show (CasePattern pattern (Right list)) = show pattern++" "++show list

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name []) = name
    show (NamePrimExpr name modname) = name++"#"++(concat $ intersperse "." modname)
    show (ApplyFunctionPrimExpr func param) = show func++" "++show param
    show (InfixPrimExpr str [] expr1 expr2) =
        "("++show expr1++" "++str++" "++show expr2++")"
    show (InfixPrimExpr str modname expr1 expr2) =
        "("++show expr1++" "++str++"#"++(concat $ intersperse "." modname)++" "++show expr2++")"
    show (NegativePrimExpr expr) = "-"++show expr
    show (BracketPrimExpr expr) = "("++show expr++")"
    show (ListPrimExpr list) = show list
    show (LambdaPrimExpr list) = "(\\"++(concat $ intersperse " | " $ map show list)++")"
    show (LetPrimExpr list expr) = "{let "++show list++" "++show expr++"}"
    show (IfPrimExpr c t f) = "{if "++show c++" "++show t++" "++show f++"}"
    show (CasePrimExpr expr list) = "{case "++show expr++" "++show list++"}"
    show (PrimExprWithDataType expr dataType) = "("++show expr++"::"++show dataType++")"

instance Show DataType where
    show (DataType _ [] dataType) = show dataType
    show (DataType _ [typeClassInfo] dataType) = show typeClassInfo++" => "++show dataType
    show (DataType _ typeClassInfo dataType) =
        "("++(concat $ intersperse "," $ map show typeClassInfo)++") => "++show dataType

instance Show TypeClassInfo where
    show (TypeClassInfo typeClass typeNames) = typeClass++concat (intersperse " " typeNames)

instance Show PrimDataType where
    show (VariableType str) = str
    show (ComposedDataType str [] []) = str
    show (ComposedDataType str modname []) = str++"#"++(concat $ intersperse "." modname)
    show (ComposedDataType str [] params) = "("++str++concatMap ((' ':).show) params++")"
    show (ComposedDataType str modname params) =
        "("++str++"#"++(concat $ intersperse "." modname)++concatMap ((' ':).show) params++")"
    show (ListType param) = "["++show param++"]"
    show (FunctionType t1 t2) = "("++show t1++" -> "++show t2++")"

-- Composed Data Constructors

literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> String -> [String] -> Expr
nameExpr pos str = Expr pos.NamePrimExpr str

applyFunctionExpr :: Position -> Expr -> Expr -> Expr
applyFunctionExpr pos func = Expr pos.ApplyFunctionPrimExpr func

infixExpr :: Position -> String -> [String] -> Expr -> Expr -> Expr
infixExpr pos str modname expr = Expr pos.InfixPrimExpr str modname expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

bracketExpr :: Position -> Expr -> Expr
bracketExpr pos = Expr pos.BracketPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

lambdaExpr :: Position -> [LambdaExpr] -> Expr
lambdaExpr pos = Expr pos.LambdaPrimExpr

letExpr :: Position -> [LetOne] -> Expr -> Expr
letExpr pos list = Expr pos.LetPrimExpr list

ifExpr :: Position -> Expr -> Expr -> Expr -> Expr
ifExpr pos c t = Expr pos.IfPrimExpr c t

caseExpr :: Position -> Expr -> [CasePattern] -> Expr
caseExpr pos expr = Expr pos.CasePrimExpr expr

exprWithDataType :: Position -> Expr -> DataType -> Expr
exprWithDataType pos expr = Expr pos.PrimExprWithDataType expr


dcPattern :: Position -> String -> [String] -> [PatternMatch] -> PatternMatch
dcPattern pos str modname = PatternMatch pos.DCPrimPattern str modname

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> String -> [String] -> PatternMatch -> PatternMatch -> PatternMatch
dcOpPattern pos str modname pat = PatternMatch pos.DCOpPrimPattern str modname pat

listPattern :: Position -> [PatternMatch] -> PatternMatch
listPattern pos = PatternMatch pos.ListPrimPattern

bindPattern :: Position -> String -> PatternMatch
bindPattern pos str = PatternMatch pos $ BindPrimPattern str Nothing

asPattern :: Position -> String -> PatternMatch -> PatternMatch
asPattern pos str = PatternMatch pos.BindPrimPattern str.Just

bracketPattern :: Position -> PatternMatch -> PatternMatch
bracketPattern pos = PatternMatch pos.BracketPrimPattern

