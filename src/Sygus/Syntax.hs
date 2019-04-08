module Sygus.Syntax where

data Lit = LitNum Integer 
         | LitDec String
         | LitBool Bool
         | Hexidecimal String
         | Binary String
         | LitStr String deriving (Eq, Show, Read)

type Symbol = String

data Cmd = CheckSynth
         | Constraint Term
         | DeclareVar Symbol Sort
         | InvConstraint Symbol Symbol Symbol Symbol
         | SetFeature Feature Bool
         | SynthFun Symbol [SortedVar] Sort (Maybe GrammarDef)
         | SynthInv Symbol [SortedVar] (Maybe GrammarDef)
         | SmtCmd SmtCmd
         deriving (Eq, Show, Read)

data Identifier = ISymb Symbol
                | Indexed Symbol [Index]
                deriving (Eq, Show, Read)

data Index = IndNumeral Integer
           | IndSymb Symbol
           deriving (Eq, Show, Read)

data Sort = IdentSort Identifier
          | IdentSortSort Identifier [Sort]
          deriving (Eq, Show, Read)

data Term = TermIdent Identifier
          | TermLit Lit
          | TermCall Identifier [Term]
          | TermExists [SortedVar] Term
          | TermForAll [SortedVar] Term
          | TermLet [VarBinding] Term
          deriving (Eq, Show, Read)

data BfTerm = BfIdentifier Identifier
            | BfLiteral Lit
            | BfIdentifierBfs Identifier [BfTerm]
            deriving (Eq, Show, Read)

data SortedVar = SortedVar Symbol Sort deriving (Eq, Show, Read)

data VarBinding = VarBinding Symbol Term deriving (Eq, Show, Read)

data Feature = Grammars
             | FwdDecls
             | Recursion
             deriving (Eq, Show, Read)

data SmtCmd = DeclareDatatype Symbol DTDec
            | DeclareDatatypes [SortDecl] [DTDec]
            | DeclareSort Symbol Integer
            | DefineFun Symbol [SortedVar] Sort Term
            | DefineSort Symbol Sort
            | SetLogic Symbol
            | SetOption Symbol Lit
            deriving (Eq, Show, Read)

data SortDecl = SortDecl Symbol Integer deriving (Eq, Show, Read)

data DTDec = DTDec [DTConsDec] deriving (Eq, Show, Read)

data DTConsDec = DTConsDec Symbol [SortedVar] deriving (Eq, Show, Read)

data GrammarDef = GrammarDef [SortedVar] [GroupedRuleList] deriving (Eq, Show, Read)

data GroupedRuleList = GroupedRuleList Symbol Sort [GTerm] deriving (Eq, Show, Read)

data GTerm = GConstant Sort
           | GVariable Sort
           | GBfTerm BfTerm
           deriving (Eq, Show, Read)
