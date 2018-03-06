module Reader

import Types
import Text.Lexer
import Text.Parser
import Debug.Trace

%access export
%default covering

-- Lexer
data MalTokenKind = Delimiter
                  | ListOpen
                  | ListClose
                  | Identifier
                  | IntLit

Eq MalTokenKind where
  Delimiter == Delimiter = True
  ListOpen == ListOpen = True
  ListClose == ListClose = True
  Identifier == Identifier = True
  IntLit == IntLit = True
  _ == _ = False

public export
MalToken : Type
MalToken = Token MalTokenKind

delimiter : Lexer
delimiter = some $ newline <|> space

listOpen : Lexer
listOpen = is '('

listClose : Lexer
listClose = is ')'

identifier : Lexer
identifier = let nonIdent = delimiter <|> listOpen <|> listClose in
                 expect (non digit) <+> some (non nonIdent)

lexers : List (Lexer, MalTokenKind)
lexers = [ (delimiter, Delimiter)
         , (listOpen, ListOpen)
         , (listClose, ListClose)
         , (intLit, IntLit)
         , (identifier, Identifier)
         ]

export
codeToTokens : String -> (List MalToken, String)
codeToTokens code = let (tokens, _, _, rest) = lex (toTokenMap lexers) code in
                        (map tok tokens, rest)

-- Parser
public export
MalGrammar : Bool -> Type -> Type
MalGrammar = Grammar MalToken

TokenKind MalTokenKind where
  TokType IntLit = MalType
  TokType Identifier = MalType
  -- Dummies
  TokType Delimiter = ()
  TokType ListOpen = ()
  TokType ListClose = ()

  tokValue IntLit n = MInt $ cast n
  tokValue Identifier s = MSymbol s
  tokValue Delimiter _ = ()
  tokValue ListOpen _ = ()
  tokValue ListClose _ = ()

gramInt : MalGrammar True MalType
gramInt = match IntLit

gramSymbol : MalGrammar True MalType
gramSymbol = match Identifier

nextKindIs : MalTokenKind -> MalGrammar True ()
nextKindIs k = terminal $ \(Tok k' _) =>
               if k' == k then Just () else Nothing

gramDelim : MalGrammar False ()
gramDelim = (optional $ nextKindIs Delimiter) *> pure ()

gramLOpen : MalGrammar True ()
gramLOpen = nextKindIs ListOpen

gramLClose : MalGrammar True ()
gramLClose = nextKindIs ListClose

-- `>>=` has to be used, since the two are mutually recursive
-- these must be able to be total...
mutual
  gramList : MalGrammar True MalType
  gramList = map MList $ between gramLOpen gramLClose $ many gramExpr

  gramExpr : MalGrammar True MalType
  gramExpr = gramDelim *> choice [gramInt, gramSymbol, pure () >>= const gramList] <* gramDelim

export
parseMal : List MalToken -> Either (ParseError MalToken) (MalType, List MalToken)
parseMal = parse $ gramExpr

export
codeToExpr : String -> Either String MalType
codeToExpr code = let (tokens, rest) = codeToTokens code in
                      if length rest == 0
                         then case parseMal tokens of
                                   Left (Error msg _) => Left msg
                                   Right (expr, _) => Right expr
                         else Left rest
