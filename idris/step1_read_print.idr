import Types
import Reader

replStep : String -> String
replStep code = case codeToExpr code of
                  Left err => err
                  Right expr => show expr

partial
main : IO ()
main = repl "user> " $ \s => replStep s ++ "\n"
