import Data.SortedMap
import Types
import Reader

%default partial

idrToMal : (List Int -> Int) -> MalType
idrToMal f = MFunc $ MInt . f . map unMInt 
  where unMInt : MalType -> Int
        unMInt (MInt n) = n

replEnv : MalEnv
replEnv = fromList [ ("+", idrToMal sum)
                   , ("*", idrToMal product)
                   , ("-", idrToMal $ foldl1 (-))
                   , ("/", idrToMal $ foldl1 div)
                   ]

evalAst : MalEnv -> MalType -> Either String MalType
evalAst env (MSymbol name) = maybeToEither (name ++ " not found\n") $ lookup name env
evalAst env (MList ((MSymbol fname) :: args)) = do
  MFunc fun <- maybeToEither (fname ++ " not found\n") (lookup fname env)
    | _ => Left (fname ++ " is not a function\n")
  case partitionEithers (map (evalAst env) args) of
       ([], args') => Right $ fun args'
       (errs, _) => Left $ unlines errs
evalAst env norm = Right norm

replStep : String -> String
replStep code = fromEither $ do
  ast <- codeToExpr code
  result <- evalAst replEnv ast
  pure $ show result

partial
main : IO ()
main = repl "user> " $ \s => replStep s ++ "\n"
