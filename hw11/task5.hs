import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          let num = parseArgs args
          case num of 
            Left err -> putStrLn err
            Right x -> mainLoop x

parseArgs :: [String] -> Either String Integer
parseArgs [num] = Right $ read num
parseArgs xs = Left "Only one number should be entered"

mainLoop :: Integer -> IO ()
mainLoop num = do putStrLn "please enter your guess"
                  line <- getLine
                  let guess = read line
                  case guess == num of
                    True -> putStrLn "correct"
                    False -> mainLoop num
