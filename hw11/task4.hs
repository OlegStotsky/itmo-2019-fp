import System.Environment
import System.Directory (listDirectory)

main :: IO ()
main = do args <- getArgs
          let dir = parseArgs args
          directories <- listDirectory dir
          sequence_ $ putStrLn <$> directories

parseArgs :: [String] -> String
parseArgs [targetDir] = targetDir
parseArgs xs = "."
