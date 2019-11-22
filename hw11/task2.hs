import Control.Monad (join)

main :: IO ()
main = join $ putStrLn <$> getLine