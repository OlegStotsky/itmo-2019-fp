import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

fromString :: String -> FilePath
fromString = id

findIn :: String -> FilePath -> MaybeT IO FilePath
findIn fileName path = do prevDir <- liftIO $ getCurrentDirectory
                          liftIO $ setCurrentDirectory path
                          newDir <- liftIO $ getCurrentDirectory
                          contents <- liftIO $ listDirectory newDir
                          if elem (fromString fileName) contents then
                             do liftIO $ setCurrentDirectory prevDir
                                liftIO $ return $ fromString fileName
                          else
                              do dirs <- liftIO $ filterM ((fmap isDirectory) . getFileStatus) contents
                                 result <- msum $ (findIn fileName) <$> dirs
                                 liftIO $ setCurrentDirectory prevDir
                                 return result