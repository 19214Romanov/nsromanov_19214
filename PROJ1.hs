import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Concurrent
import Control.Monad
import System.IO
import Data.List.Split
import Data.List

hash :: String -> String
hash = show.(MD5.md5).(BLU.fromString)

res :: [String] ->[String] -> [String]
res allHash tasker = filter (\x ->(hash x) `elem` allHash)  tasker

workerLoop :: [String] ->[String] -> MVar Int -> Int -> IO ()
workerLoop taskQueue hashList countOfTask  0 = do
  putStrLn "!!!one core completed work!!!"
  return ()
workerLoop taskQueue hashList countOfTask cou =  do
  maybeTask <- return (take 10000 taskQueue)
  case maybeTask of
    [] -> do
      putStrLn "Can't found something... =("
      workerLoop taskQueue hashList countOfTask 0
    task -> do
      let resus = res hashList task
      case resus of
        [] -> do
          trya <- readMVar countOfTask
          workerLoop (drop 10000 taskQueue) hashList countOfTask trya
        smth -> do
          putStrLn ("Your password is: "++(show smth))
          modifyMVar_ countOfTask (\q -> return $ q-length(smth))
          trya <- readMVar countOfTask
          workerLoop (drop 10000 taskQueue) hashList countOfTask trya

main :: IO ()
main = do
      let hashh context = lines context
          alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
          allPasswords = mapM (const alphabet) [()] ++ mapM (const alphabet) [(),()]  ++ mapM (const alphabet) [(),(),()]  ++ mapM (const alphabet) [(),(),(),()]  ++ mapM (const alphabet) [(),(),(),(),()]
          splitList :: Int -> [String] -> [[String]]
          splitList n xs = transpose $ chunksOf n xs
          powar :: [[String]] -> MVar Int -> Int ->String -> Int -> Int -> IO ()
          powar (x:xs) countOfTaskMVAR countOfTask contentFromFile num jader | num == jader - 1 = do {forkIO (workerLoop x (hashh contentFromFile)  countOfTaskMVAR countOfTask); return()}
                                                                                                                              | otherwise = do
                                                                                                                                forkIO (workerLoop x (hashh contentFromFile)  countOfTaskMVAR countOfTask)
                                                                                                                                powar xs countOfTaskMVAR countOfTask contentFromFile (num+1) jader

      hFile <- openFile "input.txt" ReadMode
      contentFromFile <- hGetContents hFile
      workerNumber <- getNumCapabilities
      putStrLn ("!WARNING! Please wait the end of " ++ (show workerNumber) ++ " processes !WARNING!")
      countOfTaskMVAR <- newMVar (length (hashh contentFromFile))
      countOfTask <- readMVar countOfTaskMVAR
      powar (splitList (workerNumber) allPasswords) countOfTaskMVAR countOfTask contentFromFile 0 workerNumber
      time <- getChar
      hClose hFile
      return()
