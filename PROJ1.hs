import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Concurrent
import Control.Monad
import System.IO

res allHash tasker = filter (\x ->(hash x) `elem` allHash)  tasker

hash :: String -> String
hash = show.(MD5.md5).(BLU.fromString)

workerLoop :: MVar [String] -> MVar [String] ->[String] -> MVar Int -> Int -> IO ()
workerLoop taskQueue resultQueue hashList countOfTask  0 = do
  putStrLn "!!!one core completed work!!!"
  return ()
workerLoop taskQueue resultQueue hashList countOfTask cou =  do
  maybeTask <- (modifyMVar taskQueue  (\q -> return $ case q of
                                                                                                    [] -> ([], Nothing)
                                                                                                    xs -> (drop 1 xs, Just (take 1 xs)))) --колебания: 1 - 16 сек 100+ 10 сек
                                                                                                    --также, чем больше, тем больше съест оперативы
                                                                                                    --при слишком малом числе тратим время на рекурсивный вызов и проверки кейсов
                                                                                                    --при большом числе больше охват, но и дольше время выполнения (250000 - 400МБ оперативы)
  case maybeTask of
    Nothing -> do
      let str = "Can't found something... =("
      putStrLn str
      return ()
    Just task -> do
      let resus = res hashList task
      case resus of
        [] -> do
          trya <- readMVar countOfTask
          workerLoop taskQueue resultQueue hashList countOfTask trya
        smth -> do
          putStrLn ("Your password is: "++(show smth))
          modifyMVar_ countOfTask (\q -> return $ q-length(smth))
          trya <- readMVar countOfTask
          workerLoop taskQueue resultQueue hashList countOfTask (trya)

main :: IO ()
main = do
      let hashh context = lines context
          alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
          allPasswords = mapM (const alphabet) [1..1] ++ mapM (const alphabet) [1..2]  ++ mapM (const alphabet) [1..3]  ++ mapM (const alphabet) [1..4]  ++ mapM (const alphabet) [1..5]

      hFile <- openFile "input.txt" ReadMode
      contentFromFile <- hGetContents hFile
      workerNumber <- getNumCapabilities
      putStrLn ("!WARNING! Please wait the end of " ++ (show workerNumber) ++ " processes !WARNING!")
      taskQueue <- newMVar allPasswords
      resultQueue <- newMVar []
      countOfTaskMVAR <- newMVar (length (hashh contentFromFile))
      countOfTask <- readMVar countOfTaskMVAR
      workerNumber `replicateM_` forkIO (workerLoop taskQueue resultQueue (hashh contentFromFile)  countOfTaskMVAR countOfTask)
      time <- getChar
      hClose hFile
      return()
