import Data.Word
import Text.Printf
import qualified Data.Digest.Pure.MD5 as MD5 --библа, которая генерит hash md-5
import qualified Data.ByteString.Lazy.UTF8 as BLU --библа, которая конвертит String в ByteString
import Control.Concurrent
import Control.Monad
import Control.DeepSeq
import System.IO


passwordList :: String -> Int -> [String] --создание паролей длины len (все комбинации)
passwordList charList len = stream beginState
  where
    beginState = replicate len charList --список с несколькими алфавитами (len штук)
    endState = replicate len [ last charList ] --последний пароль (zz)
    nextState ((_:[]):xs) = charList : nextState xs --когда символы в одном наборе кончились, добавляем туда полный начальный набор, забирая из след списка первый символ
    nextState ((_:ys):xs) = ys : xs --"проглатываем" головной символ, если он есть
    nextState x = error ( "nextState " ++ show x)
    stream st = --"поток" создающий список всех паролей
      let pw = map head st in
      if st == endState then [ pw ]
                        else pw : stream (nextState st)

hash :: String -> String --функция получает пароль, генерит от него хеш и возвращает хеш в представлении типа String
hash = show.(MD5.md5).(BLU.fromString)

workerLoop :: MVar [String] -> MVar [ [String] ] -> String -> Int ->[String] -> IO ()
workerLoop taskQueue resultQueue charList pwLen hashList = do
   --обертка, которая безопасно меняет полученный MVar и еще вдобавок возвращает значение в maybeTask
  maybeTask <- (modifyMVar taskQueue  (\q -> return $ case q of
                                                                                                    [] -> ([], Nothing)
                                                                                                    (x:xs) -> (xs, Just x)))
  case maybeTask of
    Nothing -> return ()
    Just task -> do
      let postfixList = passwordList charList (pwLen - length task) --генерит оставшуюся часть паролей
          pwList = map (task ++) postfixList --делаю список паролей (полных)
          pwHashList = [(pw, hash pw) | pw <- pwList] --делаю такой список, вида (пароль, хеш)
          rslt = [pw ++ ":" ++ h | (pw,h) <- pwHashList,  h `elem` hashList] --если хеш равен тому, что валяется в начально хеш листе, то записываю такой хеш и его пароль в resultat
      rslt `deepseq` modifyMVar_ resultQueue (\q -> return $ rslt:q)
      --deepseq (полностью просчитает первый аргумент и только потом будет исполнять второй)
      workerLoop taskQueue resultQueue charList pwLen hashList

mainLoop :: MVar [ [String] ] -> Int -> Int -> IO Int
mainLoop _ 0 _ = return 0  --если нет алфавита, то досвидания
mainLoop _ taskNumber 0 = return taskNumber --если нет алфавита, то досвидания (вернет число не найденных паролей)
mainLoop resultQueue count taskNumber  = do
  results <- (modifyMVar resultQueue (\q -> return ([], q)))
  case results of
    [] -> do
      threadDelay 100000 -- 100 ms ожидание ответа от ядер
      mainLoop resultQueue count taskNumber
    _ -> do
      mapM_ (mapM_ putStrLn) results --вывод (в контексте монады с пропущенным выходным значением map)
      ku <- filterM (\x -> if length x > 0 then return True else return False) results
      mainLoop resultQueue (count - length ku) (taskNumber - length results)


main :: IO ()
main = do
      let hashList = [
            -- 1111
            "b59c67bf196a4758191e42f76670ceba",
            -- r2d2
            "3e0fd1ad8efb39d90b8cd3b04a6c94f1"
            ]
          pwLen = 4 --длина пароля
          chunkLen = 2 -- длина префикса
          charList = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
          taskList = passwordList charList chunkLen --создаст кучу различных комбнаций паролей длины chunkLen
          taskNumber = length taskList --выдаст количество этих паролей

      workerNumber <- getNumCapabilities --возращает число ядер (заданных при запуске -N(X))
      --программа использует две очереди
      --newMVar - что-то вроде создания новой переменно, разделяемой несколькими потоками
      taskQueue <- newMVar taskList --создаем тип [String] (taskList - набор всех паролей длины chunkLen)
      resultQueue <- newMVar [] -- ещё один тип, [[String]] - сюда помещаются те строки, которые мы хотим вывести на экран

      workerNumber `replicateM_` forkIO (workerLoop taskQueue resultQueue charList pwLen hashList) --выполняет действие workerNumber раз, отбрасывая результат
      --workNumber - число параллельно запущенных процессов forkIO
      num <- mainLoop resultQueue (length hashList) taskNumber --вернет число не найденных паролей
      writeFile "output.txt" (show (num))
      return()
