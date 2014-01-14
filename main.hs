import System.INotify

data SyncFile = SyncFile {
  sfPath :: FilePath
  } deriving Show

class Sync s where
  sync :: s -> IO() -- mettre un functor or monade ?
  delete :: s -> String
  move :: s -> String
  create :: s -> String

instance Sync SyncFile where
  sync s = print "sync action baby" 
  delete s = "del " ++ sfPath s
  move s = "move " ++ sfPath s
  create s = "create " ++ sfPath s

watchMe e = case e of
  (Created isd f) -> print $ create (SyncFile f)
  (MovedIn isd f mc) -> print $ "i detect move in : " ++ f
  (MovedOut isd f mc) -> print $ "i detect move out : " ++ f
  (Deleted isd f) -> print $ "i detect delete action : " ++ f
  (Modified isd f) -> case f of
                           (Just f') -> print $ "i detect modif action : " ++ f'
                           Nothing -> print "err modif action"
  otherwise -> print "other event ..."
  
main = do
  inotify <- initINotify
  wd <- addWatch inotify [Create, Move, Delete, Modify] watchPath watchMe
  putStrLn "-- watch " ++ watchPath
  getLine
  removeWatch wd
  putStrLn "Bye ..."
  where
    watchPath = "/tmp/sb"
