import qualified System.INotify as IN 

data SyncAction = Create | Update | Delete | Move deriving Show

data SyncFile = SyncFile {
  sfPath :: FilePath,
  sfAction :: SyncAction
} deriving Show

class Sync s where
  sync :: s -> IO() -- mettre un functor or monade pour generic sync (ftp, rsync, ws ...) ?
  delete :: s -> String
  move :: s -> String
  create :: s -> String
  modif :: s -> String

instance Sync SyncFile where
  sync (SyncFile p a) = print "sync action baby" 
  delete (SyncFile p a) = "(" ++ show (sfAction (SyncFile p a)) ++ ") - " ++ (sfPath (SyncFile p a))
  move (SyncFile p a) = "(" ++ show (sfAction (SyncFile p a)) ++ ") - " ++ (sfPath (SyncFile p a))
  create (SyncFile p a) = "(" ++ show (sfAction (SyncFile p a)) ++ ") - " ++ (sfPath (SyncFile p a))
  modif (SyncFile p a) = "(" ++ show (sfAction (SyncFile p a)) ++ ") - " ++ (sfPath (SyncFile p a))

watchMe e = case e of
  (IN.Created isd f) -> print $ create (SyncFile f Create)
  (IN.MovedIn isd f mc) -> print $ move (SyncFile f Move)
  (IN.MovedOut isd f mc) -> print $ move (SyncFile f Move)
  (IN.Deleted isd f) -> print $ delete (SyncFile f Delete)
  (IN.Modified isd f) -> case f of
                           (Just f') -> print $ modif (SyncFile f' Update)
                           Nothing -> print "err modif action"
  otherwise -> print "other event ..."
  
main = do
  inotify <- IN.initINotify
  wd <- IN.addWatch inotify [IN.Create, IN.Move, IN.Delete, IN.Modify] watchPath watchMe
  putStrLn $ "-- watch " ++ watchPath
  getLine
  IN.removeWatch wd
  putStrLn $ "-- Bye ..." ++ watchPath
  where
    watchPath = "/tmp/sb"
