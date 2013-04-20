{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Main where


import           Crackle.Types
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import           Snap.Core
import           Snap.Iteratee
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, ask)
import           Crackle.Command
import           Crackle.Command.Types
import           Crackle.Handler 
import           Crackle.Shards.DefaultApplication
import           Control.Monad.State.Strict


data MainServer = MainServer | MainServerInit
    deriving (Show)

data SubServer = SubServer | SubServerInit
    deriving (Show)

--------

class HasShard a SubServer => HasSubServer a where
    getSubServer :: a -> SubServer

instance HasShard a SubServer => HasSubServer a where
    getSubServer = getShard

-------

printConfigCommand :: (Show a, HasBase a, HasSubServer a) => Command a
printConfigCommand = Command {
        name    = "run",
        help    = Nothing,
        options = [],
        command = (\optDict -> do
                app <- ask
                liftIO $ print $ getConfig app
                put $ setShard app BaseApplicationInit
                app2 <- get
                liftIO $ print $ app2
            )
    }

instance (Show b, HasBase b, HasSubServer b) => Shard MainServer b where
    urlPrefix _   = Just "/m"
    routes    _   = [packView IndexView]
    commands  _   = [printConfigCommand]
    runConfig _ _ = MainServer
    
data IndexView = IndexView
instance HasSubServer b => ShardView IndexView b where
    url        _ = "/c"
    runRequest _ = do
            appState <- asks getSubServer
            liftIO $ print $ appState
            writeBS "HelloWorld!"

------

instance Shard SubServer b where
    urlPrefix _   = Nothing
    routes    _   = []
    commands  _   = []
    runConfig _ _ = SubServer

-------

myApplication :: (BaseApplication :$: (MainServer :$: SubServer) )
myApplication =  (BaseApplicationInit :$: (MainServerInit :$: SubServerInit))

--------

myConfig :: CrackleConfig
myConfig = M.fromList [("SOMECONFIG", "OTHERCONFIG")]

main :: IO ()
main = do
    let (app, commands) = initApplication myConfig myApplication 
    (_, s) <- runCrackle (runCommands $ commands) app
    return ()
    
    