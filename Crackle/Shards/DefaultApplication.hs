{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Crackle.Shards.DefaultApplication where


import           Control.Monad.IO.Class (liftIO)

import Crackle.Types
import Crackle.Handler
import Crackle.Command.Types
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Snap.Core
import Snap.Http.Server

data BaseApplication = BaseApplicationInit | BaseApplication { config :: CrackleConfig }
    deriving (Show)

class HasShard a BaseApplication => HasBase a where
    getBase :: a -> BaseApplication
    setBase :: a -> BaseApplication -> a

instance HasShard a BaseApplication => HasBase a where
    getBase = getShard
    setBase = setShard

class HasShard a BaseApplication => HasConfig a where
    getConfig :: a -> CrackleConfig

instance HasShard a BaseApplication => HasConfig a where
    getConfig a = config $ getShard a

instance (Application b b, HasConfig b) => Shard BaseApplication b where
    urlPrefix _   = Nothing
    routes    _   = []
    commands  _   = [runServerCommand]
    runConfig c _ = BaseApplication c

----------------------------------------------------
--- Default Commands

site :: Application a a => a -> CrackleHandler a Snap ()
site app = route $ initRoutes app

runSite :: Application a a => a -> Snap ()
runSite app = do
    (_, _) <- runCrackle (site app) app
    return ()
    
runServerCommand :: (Application a a, HasConfig a) => Command a  
runServerCommand = Command {
        name    = "runserver",
        help    = Nothing,
        options = [],
        command = (\optDict -> do
            app <- ask
            liftIO $ quickHttpServe $ runSite app
            )
    }
    