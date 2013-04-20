{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Crackle.Types where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import           Data.Maybe 
import           Crackle.Command.Types
import           Crackle.Handler
import           Snap.Core

import Prelude hiding (catch, (.))

infixr 9 :$:
data a :$: b = a :$: !b
  deriving (Show, Eq, Ord)

------------------------------------------------------------------------------
--- Shards

class Shard a b | a -> b where
    urlPrefix :: a -> Maybe ByteString
    routes    :: a -> [PackedShardView b]
    commands  :: a -> [Command b]
    runConfig :: CrackleConfig -> a -> a
    
class HasShard a b where
    getShard :: a -> b
    setShard :: a -> b -> a

instance HasShard a a where
    getShard = id
    setShard _ v = v

instance (HasShard a a) => HasShard (a :$: b) a where
    getShard (a :$: b) = getShard a
    setShard (a :$: b) v = (setShard a v :$: b)

instance (HasShard b c) => HasShard (a :$: b) c where
    getShard (a :$: b) = getShard b
    setShard (a :$: b) v = (a :$: setShard b v)
    
instance (HasShard (a :$: b) c) => HasShard ((a :$: b) :$: d) c where
    getShard (a :$: b) = getShard a
    setShard (a :$: b) v = (setShard a v :$: b)

class ShardView a b | a -> b where
    url           :: a -> ByteString
    runRequest    :: a -> CrackleHandler b Snap ()
    
data PackedShardView b = forall a . ShardView a b => MkShardView a

packView :: ShardView a b => a -> PackedShardView b
packView = MkShardView

instance ShardView (PackedShardView b) b where
    url (MkShardView a) = url a
    runRequest (MkShardView a) = runRequest a

------------------------------------------------------------------------------
-- Application

type CrackleConfig = M.Map ByteString ByteString

class Application a b | a -> b where
    initConfig   :: CrackleConfig -> a -> a
    initRoutes   :: a -> [(ByteString, CrackleHandler b Snap ())]
    initCommands :: a  -> [Command b]

instance (Shard a b) => Application a b where
    initConfig = runConfig
    initRoutes a = createdRoutes
        where toRoutes :: ShardView a b => a -> (ByteString, CrackleHandler b Snap ())
              toRoutes x = (C.append (fromMaybe "" (urlPrefix a)) (url x) , runRequest x)
              createdRoutes = map toRoutes (routes a)
    initCommands a = commands a
        
instance (Application a c, Application b c) => Application (a :$: b) c where
    initConfig config (a :$: b) = (initConfig config a :$: initConfig config b)
    initRoutes (a :$: b) = (initRoutes a) ++ (initRoutes b)
    initCommands (a :$: b) = (initCommands a) ++ (initCommands b)

initApplication :: Application a a => CrackleConfig -> a -> (a, [Command a])
initApplication conf app = (app', initCommands app')
    where app' = initConfig conf app


    