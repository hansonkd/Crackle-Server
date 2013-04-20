module Crackle.Command.Types where

import qualified Data.Map as M
import           Crackle.Handler
import           Snap.Core

data Option  = Option {
        short :: Maybe String,
        long  :: String,
        optionHelp  :: Maybe String
        }
        deriving (Show)
        
data Command a = Command {
        name    :: String,
        help    :: Maybe String,
        options :: [Option],
        command :: M.Map String (Maybe String) -> CrackleHandler a IO ()
    }