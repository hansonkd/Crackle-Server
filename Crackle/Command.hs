module Crackle.Command where

import           Control.Monad (join)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.List (find)
import           Data.List.Split (splitOn)
import           System.Environment
import           Control.Monad.IO.Class
import           Crackle.Command.Types
import           Crackle.Handler
import           Snap.Core 

lookupArg :: String -> M.Map String (Maybe String) -> Maybe String
lookupArg k dict = join $ M.lookup k dict

processOption :: Command b -> String -> Maybe Option
processOption command base = foundOption
        where cleanedOption  = dropWhile ((==) '-') base
              commandOptions = options command
              foundOption    = case find (\x -> (fromMaybe "" $ short x) == cleanedOption) commandOptions of
                    Just shortOpt -> Just shortOpt
                    Nothing  -> find (\x -> (long x) == cleanedOption) commandOptions

optionSplit :: Command b -> String -> Maybe (String, Maybe String)
optionSplit command arg = case splitOn "=" arg of
        (x:y:[]) -> fmap (\x -> (x, Just y)) (findOption x)
        (x:[])   -> fmap (\x -> (x, Nothing)) (findOption x)
        where findOption o = fmap long (processOption command o)

runCommands :: [Command a] -> CrackleHandler a IO ()
runCommands commands = do
    args <- liftIO getArgs
    let commandString = find (\(x:_) -> x /= '-') args
        selectedCommand = case commandString of
            Just cs -> find (\x -> (name x) == cs) commands
            Nothing -> Nothing
        options = case selectedCommand of
            Just command -> M.fromList $ catMaybes $ map (optionSplit command) (filter (\(x:_) -> x == '-') args)
            Nothing      -> M.empty
    case selectedCommand of
        Just c  -> (command c) options
        Nothing -> return ()