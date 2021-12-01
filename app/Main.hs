{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable
import Data.Functor.Identity
import Data.IORef (newIORef)
import Streamly.Internal.Data.List
import Streamly.Prelude (IsStream, MonadAsync, Serial, SerialT)
import qualified Streamly.Prelude as S
import System.Environment (getArgs)
import Control.Concurrent.STM
import Flow
import Control.Monad
import qualified Data.HashSet as Hash
import Data.HashSet (HashSet)
import HPath
import HPath.IO
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Debug.Trace (traceShowM)

main :: IO ()
main = do
  args <- getArgs
  dir <- case args of
        [] -> toAbs pwdPath
        [d] -> do
          e <- HPath.parseAny $ Char8.pack d
          either pure toAbs e
        _ -> error "nearby takes 0 or 1 arguments"

  let files = filesBFS dir
  files |> S.mapM_ (toAbs >=> toFilePath .> Char8.unpack .> putStrLn)

fromChan :: (IsStream t, MonadAsync m) => TChan a -> t m a
fromChan = S.repeatM . liftIO . atomically . readTChan

-- TODO:
  -- don't visit paths we've already seen
  -- allow only list directories
filesBFS :: Path Abs -> Serial (Path Abs)
filesBFS fp = do
  q <- makeQueue
  visited <- liftIO $ newTVarIO mempty
  go visited q
  where
    makeQueue = liftIO do
      q <- newTChanIO
      atomically $ writeTChan q fp
      pure q

    go :: TVar (HashSet ByteString) -> TChan (Path Abs) -> Serial (Path Abs)
    go visited q = fromChan q |> S.mapM \fp -> do
      atomically $ modifyTVar' visited (Hash.insert $ fromAbs fp)
      -- TODO handle broken symbolic links
      -- use doesDirectoryExist to ignore symlinks
      -- isReadable comes first, because doesDirectoryExist can throw
      -- permission denied
      whenM (pure (isNotProc fp) &&^ isReadable fp &&^ doesDirectoryExist fp ) do
        dirContents <- liftIO (getDirsFiles fp)
        let parent = dirname fp
        withoutVisited <- filterM notVisited (parent : dirContents)
        for_ withoutVisited (atomically . writeTChan q)
      pure fp
      where
        isNotProc = (/= [HPath.abs|/proc|])

        notVisited :: Path Abs -> IO Bool
        notVisited fp = do
          visitedSet <- readTVarIO visited
          pure $ not . (fromAbs fp `Hash.member`) $ visitedSet

-- https://hackage.haskell.org/package/ghc-9.2.1/docs/src/GHC.Utils.Monad.html#whenM
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do b <- mb
                    when b thing

(&&^) :: Monad m => m Bool -> m Bool -> m Bool
a &&^ b = do
  a' <- a
  if a' then b else pure False
