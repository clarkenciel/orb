{-# language OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Extract.DirTree
  ( dirAt
  , getFiles
  , getFilesRec
  , overNodeContents
  , dirNodeRoot
  , dirNodeContents
  ) where

import System.IO
import System.Directory (listDirectory, getPermissions, searchable)
import System.FilePath ((</>))
import Control.Exception (IOException, catch)
import Control.Monad (mapM)
import Data.Text (Text)
import Data.List (foldl')
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Data.Either (either)

data DTree a
  = DLeaf a
  | DNode { dirNodeRoot :: FilePath
          , dirNodeContents :: [DTree a]
          }
  deriving (Show, Functor, Foldable, Traversable)

dirAt :: FilePath -> [FilePath] -> DTree Text
dirAt n l = DNode n (map (DLeaf . T.pack) l)

getFiles :: FilePath -> IO (DTree Text)
getFiles fp = do
  check <- isFile fp
  if check
    then return $ DLeaf (T.pack fp)
    else dirAt fp <$> listDirectory fp

getFilesRec :: FilePath -> IO (DTree Text)
getFilesRec fp = do  
  check <- isFile fp
  if check
    then return $ DLeaf (T.pack fp)
    else listDirectory fp >>= mapM (getFilesRec . (fp </>)) >>= return . DNode fp

isDirectory, isFile :: FilePath -> IO Bool
isDirectory = fmap searchable . getPermissions
isFile = fmap not . isDirectory

overNodeContents :: (a -> b) -> DTree a -> DTree b
overNodeContents = fmap

overNodeRoot f dt = DNode (f $ dirNodeRoot dt) (dirNodeContents dt)
