{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      :  Data.Text.TextZipper.Edit
Copyright   :  (c) Kwang Yul Seo 2016
License     :  BSD-style (see the file LICENSE)

Maintainer  :  kwangyul.seo@gmail.com
Stability   :  experimental
Portability :  portable

This module provides a monadic interface to 'Data.Text.Zipper.TextZipper'.
-}
module Data.Text.Zipper.Edit
  ( Edit
  , EditT
  , execEditT
  , execEdit
  -- * Extraction functions
  , clearZipper
  , getText
  , currentLine
  , cursorPosition
  , lineLengths
  , getLineLimit
  -- * Navigation and editing functions
  , moveCursor
  , insertChar
  , breakLine
  , killToEOL
  , gotoEOL
  , gotoBOL
  , deletePrevChar
  , deleteChar
  , moveRight
  , moveLeft
  , moveUp
  , moveDown
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Text.Zipper (TextZipper)
import qualified Data.Text.Zipper as TZ

-- | An edit monad
type Edit t a = EditT t Identity a

-- | An edit transformer monad
newtype EditT t m a = EditT { unEdit :: StateT (TextZipper t) m a }
  deriving (Functor, Applicative, Monad, MonadState (TextZipper t))

-- | Execute the edit session with the given zipper and return the
-- modified zipper.
execEditT :: (Monoid t, Monad m) => EditT t m a -> TextZipper t -> m (TextZipper t)
execEditT e tz = execStateT (unEdit e) tz

-- | Execute the edit session with the given zipper and return the
-- modified zipper.
execEdit :: (Monoid t) => Edit t a -> TextZipper t -> TextZipper t
execEdit e tz = runIdentity (execEditT e tz)

-- | Empty a zipper.
clearZipper :: (Monoid t, Monad m) => EditT t m ()
clearZipper = do
  z <- get
  put (TZ.clearZipper z)

-- | Get the text contents of the zipper.
getText :: (Monoid t, Monad m) => EditT t m [t]
getText = get >>= return . TZ.getText

-- | The line of text on which the zipper's cursor currently resides.
currentLine :: (Monoid t, Monad m) => EditT t m t
currentLine = get >>= return . TZ.currentLine

-- | Get the cursor position of the zipper; returns @(row, col)@.
-- @row@ ranges from @[0..num_rows-1]@ inclusive; @col@ ranges from
-- @[0..length of current line]@ inclusive.  Column values equal to
-- line width indicate a cursor that is just past the end of a line of
-- text.
cursorPosition :: (Monoid t, Monad m) => EditT t m (Int, Int)
cursorPosition = get >>= return . TZ.cursorPosition

-- | Return the lengths of the lines in the zipper.
lineLengths :: (Monoid t, Monad m) => EditT t m [Int]
lineLengths = get >>= return . TZ.lineLengths

-- | Get the line limit, if any, for a zipper.
getLineLimit :: (Monoid t, Monad m) => EditT t m (Maybe Int)
getLineLimit = get >>= return . TZ.getLineLimit

-- | Move the cursor to the specified row and column.  Invalid cursor
-- positions will be ignored.  Valid cursor positions range as
-- described for 'cursorPosition'.
moveCursor ::  (Monoid t, Monad m) => (Int, Int) -> EditT t m ()
moveCursor c = do
  z <- get
  put (TZ.moveCursor c z)

-- | Insert a character at the current cursor position.  Move the
-- cursor one position to the right.
insertChar :: (Monoid t, Monad m) => Char -> EditT t m ()
insertChar ch = do
  z <- get
  put (TZ.insertChar ch z)

-- |Insert a line break at the current cursor position.
breakLine :: (Monoid t, Monad m) => EditT t m ()
breakLine = do
  z <- get
  put (TZ.breakLine z)

-- | Remove all text from the cursor position to the end of the current
-- line.  If the cursor is at the beginning of a line and the line is
-- empty, the entire line will be removed.
killToEOL :: (Monoid t, Monad m) => EditT t m ()
killToEOL = do
  z <- get
  put (TZ.killToEOL z)

-- | Move the cursor to the end of the current line.
gotoEOL :: (Monoid t, Monad m) => EditT t m ()
gotoEOL = do
  z <- get
  put (TZ.gotoEOL z)

-- | Move the cursor to the beginning of the current line.
gotoBOL :: (Monoid t, Monad m) => EditT t m ()
gotoBOL = do
  z <- get
  put (TZ.gotoBOL z)

-- | Delete the character preceding the cursor position, and move the
-- cursor backwards by one character.
deletePrevChar :: (Eq t, Monoid t, Monad m) => EditT t m ()
deletePrevChar = do
  z <- get
  put (TZ.deletePrevChar z)

-- | Delete the character at the cursor position.  Leaves the cursor
-- position unchanged.  If the cursor is at the end of a line of text,
-- this combines the line with the line below.
deleteChar :: (Monoid t, Monad m) => EditT t m ()
deleteChar = do
  z <- get
  put (TZ.deleteChar z)

-- | Move the cursor right by one position.  If the cursor is at the
-- end of a line, the cursor is moved to the first position of the
-- following line (if any).
moveRight :: (Monoid t, Monad m) => EditT t m ()
moveRight = do
  z <- get
  put (TZ.moveRight z)

-- | Move the cursor left by one position.  If the cursor is at the
-- beginning of a line, the cursor is moved to the last position of
-- the preceding line (if any).
moveLeft :: (Monoid t, Monad m) => EditT t m ()
moveLeft = do
  z <- get
  put (TZ.moveLeft z)

-- | Move the cursor up by one row.  If there no are rows above the
-- current one, move to the first position of the current row.  If the
-- row above is shorter, move to the end of that row.
moveUp :: (Monoid t, Monad m) => EditT t m ()
moveUp = do
  z <- get
  put (TZ.moveUp z)

-- | Move the cursor down by one row.  If there are no rows below the
-- current one, move to the last position of the current row.  If the
-- row below is shorter, move to the end of that row.
moveDown :: (Monoid t, Monad m) => EditT t m ()
moveDown = do
  z <- get
  put (TZ.moveDown z)
