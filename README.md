# text-zipper-monad

[![Hackage](https://img.shields.io/hackage/v/text-zipper-monad.svg?style=flat)](https://hackage.haskell.org/package/text-zipper-monad)
[![Build Status](https://travis-ci.org/kseo/text-zipper-monad.svg?branch=master)](https://travis-ci.org/kseo/text-zipper-monad)

text-zipper-monad provides a monadic interface to the text-zipper package.

## Usage

```haskell
import Data.Text.Zipper
import qualified Data.Text.Zipper.Edit as Z

insertXAtTheBeginning = do
  Z.moveCursor (0, 0)
  Z.insertChar 'x'

main = do
  let tz = stringZipper ["abc", "def"] Nothing
      newTz = Z.execEdit insertXAtTheBeginning tz
  putStrLn (unlines (getText newTz))
```

