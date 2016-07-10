import Test.Hspec

import Data.Text.Zipper
import qualified Data.Text.Zipper.Edit as Z

tz = stringZipper ["abc", "def"] Nothing

insertXAtTheBeginning = do
  Z.moveCursor (0, 0)
  Z.insertChar 'x'

main :: IO ()
main = hspec $ do
  describe "execEdit" $ do
    it "execute the edit session with the given zipper and return the modified zipper" $ do
      let newTz = Z.execEdit insertXAtTheBeginning tz
      getText newTz `shouldBe` ["xabc", "def"]
