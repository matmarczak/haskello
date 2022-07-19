module TestCursor where

import qualified Cursor as C
import Data.Maybe (fromJust)
import Test.Hspec

main =
  describe "Cursor" $ do
    let c1 = C.fromList [1 .. 5]
    it "moves one item forwards" $
      shouldBe (fromJust . C.getCurrent . fromJust . C.moveNext $ c1) 2
    it "moves one item backwards" $
      shouldBe (fromJust . C.getCurrent . fromJust . C.movePrev . fromJust . C.moveNext $ c1) 1
    it "fails move on last item" $
      shouldBe (C.moveNext . C.moveLast $ c1) Nothing
    it "fails move on first item" $ shouldBe (C.movePrev c1) Nothing
    it "returns all elements by ordered groups" $
      shouldBe
        (C.allByOrder . fromJust $ (C.moveNext c1) >>= C.moveNext)
        ([1, 2], [3], [4, 5])
