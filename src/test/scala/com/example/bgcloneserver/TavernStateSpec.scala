package com.example.bgcloneserver

import org.scalatest.freespec.AnyFreeSpec
import com.example.bgcloneserver.TavernState._

class TavernStateSpec extends AnyFreeSpec {

  "coins" - {
    "coins cant be updated to more than 10 and less than 0" in {
      val coins = Coins(9)
      assert(coins.update(11).contains(Coins(10)))
      assert(coins.update(-1).isEmpty)
      assert(coins.update(7).contains(Coins(7)))

      assert(coins.increase(2) == Coins(10))
      assert(coins.increase(1) == Coins(10))
      assert(coins.increase(-11) == coins)
    }
  }

}
