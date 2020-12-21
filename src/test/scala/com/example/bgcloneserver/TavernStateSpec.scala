package com.example.bgcloneserver

import com.example.bgcloneserver.PlayerADTS._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TavernStateSpec extends AnyFlatSpec {

  val coins: Coins = Coins(9)
  "coins update" should "be correct" in {
    coins.update(11) shouldEqual Some(Coins(10))
    coins.update(-1) shouldEqual None
    coins.update(7) shouldEqual Some(Coins(7))
  }

  "coins increase" should "be correct" in {
    coins.increase(2) shouldEqual Coins(10)
    coins.increase(1) shouldEqual Coins(10)
    coins.increase(-11) shouldEqual coins
  }

  "coins buy" should "be correct" in {
    coins.buy(Cost(3)) shouldEqual Right(Coins(6))
    coins.buy(Cost(11)) shouldEqual Left("Insufficient funds")
  }

  "hp get demage" should "be correct" in {
    val hp = HP(40)
    hp.getDamage(10) shouldEqual HP(30)
    hp.getDamage(50) shouldEqual HP(-10)
    hp.getDamage(-30) shouldEqual HP(70)
  }

  val level: Level = Level(1)
  "level update " should "be correct" in {
    level.update(-1) shouldEqual None
    level.update(7) shouldEqual None
    level.update(4) shouldEqual Some(Level(4))
  }

  "level increase" should "be correct" in {
    level.increase shouldEqual Level(2)
    Level(6).increase shouldEqual Level(6)
  }

  "level next upgrade base cost" should "be correct" in {
    Level(1).nextUpgradeBaseCost shouldEqual Cost(5)
    Level(2).nextUpgradeBaseCost shouldEqual Cost(7)
    Level(3).nextUpgradeBaseCost shouldEqual Cost(8)
    Level(4).nextUpgradeBaseCost shouldEqual Cost(9)
    Level(5).nextUpgradeBaseCost shouldEqual Cost(10)
    Level(6).nextUpgradeBaseCost shouldEqual Cost(999)
  }

  "cost decrease" should "be correct" in {
    Cost(1).decrease shouldEqual Cost(0)
    Cost(0).decrease shouldEqual Cost(0)
    Cost(10).decrease shouldEqual Cost(9)
  }

}
