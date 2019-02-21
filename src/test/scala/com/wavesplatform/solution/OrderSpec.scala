package com.wavesplatform.solution

import org.scalatest._

class OrderSpec extends FlatSpec {
  "Order" should "no match with other order if action is the same" in {
    val order1 = Order(0, "", Buy, "", 1, 1)
    val order2 = Order(1, "", Buy, "", 1, 1)
    assert(order1.tryMatchWith(order2) == NoMatch)
    assert(order2.tryMatchWith(order1) == NoMatch)
  }

  "Order" should "no match with other order if buy price lower than sell price" in {
    val order1 = Order(0, "", Buy, "", price = 1, amount = 1)
    val order2 = Order(1, "", Sell, "", price = 2, amount = 1)
    assert(order1.tryMatchWith(order2) == NoMatch)
    assert(order2.tryMatchWith(order1) == NoMatch)
  }

  "Order" should "fully match with other order if amount is the same" in {
    val order1 = Order(0, "", Buy, "", price = 2, amount = 1)
    val order2 = Order(1, "", Sell, "", price = 1, amount = 1)
    order1.tryMatchWith(order2) match {
      case FullMatch(_) => assert(true)
    }
    order2.tryMatchWith(order1) match {
      case FullMatch(_) => assert(true)
    }
  }

  "Order" should "partially match with other order if amount is not the same" in {
    val order1 = Order(0, "", Buy, "", price = 2, amount = 1)
    val order2 = Order(1, "", Sell, "", price = 1, amount = 2)
    order1.tryMatchWith(order2) match {
      case PartialMatch(_, _) => assert(true)
    }
    order2.tryMatchWith(order1) match {
      case PartialMatch(_, _) => assert(true)
    }
  }
}
