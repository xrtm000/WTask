package com.wavesplatform.solution

import org.scalatest.FlatSpec

class OrderQueueSpec extends FlatSpec {
  "OrderQueue" should "store orders with price-time priority" in {
    def createOrders(ordering: Ordering[Order]) =
      SortedQueue()(ordering) +
        Order(number = 0, "", Buy, "", price = 500, 1) +
        Order(number = 1, "", Buy, "", price = 200, 1) +
        Order(number = 2, "", Buy, "", price = 700, 1) +
        Order(number = 3, "", Buy, "", price = 300, 1) +
        Order(number = 4, "", Buy, "", price = 200, 1) +
        Order(number = 5, "", Buy, "", price = 300, 1)

    val buys = createOrders(Buy.ordering)
    assert(buys(0).price == 700)
    assert(buys(1).price == 500)
    assert(buys(2).price == 300)
    assert(buys(2).number == 3)
    assert(buys(3).price == 300)
    assert(buys(3).number == 5)
    assert(buys(4).price == 200)
    assert(buys(4).number == 1)
    assert(buys(5).price == 200)
    assert(buys(5).number == 4)

    val sells = createOrders(Sell.ordering)
    assert(sells(0).price == 200)
    assert(sells(0).number == 1)
    assert(sells(1).price == 200)
    assert(sells(1).number == 4)
    assert(sells(2).price == 300)
    assert(sells(2).number == 3)
    assert(sells(3).price == 300)
    assert(sells(3).number == 5)
    assert(sells(4).price == 500)
    assert(sells(5).price == 700)
  }
}
