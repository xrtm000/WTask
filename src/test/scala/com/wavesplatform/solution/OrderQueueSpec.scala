package com.wavesplatform.solution

import org.scalatest.FlatSpec

class OrderQueueSpec extends FlatSpec {
  "OrderQueue" should "store orders with price-time priority" in {
    val buys = SortedQueue()(Buy.ordering) +
      Order(number = 0, "", Buy, "", price = 500, 1) +
      Order(number = 1, "", Buy, "", price = 700, 1) +
      Order(number = 2, "", Buy, "", price = 500, 1) +
      Order(number = 3, "", Buy, "", price = 300, 1)

    assert(buys.head.price == 700)
    assert(buys.tail.head.price == 500)
    assert(buys.tail.head.number == 0)
    assert(buys.tail.tail.head.price == 500)
    assert(buys.tail.tail.head.price == 2)
    assert(buys.tail.tail.tail.head.price == 300)

    val sells = SortedQueue()(Sell.ordering) +
      Order(number = 0, "", Sell, "", price = 500, 1) +
      Order(number = 1, "", Sell, "", price = 700, 1) +
      Order(number = 2, "", Sell, "", price = 500, 1) +
      Order(number = 3, "", Sell, "", price = 300, 1)

    assert(sells.head.price == 300)
    assert(sells.tail.head.price == 500)
    assert(sells.tail.head.number == 0)
    assert(sells.tail.tail.head.price == 500)
    assert(sells.tail.tail.head.price == 2)
    assert(sells.tail.tail.tail.head.price == 700)
  }
}
