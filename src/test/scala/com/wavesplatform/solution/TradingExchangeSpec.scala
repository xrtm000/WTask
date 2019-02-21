package com.wavesplatform.solution

import org.scalatest.FlatSpec

import scala.collection.immutable.SortedMap

class TradingExchangeSpec extends FlatSpec {
  val clients = SortedMap(
    "C1" -> Client("C1", 1000, Map("A" -> 10, "B" -> 20, "C" -> 0, "D" -> 0)),
    "C2" -> Client("C2", 1000, Map("A" -> 0, "B" -> 0, "C" -> 10, "D" -> 20))
  )
  val exchange = TradingExchange(clients)

  "Exchange" should "fully match opposite orders with same amount" in {
    val order = Order(
      number = 0,
      clientName = "C1",
      action = Buy,
      security = "C",
      price = 50,
      amount = 5
    )
    val exchange2 = exchange.processOrder(order)
    assert(exchange2.orders("C", Buy).head == order)

    val oppositeOrder = Order(
      number = 1,
      clientName = "C2",
      action = Sell,
      security = "C",
      price = 50,
      amount = 5
    )
    val exchange3 = exchange2.processOrder(oppositeOrder)
    assert(exchange3.orders("C", Buy).isEmpty)
    assert(exchange3.orders("C", Sell).isEmpty)
    assert(exchange3.clients == SortedMap(
      "C1" -> Client("C1", 750, Map("A" -> 10, "B" -> 20, "C" -> 5, "D" -> 0)),
      "C2" -> Client("C2", 1250, Map("A" -> 0, "B" -> 0, "C" -> 5, "D" -> 20))
    ))
  }

  "Exchange" should "partially match opposite orders with different amount" in {
    val order = Order(
      number = 0,
      clientName = "C1",
      action = Buy,
      security = "C",
      price = 50,
      amount = 5
    )
    val exchange2 = exchange.processOrder(order)
    assert(exchange2.orders("C", Buy).head == order)

    val oppositeOrder = Order(
      number = 1,
      clientName = "C2",
      action = Sell,
      security = "C",
      price = 50,
      amount = 3
    )
    val exchange3 = exchange2.processOrder(oppositeOrder)

    val residual = order.copy(amount = 2, number = 1)
    assert(exchange3.orders("C", Buy).head == residual)
    assert(exchange3.orders("C", Sell).isEmpty)
    assert(exchange3.clients == SortedMap(
      "C1" -> Client("C1", 850, Map("A" -> 10, "B" -> 20, "C" -> 3, "D" -> 0)),
      "C2" -> Client("C2", 1150, Map("A" -> 0, "B" -> 0, "C" -> 7, "D" -> 20))
    ))
  }

  "Exchange" should "prevent negative cash for buyer" in {
    val order = Order(
      number = 0,
      clientName = "C1",
      action = Buy,
      security = "C",
      price = 500,
      amount = 5
    )
    val exchange2 = exchange.processOrder(order)
    assert(exchange2.orders("C", Buy).head == order)

    val oppositeOrder = Order(
      number = 1,
      clientName = "C2",
      action = Sell,
      security = "C",
      price = 400,
      amount = 3
    )
    val exchange3 = exchange2.processOrder(oppositeOrder)

    assert(exchange3.orders("C", Buy).head == order)
    assert(exchange3.orders("C", Sell).head == oppositeOrder)
    assert(exchange3.clients == clients)
  }

  "Exchange" should "prevent negative security amount for seller" in {
    val order = Order(
      number = 0,
      clientName = "C1",
      action = Buy,
      security = "C",
      price = 50,
      amount = 15
    )
    val exchange2 = exchange.processOrder(order)
    assert(exchange2.orders("C", Buy).head == order)

    val oppositeOrder = Order(
      number = 1,
      clientName = "C2",
      action = Sell,
      security = "C",
      price = 40,
      amount = 12
    )
    val exchange3 = exchange2.processOrder(oppositeOrder)

    assert(exchange3.orders("C", Buy).head == order)
    assert(exchange3.orders("C", Sell).head == oppositeOrder)
    assert(exchange3.clients == clients)
  }
}
