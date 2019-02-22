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

  "Exchange" should "fill as many orders as possible by incoming order" in {
    val extendedClients = SortedMap(
      "C1" -> Client("C1", 1000, Map("A" -> 10, "B" -> 20, "C" -> 0, "D" -> 0)),
      "C2" -> Client("C2", 1000, Map("A" -> 0, "B" -> 0, "C" -> 10, "D" -> 20)),
      "C3" -> Client("C3", 1000, Map("A" -> 0, "B" -> 0, "C" -> 0, "D" -> 0)),
      "C4" -> Client("C4", 1000, Map("A" -> 0, "B" -> 0, "C" -> 0, "D" -> 0))
    )
    val exchange = TradingExchange(extendedClients)

    val buyOrder1 = Order(
      number = 1,
      clientName = "C2",
      action = Buy,
      security = "A",
      price = 100,
      amount = 3
    )
    val buyOrder2 = Order(
      number = 2,
      clientName = "C3",
      action = Buy,
      security = "A",
      price = 100,
      amount = 2
    )
    val buyOrder3 = Order(
      number = 3,
      clientName = "C4",
      action = Buy,
      security = "A",
      price = 100,
      amount = 4
    )
    val exchange2 = exchange
      .processOrder(buyOrder1)
      .processOrder(buyOrder2)
      .processOrder(buyOrder3)

    assert(exchange2.orders("A", Buy).length == 3)

    val sellOrder = Order(
      number = 4,
      clientName = "C1",
      action = Sell,
      security = "A",
      price = 50,
      amount = 10
    )
    val exchange3 = exchange2.processOrder(sellOrder)

    val residual = Order(
      number = 4,
      clientName = "C1",
      action = Sell,
      security = "A",
      price = 50,
      amount = 1
    )
    val resultClients = SortedMap(
      "C1" -> Client("C1", 1900, Map("A" -> 1, "B" -> 20, "C" -> 0, "D" -> 0)),
      "C2" -> Client("C2",  700, Map("A" -> 3, "B" -> 0, "C" -> 10, "D" -> 20)),
      "C3" -> Client("C3",  800, Map("A" -> 2, "B" -> 0, "C" -> 0, "D" -> 0)),
      "C4" -> Client("C4",  600, Map("A" -> 4, "B" -> 0, "C" -> 0, "D" -> 0))
    )

    assert(exchange3.orders("A", Buy).isEmpty)
    assert(exchange3.orders("A", Sell).head == residual)
    assert(exchange3.clients == resultClients)
  }
}
