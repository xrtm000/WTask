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
    val order1 = Order(0, "buyer",  Buy, "security", price = 2, amount = 1)
    val order2 = Order(1, "seller", Sell, "security", price = 1, amount = 1)
    order1.tryMatchWith(order2) match {
      case FullMatch(
        Transaction(buyerName, sellerName, price, security, amount)
      ) =>
        assert(buyerName  == order1.clientName)
        assert(sellerName == order2.clientName)
        assert(price      == order1.price)
        assert(security   == order1.security)
        assert(amount     == order1.amount)
    }
    assert(order2.tryMatchWith(order1) == order1.tryMatchWith(order2))
  }

  "Order" should "partially match with other order if amount is not the same" in {
    val smallAmountOrder = Order(0, "", Buy, "", price = 5, amount = 1)
    val largeAmountOrder = Order(1, "", Sell, "", price = 5, amount = 2)

    smallAmountOrder.tryMatchWith(largeAmountOrder) match {
      case PartialMatch(
        Transaction(buyerName, sellerName, price, security, amount),
        Order(number, residualClientName, residualAction, _, residualPrice, residualAmount)
      ) =>
        assert(buyerName          == smallAmountOrder.clientName)
        assert(sellerName         == largeAmountOrder.clientName)
        assert(price              == smallAmountOrder.price)
        assert(security           == smallAmountOrder.security)
        assert(amount             == smallAmountOrder.amount)
        assert(number             == largeAmountOrder.number)
        assert(residualClientName == largeAmountOrder.clientName)
        assert(residualAction     == largeAmountOrder.action)
        assert(residualPrice      == largeAmountOrder.price)
        assert(residualAmount     == largeAmountOrder.amount - smallAmountOrder.amount)
    }
    assert(largeAmountOrder.tryMatchWith(smallAmountOrder) == smallAmountOrder.tryMatchWith(largeAmountOrder))
  }
}
