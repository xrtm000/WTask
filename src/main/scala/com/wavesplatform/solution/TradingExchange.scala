package com.wavesplatform.solution

import com.wavesplatform.solution.MapHelper.Ops

import scala.annotation.tailrec

case class TradingExchange(
  clients: Map[ClientName, Client],
  orders:  Map[(Security, OrderAction), SortedQueue[Order]] = Map().withDefault(k => SortedQueue()(k._2.ordering))
) {
  def processOrder(order: Order): TradingExchange = {
    val oppositeOrders = orders(order.security, order.action.opposite)
    tryFillOrders(oppositeOrders, order)
  }

  @tailrec private def tryFillOrders(waitingOrders: SortedQueue[Order], incoming: Order): TradingExchange = {
    if (waitingOrders.isEmpty) addOrder(incoming)
    else {
      val SortedQueue(head, tail) = waitingOrders
      incoming.tryMatchWith(head) match {
        case NoMatch                             => addOrder(incoming)
        case FullMatch(transaction)              =>
          tryApplyTransaction(transaction) match {
            case Some(exchange) => exchange.removeFilledOrders(tail, incoming)
            case None => tryFillOrders(tail, incoming)
          }
        case PartialMatch(transaction, residual) =>
          tryApplyTransaction(transaction) match {
            case Some(exchange) =>
              if (residual.action == incoming.action) exchange.tryFillOrders(tail, residual)
              else exchange.removeFilledOrders(tail, incoming).addOrder(residual)
            case None => tryFillOrders(tail, incoming)
          }
      }
    }
  }

  private def addOrder(order: Order): TradingExchange = {
    val key = (order.security, order.action)
    val added = orders(key) + order
    copy(orders = orders.updated(key, added))
  }

  private def removeFilledOrders(restOrders: SortedQueue[Order], closed: Order): TradingExchange = {
    val key = (closed.security, closed.action.opposite)
    copy(orders = orders.updated(key, restOrders))
  }

  private def tryApplyTransaction(transaction: Transaction): Option[TradingExchange] = {
    val Transaction(buyerName, sellerName, price, security, amount) = transaction
    val buyer  = clients(buyerName)
    val seller = clients(sellerName)

    if (!checkMinus(buyer, seller, security, price, amount)) None
    else {
      val exchange = copy(
        clients = clients + (
          buyerName -> buyer.copy(
            cash = buyer.cash - price * amount,
            securitiesAmount = buyer.securitiesAmount.adjust(security)(_ + amount)
          ),
          sellerName -> seller.copy(
            cash = seller.cash + price * amount,
            securitiesAmount = seller.securitiesAmount.adjust(security)(_ - amount)
          )
        )
      )
      Some(exchange)
    }
  }

  private def checkMinus(
    buyer:    Client,
    seller:   Client,
    security: Security,
    price:    Int,
    amount:   Int
  ): Boolean = {
    val reducedCash = buyer.cash - price * amount
    val reducedSecurityAmount = seller.securitiesAmount(security) - amount
    reducedCash >= 0 && reducedSecurityAmount >= 0
  }
}
