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

  @tailrec private def tryFillOrders(
    waitingOrders: SortedQueue[Order],
    incoming:      Order,
    failures:      List[Order] = Nil
  ): TradingExchange = {
    if (waitingOrders.isEmpty) {
      updateOrdersAndAdd(waitingOrders, incoming, failures)
    } else {
      val SortedQueue(head, tail) = waitingOrders
      incoming.tryMatchWith(head) match {
        case NoMatch                => updateOrdersAndAdd(waitingOrders, incoming, failures)
        case FullMatch(transaction) =>
          tryApplyTransaction(transaction) match {
            case Success(exchange)        => exchange.updateOrders(tail, incoming, failures)
            case Failure(incoming.action) => updateOrdersAndAdd(waitingOrders, incoming, failures)
            case Failure(_)               => tryFillOrders(tail, incoming, head :: failures)
          }
        case PartialMatch(transaction, residual) =>
          tryApplyTransaction(transaction) match {
            case Success(exchange)        =>
              if (residual.action == incoming.action)
                exchange.tryFillOrders(tail, residual, failures)
              else
                exchange.updateOrders(tail + residual, incoming, failures)
            case Failure(incoming.action) => updateOrdersAndAdd(waitingOrders, incoming, failures)
            case Failure(_)               => tryFillOrders(tail, incoming, head :: failures)
          }
      }
    }
  }

  private def updateOrdersAndAdd(
    restOrders: SortedQueue[Order],
    order:      Order,
    failures:   List[Order] = Nil
  ): TradingExchange = {
    val key = (order.security, order.action)
    val added = orders(key) + order
    val oppositeKey = (order.security, order.action.opposite)
    val savedOrders = failures.foldLeft(restOrders)((queue, order) => queue + order)
    copy(orders = orders
      .updated(key, added)
      .updated(oppositeKey, savedOrders)
    )
  }

  private def updateOrders(
    restOrders: SortedQueue[Order],
    order:      Order,
    failures:   List[Order] = Nil
  ): TradingExchange = {
    val oppositeKey = (order.security, order.action.opposite)
    val savedOrders = failures.foldLeft(restOrders)((queue, order) => queue + order)
    copy(orders = orders.updated(oppositeKey, savedOrders))
  }

  private def tryApplyTransaction(transaction: Transaction): TransactionResult = {
    val Transaction(buyerName, sellerName, price, security, amount) = transaction
    val buyer  = clients(buyerName)
    val seller = clients(sellerName)

    if (buyer.cash < price * amount) Failure(Buy)
    else if (seller.securitiesAmount(security) < amount) Failure(Sell)
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
      Success(exchange)
    }
  }
}
