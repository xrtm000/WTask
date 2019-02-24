package com.wavesplatform.solution

import com.wavesplatform.solution.MapHelper.Ops

import scala.annotation.tailrec

case class TradingExchange(
  clients: Map[ClientName, Client],
  orders:  Map[(Security, OrderAction), SortedQueue[Order]] = Map().withDefault(k => SortedQueue()(k._2.ordering))
) {
  def processOrder(order: Order): TradingExchange = {
    val oppositeKey = (order.security, order.action.opposite)
    val oppositeOrders = orders(oppositeKey)
    val (status, updatedOrders, OrderFillAccumulator(failures, pendingChanges)) = tryFillOrder(order, oppositeOrders)

    val newOppositeOrders = oppositeKey -> collectOrders(failures, updatedOrders)
    val newOrders = status match {
      case Closed    => orders + newOppositeOrders
      case Postponed(o) => orders.adjust(o.security, o.action)(_ + o) + newOppositeOrders
    }
    copy(applyPendingChanges(pendingChanges), newOrders)
  }

  @tailrec private def tryFillOrder(
    incoming:      Order,
    waitingOrders: SortedQueue[Order],
    acc:           OrderFillAccumulator = OrderFillAccumulator()
  ): (
      FillOrderResult,
      SortedQueue[Order],
      OrderFillAccumulator
  ) = {
    if (waitingOrders.isEmpty)
      (Postponed(incoming), waitingOrders, acc)
    else {
      val SortedQueue(head, tail) = waitingOrders
      incoming.tryMatchWith(head) match {
        case NoMatch                             => (Postponed(incoming), waitingOrders, acc)
        case FullMatch(transaction)              =>
          checkForMinus(transaction, acc.pendingChanges) match {
            case Success                  => (Closed, tail, acc + transaction)
            case Failure(incoming.action) => (Postponed(incoming), waitingOrders, acc)
            case Failure(_)               => tryFillOrder(incoming, tail, acc +! head)
          }
        case PartialMatch(transaction, residual) =>
          checkForMinus(transaction, acc.pendingChanges) match {
            case Success if residual.action == incoming.action => tryFillOrder(residual, tail, acc + transaction)
            case Success                                       => (Closed, tail + residual, acc + transaction)
            case Failure(incoming.action)                      => (Postponed(incoming), waitingOrders, acc)
            case Failure(_)                                    => tryFillOrder(incoming, tail, acc +! head)
          }
      }
    }
  }

  private def collectOrders(failures: List[Order], updatedOrders: SortedQueue[Order]) =
    failures.foldLeft(updatedOrders)((queue, order) => queue + order)

  private def applyPendingChanges(changes: Map[ClientName, ClientBalanceChange]): Map[ClientName, Client] =
    changes.foldLeft(clients)((map, kv) => map.adjust(kv._1)(client => client.applyChange(kv._2)))

  private def checkForMinus(
    transaction:    Transaction,
    pendingChanges: Map[ClientName, ClientBalanceChange]
  ): TransactionCheck = {
    val Transaction(buyerName, sellerName, price, security, amount) = transaction
    val buyer  = clients(buyerName)
    val seller = clients(sellerName)

    val buyerCashChange = pendingChanges(buyerName).cash
    val sellerSecuritiesChange = pendingChanges(sellerName).securitiesAmount(security)

    if (buyer.cash + buyerCashChange < price * amount) Failure(Buy)
    else if (seller.securitiesAmount(security) + sellerSecuritiesChange < amount) Failure(Sell)
    else Success
  }
}
