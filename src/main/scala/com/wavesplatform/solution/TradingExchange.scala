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
    val (status, updatedOrders, OrderFillAccumulator(failures, transactions)) = tryFillOrder(order, oppositeOrders)

    val newOppositeOrders = oppositeKey -> collectOrders(failures, updatedOrders)
    val newOrders = status match {
      case Closed    => orders + newOppositeOrders
      case Postponed(o) => orders.adjust(o.security, o.action)(_ + o) + newOppositeOrders
    }
    copy(applyTransactions(transactions), newOrders)
  }

  @tailrec private def tryFillOrder(
    incoming:      Order,
    waitingOrders: SortedQueue[Order],
    accum:         OrderFillAccumulator = OrderFillAccumulator()
  ): (
      FillOrderResult,
      SortedQueue[Order],
      OrderFillAccumulator
  ) = {
    if (waitingOrders.isEmpty)
      (Postponed(incoming), waitingOrders, accum)
    else {
      val SortedQueue(head, tail) = waitingOrders
      incoming.tryMatchWith(head) match {
        case NoMatch                             => (Postponed(incoming), waitingOrders, accum)
        case FullMatch(transaction)              =>
          checkForMinus(transaction, accum.transactions) match {
            case Success                  => (Closed, tail, accum + transaction)
            case Failure(incoming.action) => (Postponed(incoming), waitingOrders, accum)
            case Failure(_)               => tryFillOrder(incoming, tail, accum +! head)
          }
        case PartialMatch(transaction, residual) =>
          checkForMinus(transaction, accum.transactions) match {
            case Success if residual.action == incoming.action => tryFillOrder(residual, tail, accum + transaction)
            case Success                                       => (Closed, tail + residual, accum + transaction)
            case Failure(incoming.action)                      => (Postponed(incoming), waitingOrders, accum)
            case Failure(_)                                    => tryFillOrder(incoming, tail, accum +! head)
          }
      }
    }
  }

  private def collectOrders(failures: List[Order], updatedOrders: SortedQueue[Order]) =
    failures.foldLeft(updatedOrders)((queue, order) => queue + order)

  private def applyTransactions(transactions: List[Transaction]) =
    transactions.foldLeft(clients)((map, t) => map ++ applyTransaction(t, map))

  private def applyTransaction(
    transaction: Transaction,
    clients:     Map[ClientName, Client]
  ): Map[ClientName, Client] = {
    val Transaction(buyerName, sellerName, price, security, amount) = transaction
    val buyer = clients(buyerName)
    val seller = clients(sellerName)
    Map(
      buyerName -> buyer.copy(
        cash = buyer.cash - price * amount,
        securitiesAmount = buyer.securitiesAmount.adjust(security)(_ + amount)
      ),
      sellerName -> seller.copy(
        cash = seller.cash + price * amount,
        securitiesAmount = seller.securitiesAmount.adjust(security)(_ - amount)
      )
    )
  }

  private def checkForMinus(transaction: Transaction, accum: OrderFillAccumulator): TransactionCheck = {
    val Transaction(buyerName, sellerName, price, security, amount) = transaction
    val buyer  = clients(buyerName)
    val seller = clients(sellerName)

    if (buyer.cash < price * amount) Failure(Buy)
    else if (seller.securitiesAmount(security) < amount) Failure(Sell)
    else Success
  }
}
