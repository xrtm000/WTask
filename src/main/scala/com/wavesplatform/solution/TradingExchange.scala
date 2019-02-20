package com.wavesplatform.solution

import scala.collection.immutable.Queue

case class TradingExchange(
  clients: Map[String, Client],
  orders: Map[Security, Queue[Order]]
) {
  def processOrder(order: Order): TradingExchange = {
    orders.get(order.security) match {
      case Some(waitingOrders) =>
        waitingOrders.headOption match {
          case Some(oppositeOrder) if order.`type` != oppositeOrder.`type` =>
            matchOrders()
            copy(orders = orders +)
          case Some(_) =>
            copy(orders = orders + (order.security -> (waitingOrders :+ order)))
          case None =>
            copy(orders = orders + (order.security -> Queue(order)))
        }
      case None =>
        copy(orders = orders + (order.security -> Queue(order)))
    }
  }

  private def matchOrders(waitingOrders: List[Order], oppositeOrder: Order): OrderMatchingResult = {
    waitingOrders match {
      case order :: _ =>
        mergeOrders(order, oppositeOrder)
      case Nil => OrderMatchingResult(waitingOrders = List(oppositeOrder))
    }
  }

  private def mergeOrders(order: Order, oppositeOrder: Order): Option[Order] = {
    order.amount match {
      case oppositeOrder.amount => None
      case less if less < oppositeOrder.amount => Some(oppositeOrder.copy())
    }
  }
}

case class OrderMatchingResult(
  closed: List[Order] = Nil,
  waitingOrders: List[Order]
)