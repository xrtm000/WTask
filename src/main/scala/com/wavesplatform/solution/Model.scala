package com.wavesplatform.solution

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet

case class Security(name: String) extends AnyVal

case class Client(
  name: String,
  balance: Int,
  securitiesAmount: Map[Security, Int]
)

sealed class OrderType(val code: Char)
case object Buy  extends OrderType('b')
case object Sell extends OrderType('s')

case class Order(
  client: Client,
  `type`: OrderType,
  security: Security,
  price: Int,
  amount: Int
)

object Order {
  val ascByPrice: Ordering[Order] = (x, y) => x.price - y.price
}

case class WaitingOrders(
  buys: SortedSet[Order] = TreeSet.empty[Order](Order.ascByPrice.reverse),
  sells: SortedSet[Order] = TreeSet.empty[Order](Order.ascByPrice)
)

