package com.wavesplatform

package object solution {
  val priceTimePriority: Ordering[Order] = Ordering.by(order => (order.price, order.number))

  type ClientName = String
  type Security   = String

  case class Client(
    name:             ClientName,
    cash:             Int,
    securitiesAmount: Map[Security, Int]
  )

  case class Transaction(
    buyerName:   ClientName,
    sellerName:  ClientName,
    price:       Int,
    security:    String,
    amount:      Int
  )

  sealed abstract class MatchResult
  case class  FullMatch(transaction: Transaction) extends MatchResult
  case class  PartialMatch(transaction: Transaction, residual: Order) extends MatchResult
  case object NoMatch extends MatchResult
}
