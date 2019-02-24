package com.wavesplatform

package object solution {
  val buyPriceTimePriority: Ordering[Order] =
    (x: Order, y: Order) => {
      val priceDiff = x.price - y.price
      if (priceDiff == 0) y.number - x.number else priceDiff
    }
  val sellPriceTimePriority: Ordering[Order] =
    (x: Order, y: Order) => {
      val priceDiff = y.price - x.price
      if (priceDiff == 0) y.number - x.number else priceDiff
    }

  type ClientName = String
  type Security   = String
  type ClientBalanceChange = Client

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

  sealed abstract class TransactionCheck
  case object Success extends TransactionCheck
  case class  Failure(action: OrderAction) extends TransactionCheck

  sealed abstract class FillOrderResult
  case object Closed extends FillOrderResult
  case class  Postponed(order: Order) extends FillOrderResult
}
