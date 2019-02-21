package com.wavesplatform.solution

sealed abstract class OrderAction(val ordering: Ordering[Order]) {
  def opposite: OrderAction = this match {
    case Buy  => Sell
    case Sell => Buy
  }
}
case object Buy extends OrderAction(priceTimePriority)
case object Sell extends OrderAction(priceTimePriority.reverse)
