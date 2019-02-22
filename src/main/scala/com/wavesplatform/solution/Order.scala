package com.wavesplatform.solution

case class Order(
  number:     Int,
  clientName: ClientName,
  action:     OrderAction,
  security:   Security,
  price:      Int,
  amount:     Int
) {
  def tryMatchWith(that: Order): MatchResult =
    (this.action, that.action) match {
      case (Buy, Sell) if this.price >= that.price => matchWith(that, buyer = clientName, seller = that.clientName)
      case (Sell, Buy) if this.price <= that.price => matchWith(that, seller = clientName, buyer = that.clientName)
      case _                                       => NoMatch
    }

  private def matchWith(opposite: Order, buyer: ClientName, seller: ClientName): MatchResult = {
    val (price, residualOrderNumber) =
      if (this.number < opposite.number)
        (this.price, opposite.number)
      else
        (opposite.price, this.number)

    val amount2Transaction = Transaction(
      buyerName = buyer,
      sellerName = seller,
      price = price,
      security = security,
      _: Int
    )
    if (this.amount == opposite.amount)
      FullMatch(amount2Transaction(amount))
    else {
      val (transactionAmount, residualOrder) = calcAmountAndResidual(opposite, residualOrderNumber)
      PartialMatch(amount2Transaction(transactionAmount), residualOrder)
    }
  }

  private def calcAmountAndResidual(opposite: Order, number: Int): (Int, Order) = {
    if (this.amount > opposite.amount)
      (
        opposite.amount,
        Order(
          security = security,
          number = number,
          clientName = this.clientName,
          action = this.action,
          price = this.price,
          amount = this.amount - opposite.amount
        )
      )
    else opposite.calcAmountAndResidual(this, number)
  }
}
