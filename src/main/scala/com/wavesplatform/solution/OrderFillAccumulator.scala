package com.wavesplatform.solution

case class OrderFillAccumulator(
  failures:       List[Order] = Nil,
  pendingChanges: Map[ClientName, ClientBalanceChange] = Map().withDefault(Client(_))
) {
  def +!(o: Order): OrderFillAccumulator = copy(failures = o :: failures)

  def +(t: Transaction): OrderFillAccumulator = {
    val Transaction(buyerName, sellerName, price, security, amount) = t
    val buyer = pendingChanges(buyerName)
    val seller = pendingChanges(sellerName)
    copy(
      pendingChanges = pendingChanges ++ Map(
        buyerName -> buyer.buy(price, security, amount),
        sellerName -> seller.sell(price, security, amount)
      )
    )
  }
}
