package com.wavesplatform.solution

case class OrderFillAccumulator(
  failures:       List[Order]       = Nil,
  transactions:   List[Transaction] = Nil,
  pendingChanges: Map[ClientName, Client]
) {
  def +!(o: Order): OrderFillAccumulator = copy(failures = o :: failures)
  def +(t: Transaction): OrderFillAccumulator = copy(transactions = t :: transactions)
}
