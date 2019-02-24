package com.wavesplatform.solution

import com.wavesplatform.solution.MapHelper.Ops

case class Client(
  name:             ClientName,
  cash:             Int = 0,
  securitiesAmount: Map[Security, Int] = Map().withDefaultValue(0)
) {
  def buy(price: Int, security: Security, amount: Int): Client =
    copy(
      cash = cash - price * amount,
      securitiesAmount = securitiesAmount.adjust(security)(_ + amount)
    )

  def sell(price: Int, security: Security, amount: Int): Client =
    copy(
      cash = cash + price * amount,
      securitiesAmount = securitiesAmount.adjust(security)(_ - amount)
    )

  def applyChange(change: ClientBalanceChange): Client =
    copy(
      cash = cash + change.cash,
      securitiesAmount = change.securitiesAmount.foldLeft(securitiesAmount)((map, kv) => map.adjust(kv._1)(_ + kv._2))
    )
}
