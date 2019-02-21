package com.wavesplatform.solution

object OrderMatchingApp extends App {
  val (clients, orders) = DataMapper.toModels(
    Resources.clientData(),
    Resources.orderData()
  )
  val result: Map[ClientName, Client] = orders
    .foldLeft(TradingExchange(clients))((ex, o) => ex.processOrder(o))
    .clients

  Resources.writeResults(DataMapper.toString(result.values))
}
