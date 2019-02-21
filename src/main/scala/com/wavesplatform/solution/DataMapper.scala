package com.wavesplatform.solution

import scala.collection.immutable.SortedMap

object DataMapper {
  private val clientsRegex = """^(\w+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)$""".r
  private val ordersRegex  = """^(\w+)\t(\w)\t(\w+)\t(\d+)\t(\d+)$""".r

  def toModels(
    clientData: Iterator[String],
    orderData:  Iterator[String]
  ): (Map[ClientName, Client], Iterator[Order]) = {
    val clients = mapClients(clientData)
    val orders = mapOrders(orderData, clients)
    (clients, orders)
  }

  private def mapClients(data: Iterator[String]): SortedMap[ClientName, Client] = {
    val name2Client = data
      .map {
        case clientsRegex(name, cash, amountA, amountB, amountC, amountD) =>
          Client(
            name, cash.toInt,
            Map(
              "A" -> amountA.toInt,
              "B" -> amountB.toInt,
              "C" -> amountC.toInt,
              "D" -> amountD.toInt
            )
          )
      }
      .map(client => (client.name, client))
    SortedMap[ClientName, Client]() ++ name2Client
  }

  private def mapOrders(data: Iterator[String], clients: Map[ClientName, Client]): Iterator[Order] =
    data.map {
      case ordersRegex(clientName, symbol, security, price, amount) =>
        Order(
          number = Counter.next(),
          clientName = clientName,
          action = symbol match {
            case "b" => Buy
            case "s" => Sell
          },
          security = security,
          price = price.toInt,
          amount = amount.toInt
        )
    }

  def toString(clients: Iterable[Client]): Iterable[String] =
    clients
      .map(c =>
        s"${c.name}\t" +
        s"${c.cash}\t" +
        s"${c.securitiesAmount("A")}\t" +
        s"${c.securitiesAmount("B")}\t" +
        s"${c.securitiesAmount("C")}\t" +
        s"${c.securitiesAmount("D")}"
      )
}
