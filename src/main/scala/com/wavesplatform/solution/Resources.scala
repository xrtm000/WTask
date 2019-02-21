package com.wavesplatform.solution

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

import scala.io.Source

object Resources {
  private val ordersFilename = "orders.txt"
  private val clientsFilename = "clients.txt"
  private val resultFilename = "result.txt"

  def clientData(): Iterator[String] =
    Source.fromResource(clientsFilename).getLines()

  def orderData(): Iterator[String] =
    Source.fromResource(ordersFilename).getLines()

  def writeResults(results: Iterable[String]): Unit =
    Files.write(Paths.get(resultFilename), results.asJava)
}
