package com.wavesplatform.solution

import cats.Id
import cats.data.StateT

object Counter {
  var i = 0
  def next(): Int = {
    i = i + 1
    i
  }
}
object Test extends App {
  def next: StateT[Id, Int, Unit] = StateT[Id, Int, Unit](s => (s + 1, Unit))

}