package com.wavesplatform.solution

object MapHelper {
  implicit class Ops[K, V](map: Map[K, V]) {
    def adjust(k: K)(f: V => V): Map[K, V] = map.updated(k, f(map(k)))
  }
}
