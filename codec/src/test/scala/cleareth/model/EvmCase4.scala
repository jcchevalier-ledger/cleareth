package cleareth.model

import cleareth.decoding.EvmDecoder
import cleareth.encoding.EvmEncoder

case class EvmCase4(ids: List[Int], values: List[Int]) derives EvmEncoder, EvmDecoder

object EvmCase4:
  val words: List[String] = List(
    "0000000000000000000000000000000000000000000000000000000000000040",
    "0000000000000000000000000000000000000000000000000000000000000060",
    "0000000000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000000000"
  )

  val testCase: EvmCase4 = EvmCase4(List.empty, List.empty)
