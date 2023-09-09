package cleareth.model

import cleareth.decoding.EvmDecoder
import cleareth.encoding.EvmEncoder

case class Dummy(
    string1: String,
    string2: String,
    int: Int
) derives EvmEncoder,
      EvmDecoder
