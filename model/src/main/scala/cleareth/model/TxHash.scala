package cleareth.model

import cleareth.model.constraint.HexWord
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

/** This object represents an Ethereum Transaction hash, which must be an hexadecimal string of 32 bytes
  */
opaque type TxHash = ByteVector :| HexWord[32]

object TxHash extends HexWord.Utils[32, TxHash]
