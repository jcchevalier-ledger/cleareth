package cleareth.model

import cleareth.model.constraint.HexWord
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

/** This object represents an Ethereum Block hash, which must be an hexadecimal string of 32 bytes
  */
opaque type BlockHash = ByteVector :| HexWord[32]

object BlockHash extends HexWord.Utils[32, BlockHash]
