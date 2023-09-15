package cleareth.model

import cleareth.model.constraint.HexWord
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

/** This object represents an Ethereum Public address, which must be an hexadecimal string of 20 bytes
  */
opaque type Address = ByteVector :| HexWord[20]

object Address extends HexWord.Utils[20, Address]:
  val mint: Address = Address("0x0000000000000000000000000000000000000000")
