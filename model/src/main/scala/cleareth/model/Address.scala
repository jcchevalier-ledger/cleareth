package cleareth.model

import cleareth.model.constraint.IsHexWordStrict
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

/** This object represents an Ethereum Public address, which must be an hexadecimal string of 20 bytes
  */
opaque type Address = ByteVector :| IsHexWordStrict[20]

object Address extends IsHexWordStrict.Utils[20, Address]:
  val mint: Address = Address("0x0000000000000000000000000000000000000000")
