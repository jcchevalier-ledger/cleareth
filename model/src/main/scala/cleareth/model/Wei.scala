package cleareth.model

import cleareth.model.constraint.UInt
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

opaque type Wei = ByteVector :| UInt[256]

object Wei extends UInt.Utils[256, Wei]
