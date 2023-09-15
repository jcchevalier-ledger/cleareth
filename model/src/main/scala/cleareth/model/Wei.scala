package cleareth.model

import cleareth.model.constraint.IsUInt
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

opaque type Wei = ByteVector :| IsUInt[256]

object Wei extends IsUInt.Utils[256, Wei]
