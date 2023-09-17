package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class IsHexWordStrict[L <: ByteLength]

object IsHexWordStrict:
  trait Utils[L <: ByteLength, IT <: ByteVector :| IsHexWordStrict[L]] extends ByteVectorUtils[IsHexWordStrict[L], IT]

  given [L <: ByteLength]: Constraint[ByteVector, IsHexWordStrict[L]] with
    inline def test(value: ByteVector): Boolean = value.length == constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bytes"
