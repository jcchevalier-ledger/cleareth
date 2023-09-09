package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class HexWord[L <: ByteLength]

object HexWord:
  trait Utils[L <: ByteLength, IT <: ByteVector :| HexWord[L]] extends ByteVectorUtils[HexWord[L], IT]

  given [L <: ByteLength]: Constraint[ByteVector, HexWord[L]] with
    inline def test(value: ByteVector): Boolean = value.length == constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bytes"
