package cleareth.model

import cleareth.model.constraint.*
import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

package object constraint:
  type BitLength = 8 | 16 | 32 | 64 | 128 | 256
  type ByteLength = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 |
    23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32

  given [L <: ByteLength]: Constraint[ByteVector, HexWord[L]] with
    inline def test(value: ByteVector): Boolean = value.length == constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bytes"

  given [L <: BitLength]: Constraint[ByteVector, UInt[L]] with
    inline def test(value: ByteVector): Boolean = value.length * 8 <= constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bits"
