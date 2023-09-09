package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class UInt[L <: BitLength]

object UInt:

  trait Utils[L <: BitLength, IT <: ByteVector :| UInt[L]] extends ByteVectorUtils[UInt[L], IT]:
    inline def apply(inline short: Short)(using Constraint[ByteVector, UInt[L]]): IT =
      require(short >= 0)
      applyUnsafe(ByteVector.fromShort(short))

    inline def apply(inline int: Int)(using Constraint[ByteVector, UInt[L]]): IT =
      require(int >= 0)
      applyUnsafe(ByteVector.fromInt(int))

    inline def apply(inline long: Long)(using Constraint[ByteVector, UInt[L]]): IT =
      require(long >= 0)
      applyUnsafe(ByteVector.fromLong(long))

    inline def apply(inline bigInt: BigInt)(using Constraint[ByteVector, UInt[L]]): IT =
      require(bigInt >= 0)
      applyUnsafe(ByteVector(bigInt.toByteArray).dropWhile(_ == 0))

  given [L <: BitLength]: Constraint[ByteVector, UInt[L]] with
    inline def test(value: ByteVector): Boolean = value.length * 8 <= constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bits"
