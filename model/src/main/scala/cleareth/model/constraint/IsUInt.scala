package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class IsUInt[L <: BitLength]

object IsUInt:

  trait Utils[L <: BitLength, IT <: ByteVector :| IsUInt[L]] extends ByteVectorUtils[IsUInt[L], IT]:
    inline def apply(inline short: Short)(using Constraint[ByteVector, IsUInt[L]]): IT =
      require(short >= 0, s"supplied short should be positive, got $short")
      applyUnsafe(ByteVector.fromShort(short))

    inline def apply(inline int: Int)(using Constraint[ByteVector, IsUInt[L]]): IT =
      require(int >= 0, s"supplied int should be positive, got $int")
      applyUnsafe(ByteVector.fromInt(int))

    inline def apply(inline long: Long)(using Constraint[ByteVector, IsUInt[L]]): IT =
      require(long >= 0, s"supplied long should be positive, got $long")
      applyUnsafe(ByteVector.fromLong(long))

    inline def apply(inline bigInt: BigInt)(using Constraint[ByteVector, IsUInt[L]]): IT =
      require(bigInt >= zero, s"supplied bigInt should be positive, got $bigInt")
      applyUnsafe(ByteVector(bigInt.toByteArray).dropWhile(_ == 0))

    inline def from(inline short: Short)(using Constraint[ByteVector, IsUInt[L]]): Option[IT] =
      require(short >= 0, s"supplied short should be positive, got $short")
      option(ByteVector.fromShort(short))

    inline def from(inline int: Int)(using Constraint[ByteVector, IsUInt[L]]): Option[IT] =
      require(int >= 0, s"supplied int should be positive, got $int")
      option(ByteVector.fromInt(int))

    inline def from(inline long: Long)(using Constraint[ByteVector, IsUInt[L]]): Option[IT] =
      require(long >= 0, s"supplied long should be positive, got $long")
      option(ByteVector.fromLong(long))

    inline def from(inline bigInt: BigInt)(using Constraint[ByteVector, IsUInt[L]]): Option[IT] =
      require(bigInt >= zero, s"supplied bigInt should be positive, got $bigInt")
      option(ByteVector(bigInt.toByteArray).dropWhile(_ == 0))

    inline def fromDescriptive(inline short: Short)(using Constraint[ByteVector, IsUInt[L]]): Either[String, IT] =
      require(short >= 0, s"supplied short should be positive, got $short")
      either(ByteVector.fromShort(short))

    inline def fromDescriptive(inline int: Int)(using Constraint[ByteVector, IsUInt[L]]): Either[String, IT] =
      require(int >= 0, s"supplied int should be positive, got $int")
      either(ByteVector.fromInt(int))

    inline def fromDescriptive(inline long: Long)(using Constraint[ByteVector, IsUInt[L]]): Either[String, IT] =
      require(long >= 0, s"supplied long should be positive, got $long")
      either(ByteVector.fromLong(long))

    inline def fromDescriptive(inline bigInt: BigInt)(using Constraint[ByteVector, IsUInt[L]]): Either[String, IT] =
      require(bigInt >= zero, s"supplied bigInt should be positive, got $bigInt")
      either(ByteVector(bigInt.toByteArray).dropWhile(_ == 0))

  given [L <: BitLength]: Constraint[ByteVector, IsUInt[L]] with
    inline def test(value: ByteVector): Boolean = value.length * 8 <= constValue[L]
    inline def message: String                  = s"Expecting ${constValue[L]} bits"

  private val zero = BigInt(0)
