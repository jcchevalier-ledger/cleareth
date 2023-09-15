package cleareth.model.constraint

import io.github.iltotore.iron.*
import scodec.bits.ByteVector

private[constraint] trait ByteVectorUtils[C, IT <: ByteVector :| C] extends RefinedTypeOpsImpl[ByteVector, C, IT]:

  inline def apply(inline bv: ByteVector)(using Constraint[ByteVector, C]): IT = applyUnsafe(bv)
  inline def apply(inline str: String)(using Constraint[ByteVector, C]): IT = applyUnsafe(ByteVector.fromValidHex(str))

  inline def from(inline bv: ByteVector)(using Constraint[ByteVector, C]): Option[IT] = option(bv)
  inline def from(inline str: String)(using Constraint[ByteVector, C]): Option[IT] =
    for
      bv   <- ByteVector.fromHex(str)
      word <- option(bv)
    yield word

  inline def fromDescriptive(inline bv: ByteVector)(using Constraint[ByteVector, C]): Either[String, IT] = either(bv)
  inline def fromDescriptive(inline str: String)(using Constraint[ByteVector, C]): Either[String, IT] =
    for
      bv   <- ByteVector.fromHexDescriptive(str)
      word <- either(bv)
    yield word
