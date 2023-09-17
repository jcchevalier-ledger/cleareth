package cleareth.model.constraint

import cats.syntax.either.*
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

private[constraint] trait UTF8StringUtils[C, IT <: ByteVector :| C] extends RefinedTypeOpsImpl[ByteVector, C, IT]:

  inline def fromStringDescriptive(inline str: String)(using Constraint[ByteVector, C]): Either[String, IT] =
    for
      bv   <- ByteVector.encodeUtf8(str).leftMap(_.getMessage)
      word <- either(bv)
    yield word

  inline def fromString(inline str: String)(using Constraint[ByteVector, C]): Option[IT] =
    fromStringDescriptive(str).toOption

  inline def fromStringUnsafe(inline str: String)(using Constraint[ByteVector, C]): IT =
    fromStringDescriptive(str).fold(
      err => throw new IllegalArgumentException(s"An exception occurred during the encoding of $str: $err"),
      identity
    )
