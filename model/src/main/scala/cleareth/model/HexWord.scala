package cleareth.model

import cleareth.model.constraint.IsHexWord
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

/** This object represents a static hex word, which must be an hexadecimal string of at most 32 bytes
  */
opaque type HexWord = ByteVector :| IsHexWord

object HexWord extends RefinedTypeOpsImpl[ByteVector, IsHexWord, HexWord]:

  inline def apply(inline bv: ByteVector): HexWord = applyUnsafe(bv)
  inline def apply(inline str: String): HexWord    = applyUnsafe(ByteVector.fromValidHex(str).padTo(32))

  inline def from(inline bv: ByteVector): Option[HexWord] = option(bv)
  inline def from(inline str: String): Option[HexWord] =
    for
      bv   <- ByteVector.fromHex(str)
      word <- option(bv.padTo(32))
    yield word

  inline def fromDescriptive(inline bv: ByteVector): Either[String, HexWord] = either(bv)
  inline def fromDescriptive(inline str: String): Either[String, HexWord] =
    for
      bv   <- ByteVector.fromHexDescriptive(str)
      word <- either(bv.padTo(32))
    yield word
