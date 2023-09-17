package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class IsHexWord

object IsHexWord:
  trait Utils[IT <: ByteVector :| IsHexWord] extends ByteVectorUtils[IsHexWord, IT]

  given Constraint[ByteVector, IsHexWord] with
    inline def test(value: ByteVector): Boolean = value.length <= EVM_WORD_LENGTH
    inline def message: String                  = s"Expecting at most $EVM_WORD_LENGTH bytes"
