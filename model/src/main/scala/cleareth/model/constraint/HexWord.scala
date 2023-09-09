package cleareth.model.constraint

import io.github.iltotore.iron.*
import scala.compiletime.constValue
import scodec.bits.ByteVector

final class HexWord[L <: ByteLength]

object HexWord:
  trait Utils[L <: ByteLength, IT <: ByteVector :| HexWord[L]] extends ByteVectorUtils[HexWord[L], IT]
