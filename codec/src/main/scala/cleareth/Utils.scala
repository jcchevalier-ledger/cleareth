package cleareth

import scodec.bits.ByteVector

object Utils:

  val EVM_WORD_LENGTH = 32 // bytes

  extension (bytes: ByteVector)
    def toWords: Seq[ByteVector] =
      if bytes.isEmpty then Seq.empty
      else
        bytes.splitAt(32) match
          case (head, tail) => head +: tail.toWords

    def takeWordAt(offset: Long): ByteVector = bytes.slice(offset, offset + EVM_WORD_LENGTH)
