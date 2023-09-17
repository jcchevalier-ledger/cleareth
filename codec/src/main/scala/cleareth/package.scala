import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scodec.bits.ByteVector

package object cleareth:
  val EVM_WORD_LENGTH = 32 // bytes

  extension (bytes: ByteVector)
    def toWords: Seq[ByteVector] =
      if bytes.isEmpty then Seq.empty
      else
        bytes.splitAt(32) match
          case (head, tail) => head +: tail.toWords

    def takeWordAt(offset: Long): ByteVector = bytes.slice(offset, offset + EVM_WORD_LENGTH)

  private def tpeNmeMacro[A: Type](using Quotes) = Expr(Type.show[A])
  transparent inline def typeName[A]             = ${ tpeNmeMacro[A] }
