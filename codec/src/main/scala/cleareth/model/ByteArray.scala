package cleareth.model

import cats.data.State
import cats.syntax.either.*
import cleareth.Utils.*
import cleareth.decoding.DecodingError
import cleareth.decoding.EvmDecoder
import cleareth.encoding.EncodingAcc
import cleareth.encoding.EvmEncoder
import java.nio.charset.CharacterCodingException
import scala.annotation.nowarn
import scala.compiletime.constValue
import scala.util.Try
import scodec.bits.ByteVector

final case class ByteArray[Length](private val value: ByteVector) extends AnyVal:
  override def toString: String  = s"0x${value.toHex}"
  def decodeUtf8Lenient: String  = value.decodeUtf8Lenient
  def decodeAsciiLenient: String = value.decodeAsciiLenient

object ByteArray:

  private type Length = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 |
    22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32

  @nowarn
  inline given [T <: Length]: EvmDecoder[ByteArray[T]] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Try(bytes.takeWordAt(offset).take(constValue[T])).toEither
          .map(ByteArray[T].apply)
          .leftMap(DecodingError.apply(bytes))
      )
    }

  @nowarn
  inline given [T <: Length]: EvmEncoder[ByteArray[T]] = (a: ByteArray[T]) =>
    State.apply { offset => offset -> EncodingAcc.static(a.value.padRight(32)) }

  def apply[T <: Length](string: String): Either[CharacterCodingException, ByteArray[T]] =
    ByteVector.encodeUtf8(string).map(ByteArray[T].apply)
