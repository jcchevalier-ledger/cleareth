package cleareth.decoding

import cats.Functor
import cats.data.State
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cleareth.Utils.*
import cleareth.model.Address
import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.math.*
import scala.reflect.ClassTag
import scala.util.Try
import scodec.bits.ByteVector

trait EvmDecoder[A] extends Serializable:
  final def decode(bytes: ByteVector): Either[DecodingError, A] = stateDecode(bytes).runEmptyA.value
  def stateDecode(bytes: ByteVector): State[Long, Either[DecodingError, A]]

object EvmDecoder:

  given Functor[EvmDecoder] = new Functor[EvmDecoder]:
    override def map[A, B](fa: EvmDecoder[A])(f: A => B): EvmDecoder[B] = (bytes: ByteVector) =>
      fa.stateDecode(bytes).map(_.map(f))

  final def apply[A](using instance: EvmDecoder[A]): EvmDecoder[A]                               = instance
  final def apply[A](value: ByteVector)(using instance: EvmDecoder[A]): Either[DecodingError, A] = instance.decode(value)

  given EvmDecoder[ByteVector] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Right(bytes.takeWordAt(offset))
      )
    }

  given EvmDecoder[Int] = new EvmDecoder[Int]:
    private val paddingLength = EVM_WORD_LENGTH - 4
    private val padding       = ByteVector.low(paddingLength)

    override def stateDecode(bytes: ByteVector): State[Long, Either[DecodingError, Int]] =
      State { offset =>
        (
          offset + EVM_WORD_LENGTH,
          for
            word <- Try(bytes.takeWordAt(offset)).toEither.leftMap(DecodingError.apply(bytes))
            trimmedWord <- Either.cond(
              word.startsWith(padding) || word.isEmpty,
              word.drop(paddingLength),
              DecodingError("could not parse following value as an Int", bytes)
            )
            decodedValue <- Try(trimmedWord.toInt()).toEither.leftMap(DecodingError.apply(bytes))
          yield decodedValue
        )
      }

  given EvmDecoder[Long] = new EvmDecoder[Long]:
    private val paddingLength = EVM_WORD_LENGTH - 8
    private val padding       = ByteVector.low(paddingLength)

    override def stateDecode(bytes: ByteVector): State[Long, Either[DecodingError, Long]] =
      State { offset =>
        (
          offset + EVM_WORD_LENGTH,
          for
            word <- Try(bytes.takeWordAt(offset)).toEither.leftMap(DecodingError.apply(bytes))
            trimmedWord <- Either.cond(
              word.startsWith(padding) || word.isEmpty,
              word.drop(paddingLength),
              DecodingError("could not parse following value as a Long", bytes)
            )
            decodedValue <- Try(trimmedWord.toLong()).toEither.leftMap(DecodingError.apply(bytes))
          yield decodedValue
        )
      }

  given EvmDecoder[BigInt] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Try(BigInt(1, bytes.takeWordAt(offset).toArray)).toEither.leftMap(DecodingError.apply(bytes))
      )
    }

  given EvmDecoder[Address] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Address.fromDescriptive(bytes.takeWordAt(offset)).leftMap(DecodingError.apply(bytes))
      )
    }

  given EvmDecoder[Boolean] = new EvmDecoder[Boolean]:
    private val FALSE = ByteVector.fromValidHex("0x00").padLeft(EVM_WORD_LENGTH)
    private val TRUE  = ByteVector.fromValidHex("0x01").padLeft(EVM_WORD_LENGTH)

    override def stateDecode(bytes: ByteVector): State[Long, Either[DecodingError, Boolean]] =
      State { offset =>
        (
          offset + EVM_WORD_LENGTH,
          bytes.takeWordAt(offset) match
            case FALSE => Right(false)
            case TRUE  => Right(true)
            case _     => Left(DecodingError("Could not parse following value as a Boolean", bytes))
        )
      }

  given EvmDecoder[String] = (bytes: ByteVector) =>
    State { offset =>
      val newOffset = offset + EVM_WORD_LENGTH
      (
        newOffset,
        for
          dynamicOffset        <- Try(bytes.takeWordAt(offset)).toEither.leftMap(DecodingError(bytes))
          decodedDynamicOffset <- EvmDecoder[Long].decode(dynamicOffset)
          _ <- Either.cond(
            decodedDynamicOffset >= offset + EVM_WORD_LENGTH,
            (),
            DecodingError(s"invalid dynamic offset. it should be >= decoding offset ($newOffset)", dynamicOffset)
          )
          resp <- Try(bytes.drop(decodedDynamicOffset).splitAt(EVM_WORD_LENGTH)).toEither
            .leftMap(DecodingError(bytes))
          (stringLength, value) = resp
          decodedStringLength <- EvmDecoder[Long].decode(stringLength)
          parsedValue <- Try(value.take(decodedStringLength).decodeUtf8Lenient).toEither.leftMap(DecodingError(bytes))
        yield parsedValue
      )
    }

  given [T](using EvmDecoder[T]): EvmDecoder[Seq[T]] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        for
          dynamicOffset        <- Try(bytes.takeWordAt(offset)).toEither.leftMap(DecodingError(bytes))
          decodedDynamicOffset <- EvmDecoder[Long].decode(dynamicOffset)
          resp <- Try(bytes.drop(decodedDynamicOffset).splitAt(EVM_WORD_LENGTH)).toEither
            .leftMap(DecodingError(bytes))
          (arraySize, rest) = resp
          decodedArraySize <- EvmDecoder[Long].decode(arraySize)
          decodedValues <- Seq
            .range(0L, decodedArraySize)
            .traverse(_ => EvmDecoder[T].stateDecode(rest))
            .runEmptyA
            .value
            .sequence
        yield decodedValues
      )
    }

  given [T](using EvmDecoder[T], ClassTag[T]): EvmDecoder[Array[T]] = EvmDecoder[Seq[T]].map(_.toArray)
  given [T](using EvmDecoder[T]): EvmDecoder[Set[T]]                = EvmDecoder[Seq[T]].map(_.toSet)
  given [T](using EvmDecoder[T]): EvmDecoder[List[T]]               = EvmDecoder[Seq[T]].map(_.toList)

  inline given derived[A](using m: Mirror.Of[A]): EvmDecoder[A] =
    new EvmDecoder[A]:
      private val elemDecoders: List[EvmDecoder[?]] = summonDecoders[m.MirroredElemTypes]

      override def stateDecode(value: ByteVector): State[Long, Either[DecodingError, A]] =
        m match
          case product: Mirror.ProductOf[A] =>
            for decodedElems <- elemDecoders.traverse(_.asInstanceOf[EvmDecoder[AnyRef]].stateDecode(value)).map(_.sequence)
            yield decodedElems.map(elems => product.fromProduct(Tuple.fromArray(elems.toArray)))
