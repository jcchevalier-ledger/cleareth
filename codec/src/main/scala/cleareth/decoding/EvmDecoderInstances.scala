package cleareth.decoding

import cats.data.State
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cleareth.*
import cleareth.model.*
import scala.compiletime.constValue
import scala.reflect.ClassTag
import scala.util.Try
import scodec.bits.ByteVector

trait EvmDecoderInstances:
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

  given [T: EvmDecoder: ClassTag]: EvmDecoder[Array[T]] = EvmDecoder[Seq[T]].map(_.toArray)
  given [T: EvmDecoder]: EvmDecoder[Set[T]]             = EvmDecoder[Seq[T]].map(_.toSet)
  given [T: EvmDecoder]: EvmDecoder[List[T]]            = EvmDecoder[Seq[T]].map(_.toList)

  given EvmDecoder[Address] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Address.fromDescriptive(bytes.takeWordAt(offset)).leftMap(DecodingError.apply(bytes))
      )
    }

  given EvmDecoder[Wei] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        Wei.fromDescriptive(bytes.takeWordAt(offset)).leftMap(DecodingError.apply(bytes))
      )
    }

  given EvmDecoder[HexWord] = (bytes: ByteVector) =>
    State { offset =>
      (
        offset + EVM_WORD_LENGTH,
        HexWord.fromDescriptive(bytes.takeWordAt(offset)).leftMap(DecodingError.apply(bytes))
      )
    }
