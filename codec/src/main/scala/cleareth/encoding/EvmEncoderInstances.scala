package cleareth.encoding

import cats.data.State
import cats.syntax.contravariant.*
import cats.syntax.monoid.*
import cleareth.model.*
import io.github.iltotore.iron.*
import scodec.bits.ByteVector

trait EvmEncoderInstances:
  given EvmEncoder[Int] = (value: Int) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(ByteVector.fromInt(value).padLeft(32))
      )
    }

  given EvmEncoder[Long] = (value: Long) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(ByteVector.fromLong(value).padLeft(32))
      )
    }

  given EvmEncoder[BigInt] = (value: BigInt) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(ByteVector(value.toByteArray).padLeft(32))
      )
    }

  given EvmEncoder[Boolean] = (bool: Boolean) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(ByteVector.fromValidHex(if (bool) "0x01" else "0x00").padLeft(32))
      )
    }

  given EvmEncoder[String] = (value: String) =>
    State { offset =>
      val cont          = ByteVector(value.getBytes)
      val paddedContent = cont.padRight((cont.length / 32 + 1) * 32)
      val length        = ByteVector.fromLong(cont.length).padLeft(32)
      val newOffset     = offset + length.length + paddedContent.length
      newOffset -> EncodingAcc.dynamic(ByteVector.fromLong(offset).padLeft(32), length ++ paddedContent)
    }

  given [T: EvmEncoder]: EvmEncoder[Seq[T]] = (value: Seq[T]) =>
    State { offset =>
      val cont = value
        .map(EvmEncoder[T].stateEncode)
        .foldLeft(State.empty[Long, EncodingAcc])((acc, next) =>
          for
            a <- acc
            n <- next
          yield a |+| n
        )
        .run(32 * value.length)
        .value
        ._2
        .merge
      val noOfElements = ByteVector.fromLong(value.length).padLeft(32)
      val newOffset    = offset + noOfElements.length + cont.length
      newOffset -> EncodingAcc.dynamic(ByteVector.fromLong(offset).padLeft(32), noOfElements ++ cont)
    }

  given [T: EvmEncoder]: EvmEncoder[Array[T]] = EvmEncoder[Seq[T]].contramap(_.toSeq)
  given [T: EvmEncoder]: EvmEncoder[List[T]]  = EvmEncoder[Seq[T]].contramap(_.toSeq)
  given [T: EvmEncoder]: EvmEncoder[Set[T]]   = EvmEncoder[Seq[T]].contramap(_.toSeq)

  given EvmEncoder[Address] = (value: Address) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(value.value.padLeft(32))
      )
    }

  given EvmEncoder[Wei] = (value: Wei) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(value.value.padLeft(32))
      )
    }

  given EvmEncoder[HexWord] = (value: HexWord) =>
    State { offset =>
      (
        offset,
        EncodingAcc.static(value.value)
      )
    }
