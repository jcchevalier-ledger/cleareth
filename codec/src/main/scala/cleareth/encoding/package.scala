package cleareth

import cats.data.State
import cats.syntax.contravariant.*
import cats.syntax.semigroup.*
import cleareth.model.Address
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scodec.bits.ByteVector

package object encoding:
  inline final private[encoding] def summonEncoders[T <: Tuple]: List[EvmEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonEncoder[t] :: summonEncoders[ts]

  inline final private[encoding] def summonEncoder[A]: EvmEncoder[A] =
    summonFrom:
        case encodeA: EvmEncoder[A] => encodeA
        case _: Mirror.Of[A]        => EvmEncoder.derived[A]

  given EvmEncoder[Int] = (value: Int) =>
    State(offset => offset -> EncodingAcc.static(ByteVector.fromInt(value).padLeft(32)))

  given EvmEncoder[Long] = (value: Long) =>
    State(offset => offset -> EncodingAcc.static(ByteVector.fromLong(value).padLeft(32)))

  given EvmEncoder[BigInt] = (value: BigInt) =>
    State(offset => offset -> EncodingAcc.static(ByteVector(value.toByteArray).padLeft(32)))

  given EvmEncoder[Address] = (value: Address) => State(offset => offset -> EncodingAcc.static(value.value.padLeft(32)))

  given EvmEncoder[Boolean] = (bool: Boolean) =>
    State(offset => offset -> EncodingAcc.static(ByteVector.fromValidHex(if (bool) "0x01" else "0x00").padLeft(32)))

  given EvmEncoder[String] = (value: String) =>
    State { offset =>
      val cont          = ByteVector(value.getBytes)
      val paddedContent = cont.padRight((cont.length / 32 + 1) * 32)
      val length        = ByteVector.fromLong(cont.length).padLeft(32)
      val newOffset     = offset + length.length + paddedContent.length
      newOffset -> EncodingAcc.dynamic(ByteVector.fromLong(offset).padLeft(32), length ++ paddedContent)
    }

  given [T](using EvmEncoder[T]): EvmEncoder[Seq[T]] = (value: Seq[T]) =>
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

  given [T](using EvmEncoder[T]): EvmEncoder[Array[T]] = EvmEncoder[Seq[T]].contramap(_.toSeq)
  given [T](using EvmEncoder[T]): EvmEncoder[List[T]]  = EvmEncoder[Seq[T]].contramap(_.toSeq)
  given [T](using EvmEncoder[T]): EvmEncoder[Set[T]]   = EvmEncoder[Seq[T]].contramap(_.toSeq)
