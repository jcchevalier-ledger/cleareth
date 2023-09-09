package cleareth.encoding

import cats.Contravariant
import cats.data.State
import cats.syntax.contravariant.*
import cats.syntax.monoid.*
import cleareth.model.Address
import scala.deriving.Mirror
import scala.math.*
import scodec.bits.ByteVector

trait EvmEncoder[A] extends Serializable:
  protected val nbOfElements: Int = 1
  def stateEncode(a: A): State[Long, EncodingAcc]
  final def encode(a: A): ByteVector = stateEncode(a).run(nbOfElements * 32).value._2.merge

object EvmEncoder:

  given Contravariant[EvmEncoder] = new Contravariant[EvmEncoder]:
    override def contramap[A, B](fa: EvmEncoder[A])(f: B => A): EvmEncoder[B] = (b: B) => fa.stateEncode(f(b))

  final def apply[A](using instance: EvmEncoder[A]): EvmEncoder[A]        = instance
  final def apply[A](value: A)(using instance: EvmEncoder[A]): ByteVector = instance.encode(value)

  given EvmEncoder[Int] = (value: Int) =>
    State(offset => offset -> EncodingAcc.static(ByteVector.fromInt(value).padLeft(32)))
  given EvmEncoder[Long] = (value: Long) =>
    State(offset => offset -> EncodingAcc.static(ByteVector.fromLong(value).padLeft(32)))
  given EvmEncoder[BigInt] = (value: BigInt) =>
    State(offset => offset -> EncodingAcc.static(ByteVector(value.toByteArray).padLeft(32)))
  given EvmEncoder[Address] = (value: Address) => State(offset => offset -> EncodingAcc.static(value.value.padLeft(32)))
  given EvmEncoder[Boolean] = (bool: Boolean) =>
    State { offset =>
      offset -> EncodingAcc.static(ByteVector.fromValidHex(if (bool) "0x01" else "0x00").padLeft(32))
    }

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

  inline given derived[T](using m: Mirror.Of[T]): EvmEncoder[T] =
    new EvmEncoder[T]:
      private val elemEncoders: List[EvmEncoder[?]] = summonEncoders[m.MirroredElemTypes]
      override protected val nbOfElements: Int      = elemEncoders.length

      override def stateEncode(value: T): State[Long, EncodingAcc] =
        m match
          case _: Mirror.ProductOf[T] =>
            val product = value.asInstanceOf[Product]
            Iterable
              .tabulate(product.productArity) { index =>
                elemEncoders(index)
                  .asInstanceOf[EvmEncoder[Any]]
                  .stateEncode(product.productElement(index))
              }
              .foldLeft(State.empty[Long, EncodingAcc]) { (acc, next) =>
                for
                  a <- acc
                  n <- next
                yield a |+| n
              }
