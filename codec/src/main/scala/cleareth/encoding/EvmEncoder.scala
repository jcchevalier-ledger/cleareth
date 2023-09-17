package cleareth.encoding

import cats.Contravariant
import cats.data.State
import cats.syntax.monoid.*
import scala.deriving.Mirror
import scodec.bits.ByteVector

trait EvmEncoder[A] extends Serializable:
  protected val nbOfElements: Int = 1
  def stateEncode(a: A): State[Long, EncodingAcc]
  final def encode(a: A): ByteVector = stateEncode(a).run(nbOfElements * 32).value._2.merge

object EvmEncoder extends EvmEncoderInstances:

  given Contravariant[EvmEncoder] = new Contravariant[EvmEncoder]:
    override def contramap[A, B](fa: EvmEncoder[A])(f: B => A): EvmEncoder[B] = (b: B) => fa.stateEncode(f(b))

  final def apply[A](using instance: EvmEncoder[A]): EvmEncoder[A]        = instance
  final def apply[A](value: A)(using instance: EvmEncoder[A]): ByteVector = instance.encode(value)

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
