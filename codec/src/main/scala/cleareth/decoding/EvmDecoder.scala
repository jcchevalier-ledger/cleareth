package cleareth.decoding

import cats.Functor
import cats.data.State
import cats.syntax.traverse.*
import cleareth.*
import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scodec.bits.ByteVector

trait EvmDecoder[A] extends Serializable:
  final def decode(bytes: ByteVector): Either[DecodingError, A] = stateDecode(bytes).runEmptyA.value
  def stateDecode(bytes: ByteVector): State[Long, Either[DecodingError, A]]

object EvmDecoder extends EvmDecoderInstances:

  given Functor[EvmDecoder] = new Functor[EvmDecoder]:
    override def map[A, B](fa: EvmDecoder[A])(f: A => B): EvmDecoder[B] = (bytes: ByteVector) =>
      fa.stateDecode(bytes).map(_.map(f))

  final def apply[A](using instance: EvmDecoder[A]): EvmDecoder[A] = instance
  final def apply[A](value: ByteVector)(using instance: EvmDecoder[A]): Either[DecodingError, A] =
    instance.decode(value)

  inline given derived[A](using m: Mirror.Of[A]): EvmDecoder[A] =
    new EvmDecoder[A]:
      private val elemDecoders: List[EvmDecoder[?]] = summonDecoders[m.MirroredElemTypes]

      override def stateDecode(value: ByteVector): State[Long, Either[DecodingError, A]] =
        m match
          case product: Mirror.ProductOf[A] =>
            for decodedElems <- elemDecoders
                .traverse(_.asInstanceOf[EvmDecoder[AnyRef]].stateDecode(value))
                .map(_.sequence)
            yield decodedElems.map(elems => product.fromProduct(Tuple.fromArray(elems.toArray)))
