package cleareth

import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.Mirror

package object decoding:
  inline final private[decoding] def summonDecoders[T <: Tuple]: List[EvmDecoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonDecoder[t] :: summonDecoders[ts]

  inline final private[decoding] def summonDecoder[A]: EvmDecoder[A] =
    summonFrom:
      case encodeA: EvmDecoder[A] => encodeA
      case _: Mirror.Of[A]        => EvmDecoder.derived[A]
