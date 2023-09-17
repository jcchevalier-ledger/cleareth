package cleareth

import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.deriving.Mirror

package object encoding:
  inline final private[encoding] def summonEncoders[T <: Tuple]: List[EvmEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonEncoder[t] :: summonEncoders[ts]

  inline final private[encoding] def summonEncoder[A]: EvmEncoder[A] =
    summonFrom {
      case encodeA: EvmEncoder[A] => encodeA
      case _: Mirror.Of[A]        => EvmEncoder.derived[A]
      case _                      => error("did not found any EvmEncoder instance for class " + typeName[A])
    }
