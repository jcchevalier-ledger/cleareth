package cleareth.decoding

import scodec.bits.ByteVector

final case class DecodingError(cause: String, parsedValue: ByteVector) extends Throwable:
  override def getMessage: String = s"$cause. got ${if (parsedValue.nonEmpty) parsedValue.toHex else "an empty byte array"}"

object DecodingError:
  def apply(parsedValue: ByteVector)(e: Throwable): DecodingError = DecodingError(e.getMessage, parsedValue)
