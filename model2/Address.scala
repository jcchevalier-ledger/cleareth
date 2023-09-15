package cleareth.model2

import cleareth.model.Address.ETH_ADDRESS_LENGTH
import scala.util.Try
import scodec.bits.ByteVector

final case class Address(value: ByteVector):
  require(value.length == ETH_ADDRESS_LENGTH, "An ethereum address is 20 bytes")
  def format(format: HexFormat): String = format.format(value)

object Address:
  private val ETH_ADDRESS_LENGTH = 20

  def fromDescriptive(bytes: ByteVector): Either[Throwable, Address] =
    for
      trim <- Try(bytes.dropWhile(_ == 0.toByte).padLeft(ETH_ADDRESS_LENGTH)).toEither
      addr <- Either.cond(
        trim.length == ETH_ADDRESS_LENGTH,
        Address(trim),
        IllegalArgumentException(
          s"Could not parse following value as an ethereum address: length is not $ETH_ADDRESS_LENGTH bytes (got ${bytes.toHex})"
        )
      )
    yield addr

  def from(bytes: ByteVector): Option[Address] = fromDescriptive(bytes).toOption

  def unsafeFrom(bytes: ByteVector): Address = from(bytes).get

  def from(s: String): Option[Address] = for
    byteVector <- ByteVector.fromHex(s)
    addr       <- Option.when(byteVector.length == ETH_ADDRESS_LENGTH)(byteVector)
  yield Address(addr)

  def unsafeFrom(s: String): Address = from(s).get

  def from(bytes: Array[Byte]): Option[Address] = from(ByteVector(bytes))

  val burn: Address          = Address.unsafeFrom("0x000000000000000000000000000000000000dead")
  val mint: Address          = Address.unsafeFrom("0x0000000000000000000000000000000000000000")
  val deadBeef: Address      = Address.unsafeFrom("0x00000000000000000000000000000000deadbeef")
  val cryptoKitties: Address = Address.unsafeFrom("0x06012c8cf97bead5deae237070f9587f8e7a266d")
