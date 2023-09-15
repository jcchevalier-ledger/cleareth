package cleareth.model

import munit.FunSuite
import scodec.bits.ByteVector

class AddressTest extends FunSuite {

  // String inputs
  val validString: String    = "0x5600cc0b56aca376ee5854403e06ba4a7480289f" // plz send NFTs
  val tooLongString: String  = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  val tooShortString: String = "0xdeadbeef"
  val nonHexString: String   = "0xgggggggggggggggggggggggggggggggggggggggg"

  // ByteVector inputs
  val validBv: ByteVector    = ByteVector.fromValidHex(validString)
  val tooLongBv: ByteVector  = ByteVector.fromValidHex(tooLongString)
  val tooShortBv: ByteVector = ByteVector.fromValidHex(tooShortString)

  test(s"creation from a valid hexadecimal string should succeed (length = 40)") { Address(validString) }
  test(s"creation from a valid ByteVector should succeed (length = 20)") { Address(validBv) }

  test(s"creation from an invalid hexadecimal string should fail (length > 40)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 20 bytes"
    )(Address(tooLongString))
  }

  test(s"creation from an invalid hexadecimal string should fail (length < 40)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 20 bytes"
    )(Address(tooShortString))
  }

  test(s"creation from an invalid ByteVector should fail (length > 20)") {
    intercept[IllegalArgumentException](Address(tooLongBv))
  }

  test(s"creation from an invalid ByteVector should fail (length < 20)") {
    intercept[IllegalArgumentException](Address(tooShortBv))
  }

  test(s"creation from a non-hexadecimal string should fail") {
    interceptMessage[IllegalArgumentException](
      s"Invalid hexadecimal character 'g' at index 2"
    )(Address(nonHexString))
  }
}
