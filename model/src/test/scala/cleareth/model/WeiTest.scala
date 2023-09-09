package cleareth.model

import cleareth.model.Wei
import munit.FunSuite
import scodec.bits.ByteVector

class WeiTest extends FunSuite {

  // String inputs
  val validString: String    = "0x011c4b789027239e20e768827751eed2d66a58af80a4d64165700ca5d8a15d32"
  val tooLongString: String  = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  val tooShortString: String = "0xdeadbeefdeadbeefdeadbeef"
  val nonHexString: String   = "0xgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"

  // ByteVector inputs
  val validBv: ByteVector    = ByteVector.fromValidHex(validString)
  val tooLongBv: ByteVector  = ByteVector.fromValidHex(tooLongString)
  val tooShortBv: ByteVector = ByteVector.fromValidHex(tooShortString)

  test(s"creation from a valid hexadecimal string should succeed (length = 64)") { Wei(validString) }
  test(s"creation from a valid hexadecimal string should succeed (length < 64)") { Wei(tooShortString) }
  test(s"creation from a valid ByteVector should succeed (length = 32)") { Wei(validBv) }
  test(s"creation from a valid ByteVector should succeed (length < 32)") { Wei(tooShortBv) }

  test(s"creation from an invalid hexadecimal string should fail (length > 64)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 256 bits"
    )(Wei(tooLongString))
  }

  test(s"creation from an invalid ByteVector should fail (length > 32)") {
    intercept[IllegalArgumentException](Wei(tooLongBv))
  }

  test(s"creation from a non-hexadecimal string should fail") {
    interceptMessage[IllegalArgumentException](
      s"Invalid hexadecimal character 'g' at index 2"
    )(Wei(nonHexString))
  }
}
