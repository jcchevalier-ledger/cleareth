package cleareth.model

import munit.FunSuite
import scodec.bits.ByteVector

class BlockHashTest extends FunSuite {

  // String inputs
  val validString: String    = "0x2ccc7ae145c091bef4aa0da65562c1f5249d774185522f12507986be66cdf1d2"
  val tooLongString: String  = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  val tooShortString: String = "0xdeadbeefdeadbeefdeadbeef"
  val nonHexString: String   = "0xgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"

  // ByteVector inputs
  val validBv: ByteVector    = ByteVector.fromValidHex(validString)
  val tooLongBv: ByteVector  = ByteVector.fromValidHex(tooLongString)
  val tooShortBv: ByteVector = ByteVector.fromValidHex(tooShortString)

  test(s"creation from a valid hexadecimal string should succeed (length = 64)") { BlockHash(validString) }
  test(s"creation from a valid ByteVector should succeed (length = 32)") { BlockHash(validBv) }

  test(s"creation from an invalid hexadecimal string should fail (length > 64)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 32 bytes"
    )(BlockHash(tooLongString))
  }

  test(s"creation from an invalid hexadecimal string should fail (length < 64)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 32 bytes"
    )(BlockHash(tooShortString))
  }

  test(s"creation from an invalid ByteVector should fail (length > 32)") {
    intercept[IllegalArgumentException](BlockHash(tooLongBv))
  }

  test(s"creation from an invalid ByteVector should fail (length < 32)") {
    intercept[IllegalArgumentException](BlockHash(tooShortBv))
  }

  test(s"creation from a non-hexadecimal string should fail") {
    interceptMessage[IllegalArgumentException](
      s"Invalid hexadecimal character 'g' at index 2"
    )(BlockHash(nonHexString))
  }
}
