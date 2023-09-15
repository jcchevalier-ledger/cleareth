package cleareth.model

import cleareth.model.TxHash
import munit.FunSuite
import scodec.bits.ByteVector

class TxHashTest extends FunSuite {

  // String inputs
  val validString: String    = "0x011c4b789027239e20e768827751eed2d66a58af80a4d64165700ca5d8a15d32"
  val tooLongString: String  = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  val tooShortString: String = "0xdeadbeefdeadbeefdeadbeef"
  val nonHexString: String   = "0xgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"

  // ByteVector inputs
  val validBv: ByteVector    = ByteVector.fromValidHex(validString)
  val tooLongBv: ByteVector  = ByteVector.fromValidHex(tooLongString)
  val tooShortBv: ByteVector = ByteVector.fromValidHex(tooShortString)

  test(s"creation from a valid hexadecimal string should succeed (length = 64)") { TxHash(validString) }
  test(s"creation from a valid ByteVector should succeed (length = 32)") { TxHash(validBv) }

  test(s"creation from an invalid hexadecimal string should fail (length > 64)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 32 bytes"
    )(TxHash(tooLongString))
  }

  test(s"creation from an invalid hexadecimal string should fail (length < 64)") {
    interceptMessage[IllegalArgumentException](
      s"Expecting 32 bytes"
    )(TxHash(tooShortString))
  }

  test(s"creation from an invalid ByteVector should fail (length > 32)") {
    intercept[IllegalArgumentException](TxHash(tooLongBv))
  }

  test(s"creation from an invalid ByteVector should fail (length < 32)") {
    intercept[IllegalArgumentException](TxHash(tooShortBv))
  }

  test(s"creation from a non-hexadecimal string should fail") {
    interceptMessage[IllegalArgumentException](
      s"Invalid hexadecimal character 'g' at index 2"
    )(TxHash(nonHexString))
  }
}
