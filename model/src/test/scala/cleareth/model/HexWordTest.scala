package cleareth.model

import munit.FunSuite
import scodec.bits.ByteVector

class HexWordTest extends FunSuite:

  test(s"creation from a valid hexadecimal string should succeed (length < 64)") {
    HexWord("0xdeadbeefdeadbeef")
  }

  test(s"creation from a valid hexadecimal string should succeed (`0x` string)") {
    HexWord("0x")
  }

  test(s"creation from a valid hexadecimal string should succeed (length = 64)") {
    HexWord("0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
  }

  test(s"creation from a valid hexadecimal string should fail (length > 64)") {
    interceptMessage[IllegalArgumentException](
      "Expecting at most 32 bytes"
    )(HexWord("0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"))
  }
