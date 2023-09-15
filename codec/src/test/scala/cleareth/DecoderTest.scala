package cleareth

import cleareth.decoding.*
import cleareth.model.*
import munit.FunSuite
import scodec.bits.ByteVector

class DecoderTest extends FunSuite:
  test("should work with basic case class"):
      assertEquals(
        EvmDecoder[NodeOperatorInfos](ByteVector.fromValidHex(NodeOperatorInfos.encodedNodeOperatorInfos.mkString)),
        Right(NodeOperatorInfos.decodedNodeOperatorInfos)
      )

  test("decode this string properly"):
      val input: ByteVector = ByteVector.fromValidHex(
        "0000000000000000000000000000000000000000000000000000000000000020" +
          "000000000000000000000000000000000000000000000000000000000000000d" +
          "48656c6c6f2c20576f726c642100000000000000000000000000000000000000"
      )
      val expected = "Hello, World!"
      assertEquals(EvmDecoder[String](input), Right(expected))

  test("should not work when trying to decode a NodeOperatorInfos as a Dummy instance"):
      assertEquals(
        EvmDecoder[Dummy](ByteVector.fromValidHex(encodedNodeOperatorInfos.mkString)),
        Left(
          DecodingError(
            "invalid dynamic offset. it should be >= decoding offset (32)",
            ByteVector.fromValidHex("0x01").padLeft(32)
          )
        )
      )

  test("evm case"):
      assertEquals(EvmDecoder[EvmCase](ByteVector.fromValidHex(EvmCase.words.mkString)), Right(EvmCase.testCase))

  test("evm case 2"):
      assertEquals(EvmDecoder[EvmCase2](ByteVector.fromValidHex(EvmCase2.words.mkString)), Right(EvmCase2.testCase))

  test("evm case 3"):
      assertEquals(EvmDecoder[EvmCase3](ByteVector.fromValidHex(EvmCase3.words.mkString)), Right(EvmCase3.testCase))

  test("evm case 4"):
      assertEquals(EvmDecoder[EvmCase4](ByteVector.fromValidHex(EvmCase4.words.mkString)), Right(EvmCase4.testCase))
