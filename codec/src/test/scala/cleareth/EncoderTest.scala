package cleareth

import cleareth.*
import cleareth.encoding.*
import cleareth.model.Dummy
import cleareth.model.EvmCase
import cleareth.model.EvmCase2
import cleareth.model.NodeOperatorInfos
import munit.FunSuite

class EncoderTest extends FunSuite:
  test("encode case class properly"):
      assertEquals(
        EvmEncoder[NodeOperatorInfos](NodeOperatorInfos.decodedNodeOperatorInfos).toWords.map(_.toHex),
        NodeOperatorInfos.encodedNodeOperatorInfos
      )

  test("encode string properly"):
      val input = "Liquid staked Ether 2.0"
      val expected: Seq[String] = List(
        "0000000000000000000000000000000000000000000000000000000000000020",
        "0000000000000000000000000000000000000000000000000000000000000017",
        "4c6971756964207374616b656420457468657220322e30000000000000000000"
      )
      assertEquals(EvmEncoder[String](input).toWords.map(_.toHex), expected)

  test("encode this string properly too"):
      val input = "Hello, World!"
      val expected: Seq[String] = List(
        "0000000000000000000000000000000000000000000000000000000000000020",
        "000000000000000000000000000000000000000000000000000000000000000d",
        "48656c6c6f2c20576f726c642100000000000000000000000000000000000000"
      )
      assertEquals(EvmEncoder[String](input).toWords.map(_.toHex), expected)

  test("should work with trickier cases too"):
      val testCaseClass = Dummy(string1 = "Staking Facilities", string2 = "Staking Facilities", 20)
      val expected = List(
        "0000000000000000000000000000000000000000000000000000000000000060",
        "00000000000000000000000000000000000000000000000000000000000000a0",
        "0000000000000000000000000000000000000000000000000000000000000014",
        "0000000000000000000000000000000000000000000000000000000000000012",
        "5374616b696e6720466163696c69746965730000000000000000000000000000",
        "0000000000000000000000000000000000000000000000000000000000000012",
        "5374616b696e6720466163696c69746965730000000000000000000000000000"
      )
      assertEquals(EvmEncoder[Dummy](testCaseClass).toWords.map(_.toHex), expected)

  test("Should work with int list"):
      val input = List(1, 2, 3)

      val expected = List(
        "0000000000000000000000000000000000000000000000000000000000000020", // offset
        "0000000000000000000000000000000000000000000000000000000000000003", // length
        "0000000000000000000000000000000000000000000000000000000000000001", // first element
        "0000000000000000000000000000000000000000000000000000000000000002", // second element
        "0000000000000000000000000000000000000000000000000000000000000003"  // third element
      )
      assertEquals(EvmEncoder[List[Int]](input).toWords.map(_.toHex), expected)

  test("Should work with string list"):
      val input = List("test", "de JC", "& Yohan")

      val expected = List(
        "0000000000000000000000000000000000000000000000000000000000000020", // offset
        "0000000000000000000000000000000000000000000000000000000000000003", // # of elems
        "0000000000000000000000000000000000000000000000000000000000000060", // first string offset
        "00000000000000000000000000000000000000000000000000000000000000a0", // second string offset
        "00000000000000000000000000000000000000000000000000000000000000e0", // third string offset
        "0000000000000000000000000000000000000000000000000000000000000004", // first string length
        "7465737400000000000000000000000000000000000000000000000000000000", // first string
        "0000000000000000000000000000000000000000000000000000000000000005", // second string length
        "6465204a43000000000000000000000000000000000000000000000000000000", // second string
        "0000000000000000000000000000000000000000000000000000000000000007", // third string length
        "2620596f68616e00000000000000000000000000000000000000000000000000"  // third string
      )
      assertEquals(EvmEncoder[List[String]](input).toWords.map(_.toHex), expected)

  test("evm case"):
      assertEquals(EvmEncoder[EvmCase](EvmCase.testCase).toWords.map(_.toHex), EvmCase.words)

  test("evm case 2"):
      assertEquals(EvmEncoder[EvmCase2](EvmCase2.testCase).toWords.map(_.toHex), EvmCase2.words)
