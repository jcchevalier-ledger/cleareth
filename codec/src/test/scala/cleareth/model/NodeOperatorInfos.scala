package cleareth.model

import cleareth.decoding.EvmDecoder
import cleareth.encoding.EvmEncoder
import scodec.bits.ByteVector

final case class NodeOperatorInfos(
    active: Boolean,
    name: String,
    rewardAddress: Address,
    stakingLimit: Wei,
    stoppedValidators: BigInt,
    totalSigningKeys: BigInt,
    usedSigningKeys: BigInt
) derives EvmEncoder,
      EvmDecoder

object NodeOperatorInfos:
  val encodedNodeOperatorInfos: Seq[String] = List(
    "0000000000000000000000000000000000000000000000000000000000000001", // boolean
    "00000000000000000000000000000000000000000000000000000000000000e0", // pointer to string
    "000000000000000000000000dd4bc51496dc93a0c47008e820e0d80745476f22", // address
    "00000000000000000000000000000000000000000000000000000000000020d0", // staking limit: uint256
    "0000000000000000000000000000000000000000000000000000000000000000", // stopped validators: uint256
    "00000000000000000000000000000000000000000000000000000000000020d0", // total signing keys: uint256
    "0000000000000000000000000000000000000000000000000000000000001cdf", // used signing keys: uint256
    // dynamic types are encoded AFTER the fixed length types
    // a string is a dynamic type encoded over two 32 bytes words with first the length then the content
    // ➜ abi.encodePacked("Staking Facilities")
    //   Type: dynamic bytes
    //   ├ Hex (Memory):
    //   ├─ Length ([0x00:0x20]): 0x0000000000000000000000000000000000000000000000000000000000000012
    //   ├─ Contents ([0x20:..]): 0x5374616b696e6720466163696c69746965730000000000000000000000000000
    //   ├ Hex (Tuple Encoded):
    //   ├─ Pointer ([0x00:0x20]): 0x0000000000000000000000000000000000000000000000000000000000000020
    //   ├─ Length ([0x20:0x40]): 0x0000000000000000000000000000000000000000000000000000000000000012
    //   └─ Contents ([0x40:..]): 0x5374616b696e6720466163696c69746965730000000000000000000000000000
    "0000000000000000000000000000000000000000000000000000000000000012", // string length
    "5374616b696e6720466163696c69746965730000000000000000000000000000"  // string
  )

  val decodedNodeOperatorInfos: NodeOperatorInfos = NodeOperatorInfos(
    active = true,
    name = "Staking Facilities",
    rewardAddress = Address(ByteVector.fromValidHex("0xdd4bc51496dc93a0c47008e820e0d80745476f22")),
    stakingLimit = Wei(8400),
    stoppedValidators = 0,
    totalSigningKeys = 8400,
    usedSigningKeys = 7391
  )
