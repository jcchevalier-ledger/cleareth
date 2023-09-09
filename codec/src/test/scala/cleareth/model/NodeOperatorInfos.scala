package cleareth.model

import cleareth.decoding.EvmDecoder
import cleareth.encoding.EvmEncoder

final case class NodeOperatorInfos(
    active: Boolean,
    name: String,
    rewardAddress: Address,
    stakingLimit: BigInt,
    stoppedValidators: BigInt,
    totalSigningKeys: BigInt,
    usedSigningKeys: BigInt
) derives EvmEncoder,
      EvmDecoder
