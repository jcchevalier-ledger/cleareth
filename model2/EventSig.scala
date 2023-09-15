package cleareth.model2

abstract class EventSig(name: java.lang.String, args: EVMType*) extends Signature(name, args*)

object EventSig {}
/*
  /** Keccak256('Transfer(address,address,uint256)')
 * 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
 */
  case object Transfer extends Signature("Transfer", Address, Address, UInt256)

  /** Keccak256('Approval(address,address,uint256)')
 * 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
 */
  case object Approval extends Signature("Approval", Address, Address, UInt256)

  /** Keccak256('ApprovalForAll(address,address,bool)')
 * 0x17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31
 */
  case object ApprovalForAll extends Signature("ApprovalForAll", Address, Address, Boolean)

  /** Keccak256('TransferSingle(address,address,address,uint256,uint256)')
 * 0xc3d58168c5ae7397731d063d5bbf3d657854427343f4c083240f7aacaa2d0f62
 */
  case object TransferSingle extends Signature("TransferSingle", Address, Address, Address, UInt256, UInt256)

  /** Keccak256('TransferBatch(address,address,address,uint256[],uint256[])')
 * 0x4a39dc06d4c0dbc64b70af90fd698a233a518aa5d07e595d983b8c0526c8f7fb
 */
  case object TransferBatch extends Signature("TransferBatch", Address, Address, Address, ArrayOf(UInt256), ArrayOf(UInt256))

  /** Keccak256('Withdrawal(address,uint256)')
 * 0x7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65
 */
  case object Withdrawal extends Signature("Withdrawal", Address, UInt256)

  /** Keccak256('Deposit(address,uint256)')
 * 0xe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c
 */
  case object Deposit extends Signature("Deposit", Address, UInt256)
 */
