package cleareth.model

import cleareth.model.EVMType.*

abstract class MethodSig(name: java.lang.String, args: EVMType*) extends Signature(name, args*):
  val selector: java.lang.String = "0x" + signature.take(4).toHex

object MethodSig:

  /** Keccak256('Implementation()') 0xe6a8ee4f18f4a3a97bd2b32c0d655b264d93e073ccf4cee4570a8ebc81926917
    */
  case object Implementation extends Signature("Implementation")
