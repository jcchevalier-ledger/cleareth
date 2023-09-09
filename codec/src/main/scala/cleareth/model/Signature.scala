package cleareth.model

import org.bouncycastle.jcajce.provider.digest.Keccak
import scodec.bits.ByteVector

abstract private class Signature(name: String, args: EVMType*):
  protected val signature: ByteVector =
    val description = name + args.mkString(",") // + args.map(_.name).mkString("(", ",", ")")
    ByteVector.view(description.getBytes).digest(new Keccak.Digest256())

  val topic: String = "0x" + signature.toHex
