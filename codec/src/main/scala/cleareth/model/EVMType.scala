package cleareth.model

import scodec.*
import scodec.bits.*

sealed trait EVMType:
  def encode: ByteVector

object EVMType:
  sealed trait Dynamic
  sealed trait Static

  case class Address(value: java.lang.String) extends EVMType with Static:
    def encode: ByteVector = ByteVector.fromValidHex(value)

  case class String(value: java.lang.String) extends EVMType with Dynamic:
    def encode: ByteVector =
      val length  = UInt256(BigInt(value.length)).encode
      val content = ByteVector(value.getBytes)
      length ++ content

  case class UInt256(value: BigInt) extends EVMType with Static:
    override def encode: ByteVector = ByteVector(value.toByteArray)

  private case class Acc(
      offset: Int,
      statics: ByteVector,
      dynamics: ByteVector
  ):
    val value: ByteVector = statics ++ dynamics

  private object Acc:
    def init(offset: Int): Acc = Acc(offset, ByteVector.empty, ByteVector.empty)

  def encodeSeq(xs: List[EVMType]): ByteVector =
    val offset = xs.length * 32
    xs.foldLeft(Acc.init(offset)) { (acc, value) =>
      value match
        case static: Static =>
          acc.copy(statics = acc.statics ++ static.encode)
        case dyn: Dynamic =>
          val encoded          = dyn.encode
          val additionalOffset = (encoded.length / 32).toInt
          acc.copy(
            offset = acc.offset + additionalOffset,
            statics = acc.statics ++ UInt256(BigInt(acc.offset)).encode,
            dynamics = acc.dynamics ++ encoded
          )
    }.value

//    val valuesOrOffset =  xs.map {
//
//    }
//    xs.collect { case dyn: Dynamic }.foldLeft(offset) { (off, f) =>

// (Offset, List[Dynamic])      => (List[Offset], ByteVector)
// (List[Fields], List[Offset]) => ByteVector

// trait HexEncoder[T, EVMT <: EVMType]
