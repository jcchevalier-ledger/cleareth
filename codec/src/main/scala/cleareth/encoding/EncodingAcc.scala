package cleareth.encoding

import cats.kernel.Monoid
import scodec.bits.ByteVector

final case class EncodingAcc(static: ByteVector, dynamic: ByteVector):
  def merge: ByteVector = static ++ dynamic

object EncodingAcc:

  val empty: EncodingAcc = EncodingAcc(ByteVector.empty, ByteVector.empty)

  def static(static: ByteVector): EncodingAcc                       = EncodingAcc(static, ByteVector.empty)
  def dynamic(static: ByteVector, dynamic: ByteVector): EncodingAcc = EncodingAcc(static, dynamic)
  def method(sig: ByteVector): EncodingAcc                          = EncodingAcc(sig, ByteVector.empty)

  given Monoid[EncodingAcc] = new Monoid[EncodingAcc]:
    override def empty: EncodingAcc = EncodingAcc.empty
    override def combine(x: EncodingAcc, y: EncodingAcc): EncodingAcc =
      EncodingAcc(x.static ++ y.static, x.dynamic ++ y.dynamic)
