package cleareth.model

import scodec.bits.ByteVector

enum HexFormat:
  case Raw, Prefixed

  def format(bytes: ByteVector): String =
    this match
      case Raw      => bytes.toHex
      case Prefixed => s"0x${bytes.toHex}"
