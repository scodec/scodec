package scodec

import language.experimental.macros

import scala.reflect.macros.Context

/** Macros that support binary and hexadecimal literals. */
object LiteralSyntaxMacros {

  def binStringInterpolator(c: Context)(args: c.Expr[BitVector]*): c.Expr[BitVector] = {
    import c.universe._

    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val partLiterals: List[String] = parts map {
      case Literal(Constant(part: String)) =>
        if (BitVector.fromBin(part).isLeft)
          c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
        part
    }

    val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
    val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
    val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
      case (sb, (arg, part)) =>
        val partExpr = c.Expr[String](Literal(Constant(part)))
        reify { sb.splice.append(arg.splice.toBin).append(partExpr.splice) }
    }

    reify { BitVector.fromValidBin(stringBuilder.splice.toString) }
  }

  def hexStringInterpolator(c: Context)(args: c.Expr[ByteVector]*): c.Expr[ByteVector] = {
    import c.universe._

    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val partLiterals: List[String] = parts map {
      case Literal(Constant(part: String)) =>
        if (ByteVector.fromHex(part).isLeft)
          c.error(c.enclosingPosition, "hexadecimal string literal may only contain characters [0-9a-fA-f]")
        part
    }

    val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
    val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
    val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
      case (sb, (arg, part)) =>
        val partExpr = c.Expr[String](Literal(Constant(part)))
        reify { sb.splice.append(arg.splice.toHex).append(partExpr.splice) }
    }

    reify { ByteVector.fromValidHex(stringBuilder.splice.toString) }
  }

}
