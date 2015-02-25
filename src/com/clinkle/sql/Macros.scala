package com.clinkle.sql

import language.experimental.macros
import scala.reflect.macros.blackbox

private[sql] object Macros {
  def column(c: blackbox.Context)(primitive: c.Expr[Primitive[_]]): c.Tree = {
    import c.universe._
    val typeArg = c.macroApplication.tpe.typeArgs(0)
    val assigneeName = c.internal.enclosingOwner.name.toString.trim
    val macroName = c.macroApplication.toString.split(Array('[', '(')).head.split('.').last

    q"new Column[$typeArg]($assigneeName, $macroName)($primitive)"
  }

  def column_length(c: blackbox.Context)(length: c.Expr[Int])(primitive: c.Tree) = {
    import c.universe._
    val typeArg = c.macroApplication.tpe.typeArgs(0)
    val assigneeName = c.internal.enclosingOwner.name.toString.trim
    val macroName = c.macroApplication.toString.split(Array('[', '(')).head.split('.').last

    val decl = s"$macroName(%d)"

    q"new Column[$typeArg]($assigneeName, $decl.format($length))($primitive)"
  }

  def alias_impl(c: blackbox.Context): c.Tree = {
    import c.universe._

    val assigneeName = c.internal.enclosingOwner.name.toString.trim

    q"new com.clinkle.sql.Alias($assigneeName)"
  }
}
