package bril.analysis.local.extensions

import bril.analysis.local._

final case class Extension(
  preLookup: (SerializedExpression, Table) => SerializedExpression,
  postLookup: (SerializedExpression, Table) => SerializedExpression,
  argConversion: (String, Table) => String
)

object Extension {
  // Identity extension
  val idExtension: Extension = Extension(
    (serExpr: SerializedExpression, _: Table) => serExpr,
    (serExpr: SerializedExpression, _: Table) => serExpr,
    (arg: String, _: Table) => arg
  )

  // Produces the extension that uses ext1 first, then ext2
  def pipe(ext1: Extension, ext2: Extension): Extension = {
    def pipeTabled[T](f1: (T, Table) => T, f2: (T, Table) => T): (T, Table) => T = {
      (t: T, table: Table) => f2(f1(t, table), table)
    }
    Extension(
      pipeTabled(ext1.preLookup, ext2.preLookup),
      pipeTabled(ext1.postLookup, ext2.postLookup),
      pipeTabled(ext1.argConversion, ext2.argConversion),
    )
  }

  def pipeAll(exts: Extension*): Extension = exts match {
    case Nil => idExtension
    case e +: es => es.foldLeft(e)((acc, cur) => pipe(cur, acc))
  }
}
