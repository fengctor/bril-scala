package bril.analysis.local.extensions

import bril.analysis.local._

final case class Extension(
  preLookup: (SerializedExpression, Table) => SerializedExpression,
  postLookup: (SerializedExpression, Table) => SerializedExpression,
  argConversion: (String, Table) => String
)
