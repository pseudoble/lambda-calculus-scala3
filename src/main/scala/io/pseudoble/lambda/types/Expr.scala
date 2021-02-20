package io.pseudoble.lambda.types

enum Expr:
  case Literal(value: Boolean | Int | Double | BigDecimal | String)
  case Binding(name: String)
  case BindingRef(idx: Int)
  case FreeVar(name: String)
  case Func(binding: Binding, expression: Expr) 
  case App(left: Expr, right: Expr)

