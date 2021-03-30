package io.pseudoble.lambda.types

enum Expr:
  case Binding(name: String)
  case BindingRef(idx: Int)
  case FreeVar(name: String)
  case Func(binding: Binding, expression: Expr)
  case App(left: Expr, right: Expr)


opaque type Binder = String
opaque type BindRef = Int
opaque type FreeVar = String
enum ExprTerm:
  case Var(idx: BindRef | FreeVar)
  case Func(binding: Binder, expression: ExprTerm)
  case App(left: ExprTerm, right: ExprTerm)

