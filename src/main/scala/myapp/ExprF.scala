package myapp

import mylibrary.Functor

enum ExprF[A] derives Functor {
  case Lit(x: Int)
  case Add(x: A, y: A)
  case Mult(x: A, y: A)
}

final case class Fix[F[_]](unFix: F[Fix[F]])
