package myapp

import mylibrary.Functor

case class Foo[A](x: A, y: String) derives Functor
