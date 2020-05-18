package myapp

import mylibrary.Functor

enum MyList[+A] derives Functor {
  case Cons(head: A, tail: MyList[A])
  case Nil
}

