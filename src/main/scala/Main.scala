import mylibrary._
import myapp._

object Main {

  val chris = Person("Chris", 180)
  val eric = Pet("Eric", "cat", chris)

  val foo = Foo(42, "hello")
  val oneTwoThree = MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))

  import ExprF._
  val expr =
    Add(
      Fix(Mult(
        Fix(Lit(2)),
        Fix(Lit(3)))),
      Fix(Lit(4)))

  type Algebra[F[_], A] = F[A] => A

  def cata[F[_]: Functor, A](fix: Fix[F])(alg: Algebra[F, A]): A =
    alg(fix.unFix.fmap(cata(_)(alg)))

  val evaluate: Algebra[ExprF, Int] = {
    case Lit(x) => x
    case Add(x, y) => x + y
    case Mult(x, y) => x * y
  }

  def showMe[A: Show](a: A): String =
    a.show

  def addOneToEach[F[_]: Functor](fa: F[Int]): F[Int] = fa.fmap(_ + 1)

  def main(args: Array[String]): Unit = {
    println(showMe(chris))
    println(showMe(eric))

    println(addOneToEach(oneTwoThree))
    println(addOneToEach(foo))
    println(foo.fmap(x => s"x is $x"))
    println(cata(Fix(expr))(evaluate))
  }


}
