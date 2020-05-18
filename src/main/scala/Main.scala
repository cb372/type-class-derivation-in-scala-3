import mylibrary._
import myapp._

object Main {

  val chris = Person("Chris", 180)
  val eric = Pet("Eric", "cat", chris)

  val oneTwoThree = MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))

  def showMe[A: Show](a: A): String =
    a.show

   def addOneToEach[F[_]: Functor](fa: F[Int]): F[Int] = fa.fmap(_ + 1)

  def main(args: Array[String]): Unit = {
    println(showMe(chris))
    println(showMe(eric))

    println(addOneToEach(oneTwoThree))
  }

}
