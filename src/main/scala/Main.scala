import mylibrary._
import myapp._

object Main {

  val chris = Person("Chris", 180)
  val eric = Pet("Eric", "cat", chris)

  def showMe[A: Show](a: A): String =
    a.show

  def main(args: Array[String]): Unit = {
    println(showMe(chris))
    println(showMe(eric))
  }

}
