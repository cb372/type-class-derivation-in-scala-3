package mylibrary

trait Show[A] {
  def (a: A).show: String
}

object Show {

  import scala.compiletime._
  import scala.deriving.Mirror

  private inline def summonAll[T <: Tuple]: List[Show[_]] = inline erasedValue[T] match {
    case _: Unit => Nil
    case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]
  }

  private inline def elemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: Unit => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: elemLabels[ts]
  }

  inline def derived[A](using m: Mirror.ProductOf[A]): Show[A] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    val productName = constValue[m.MirroredLabel]
    val labels = elemLabels[m.MirroredElemLabels]
    new Show[A] {
      def (a: A).show: String = {
        val prod = a.asInstanceOf[Product]
        val elements = labels.iterator zip prod.productIterator
        val elemStrings = (elements zip elemInstances).map {
          case ((name, value), instance) =>
            s"$name = ${instance.asInstanceOf[Show[Any]].show(value)}"
        }
        s"${productName}(${elemStrings.mkString(", ")})"
      }
    }
  }

  given Show[Int] = new Show[Int] {
    def (a: Int).show: String = a.toString
  }

  given Show[String] = new Show[String] {
    def (a: String).show: String = s""""$a""""
  }

}

