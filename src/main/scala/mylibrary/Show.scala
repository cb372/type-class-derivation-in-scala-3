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

  inline given derived[A](using m: Mirror.Of[A]) as Show[A] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[A]     => derivedForSum(s, elemInstances)
      case p: Mirror.ProductOf[A] => derivedForProduct(p, elemInstances)
    }
  }

  inline def derivedForSum[A](m: Mirror.SumOf[A], elemInstances: List[Show[_]]): Show[A] = {
    new Show[A] {
      def (a: A).show: String = {
        val i = m.ordinal(a)
        elemInstances(i).asInstanceOf[Show[Any]].show(a)
      }
    }
  }

  inline def derivedForProduct[A](m: Mirror.ProductOf[A], elemInstances: List[Show[_]]): Show[A] = {
    val productName = constValue[m.MirroredLabel]
    val labels = elemLabels[m.MirroredElemLabels]
    new Show[A] {
      def (a: A).show: String = {
        val elements = labels.iterator zip a.asInstanceOf[Product].productIterator
        val elemStrings = (elements zip elemInstances).map {
          case ((name, value), instance) =>
            s"$name = ${instance.asInstanceOf[Show[Any]].show(value)}"
        }
        s"${productName}(${elemStrings.mkString(", ")})"
      }
    }
  }

  given as Show[Int] {
    def (a: Int).show: String = a.toString
  }

  given as Show[String] {
    def (a: String).show: String = s""""$a""""
  }

}

