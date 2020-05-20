package mylibrary

trait Functor[F[_]] {
  def [A, B] (fa: F[A]).map(f: A => B): F[B]
}

object Functor {

  import scala.compiletime._
  import scala.deriving.Mirror

  type Id[t] = t
  type Const[c] = [t] =>> c

  given Functor[Id] {
    def [A, B] (a: A).map(f: A => B): B = f(a)
  }

  given [T] as Functor[Const[T]] {
    def [A, B] (t: T).map(f: A => B): T = t
  }

  object Helpers {

    inline def summonAsArray[T <: Tuple]: Array[Any] =
      summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

    inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
      case _: Unit => arr
      case _: (a *: b) =>
        arr(i) = summonInline[a]
        summonAsArray0[b](i+1, arr)
    }

    final class ArrayProduct(val elems: Array[Any]) extends Product {
      def canEqual(that: Any): Boolean = true
      def productElement(n: Int) = elems(n)
      def productArity = elems.length
      override def productIterator: Iterator[Any] = elems.iterator
    }

    type PolyMirror[C, O[_]] = C { type MirroredType = O ; type MirroredElemTypes[_] }
    type MirrorOf[O[_]] = PolyMirror[Mirror, O]
    type ProductMirrorOf[O[_]] = PolyMirror[Mirror.Product, O]
    type CoproductMirrorOf[O[_]] = PolyMirror[Mirror.Sum, O]

    case class Wrap[T](t: T)
    class Dummy
    type Apply[F[_]] = F[Dummy]
    type Unapply[T] = T match {
      case Wrap[Apply[a]] => Functor[a]
      case Wrap[Dummy] => Functor[Id]
      case Wrap[c] => Functor[Const[c]]
    }

    type Functors[T[_]] = Functors0[Apply[T]]

    type Functors0[T] <: Tuple = T match {
      case Unit => Unit
      case (a *: b) => Unapply[Wrap[a]] *: Functors0[b]
    }

  }

  import Helpers._

  case class LazyWrapper[F[_]](functor: Functor[F])

  given derived[F[_]](using wrapper: => LazyWrapper[F]) as Functor[F] {
    def [A, B] (fa: F[A]).map(f: A => B): F[B] =
      wrapper.functor.map(fa)(f)
  }

  inline given [F[_]](using mirror: MirrorOf[F]) as LazyWrapper[F] = {
    val functors = summonAsArray[Functors[mirror.MirroredElemTypes]]
    inline mirror match {
      case p: ProductMirrorOf[F] => derivedForProduct[F](p, functors)
      case c: CoproductMirrorOf[F] => derivedForCoproduct[F](c, functors)
    }
  }

  inline def derivedForProduct[F[_]](m: ProductMirrorOf[F], elemFunctors: Array[Any]): LazyWrapper[F] =
    LazyWrapper(
      new Functor[F] {
        def [A, B] (fa: F[A]).map(f: A => B): F[B] = {
          val n = elemFunctors.length
          if (n == 0) fa.asInstanceOf[F[B]]
          else {
            val arr = new Array[Any](n)
            var i = 0
            while(i < n) {
              val F: Functor[_] = elemFunctors(i).asInstanceOf
              val elem: Any = fa.asInstanceOf[Product].productElement(i)
              arr(i) = F.map(elem.asInstanceOf)(f)
              i = i+1
            }
            m.fromProduct(ArrayProduct(arr)).asInstanceOf[F[B]]
          }
        }
      }
    )

  inline def derivedForCoproduct[F[_]](m: CoproductMirrorOf[F], elemFunctors: Array[Any]): LazyWrapper[F] =
    LazyWrapper(
      new Functor[F] {
        def [A, B] (fa: F[A]).map(f: A => B): F[B] = {
          val i = m.ordinal(fa.asInstanceOf)
          elemFunctors(i).asInstanceOf[Functor[F]].map(fa)(f)
        }
      }
    )

}
