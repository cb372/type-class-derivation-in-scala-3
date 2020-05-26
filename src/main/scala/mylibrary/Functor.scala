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
    type Unapply2[T] = T match {
      case Wrap[Apply[a]] => a
    }

    type Functors[T[_]] = Functors0[Apply[T]]

    type Functors0[T] <: Tuple = T match {
      case Unit => Unit
      case (a *: b) => Unapply[Wrap[a]] *: Functors0[b]
    }

  }

  import Helpers._

  case class LazyWrapper[F[_]](functor: Functor[F])

  //def derived[F[_]](using wrapper: => LazyWrapper[F]): Functor[F] = new Functor[F] {
  //  def [A, B] (fa: F[A]).map(f: A => B): F[B] =
  //    wrapper.functor.map(fa)(f)
  //}

  //inline given [F[_]](using mirror: MirrorOf[F]) as LazyWrapper[F] = {
  //  inline mirror match {
  //    case p: ProductMirrorOf[F] => derivedForProduct[F](p)
  //    case c: CoproductMirrorOf[F] => derivedForCoproduct[F](c)
  //  }
  //}

  inline def derived[F[_]](using mirror: MirrorOf[F]): Functor[F] = new Functor[F] {
    def [A, B] (fa: F[A]).map(f: A => B): F[B] = {
      inline mirror match {
        case p: ProductMirrorOf[F] => mapForProduct[F, A, B](p, fa, f)
        case c: CoproductMirrorOf[F] => mapForCoproduct[F, A, B](c, fa, f)
      }
    }
  }

  inline def mapForProduct[F[_], A, B](m: ProductMirrorOf[F], fa: F[A], f: A => B): F[B] = {
    val elemFunctors = summonAsArray[Functors[m.MirroredElemTypes]]
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

  private inline def rec[F[_], T, A, B](n: Int, ord: Int, fa: F[A], f: A => B): F[B] = {
    inline erasedValue[T] match {
      case _: (t *: ts) =>
        if (n == ord) {
          type x[A] = t
          summonFrom {
            case p: ProductMirrorOf[`x`] => mapForProduct[x, A, B](p, fa.asInstanceOf[x[A]], f).asInstanceOf[F[B]]
          }
        } else {
          rec[F, ts, A, B](n + 1, ord, fa, f)
        }
    }
  }

  inline def mapForCoproduct[F[_], A, B](m: CoproductMirrorOf[F], fa: F[A], f: A => B): F[B] = {
    val ord = m.ordinal(fa.asInstanceOf[m.MirroredMonoType])
    rec[F, m.MirroredElemTypes, A, B](0, ord, fa, f).asInstanceOf[F[B]]
  }

  //inline def derivedForProduct[F[_]](m: ProductMirrorOf[F]): LazyWrapper[F] = {
  //  val elemFunctors = summonAsArray[Functors[m.MirroredElemTypes]]
  //  LazyWrapper(
  //    new Functor[F] {
  //      def [A, B] (fa: F[A]).map(f: A => B): F[B] = {
  //        val n = elemFunctors.length
  //        if (n == 0) fa.asInstanceOf[F[B]]
  //        else {
  //          val arr = new Array[Any](n)
  //          var i = 0
  //          while(i < n) {
  //            val F: Functor[_] = elemFunctors(i).asInstanceOf
  //            val elem: Any = fa.asInstanceOf[Product].productElement(i)
  //            arr(i) = F.map(elem.asInstanceOf)(f)
  //            i = i+1
  //          }
  //          m.fromProduct(ArrayProduct(arr)).asInstanceOf[F[B]]
  //        }
  //      }
  //    }
  //  )
  //}

  //private inline def rec[F[_], T](n: Int, ord: Int): LazyWrapper[F] = {
  //  inline erasedValue[T] match {
  //    case _: (t *: ts) =>
  //      if (n == ord) {
  //        type x[_] = Unapply2[Wrap[t]]
  //        summonFrom {
  //          case p: ProductMirrorOf[x] => derivedForProduct[x](p).asInstanceOf[LazyWrapper[F]]
  //        }
  //      } else {
  //        rec[F, ts](n + 1, ord)
  //      }
  //  }
  //}

  //inline def derivedForCoproduct[F[_]](m: CoproductMirrorOf[F]): LazyWrapper[F] =
  //  LazyWrapper(
  //    new Functor[F] {
  //      def [A, B] (fa: F[A]).map(f: A => B): F[B] = {
  //        val ord = m.ordinal(fa.asInstanceOf)
  //        rec[F, m.MirroredElemTypes](0, ord).functor.map(fa)(f)
  //      }
  //    }
  //  )

}
