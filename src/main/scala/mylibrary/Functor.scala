package mylibrary

trait Functor[F[_]] {
  def [A, B] (fa: F[A]).fmap(f: A => B): F[B]
}

object Functor {

  import scala.compiletime._
  import scala.deriving.Mirror

  type Id[t] = t
  type Const[c] = [t] =>> c

  given Functor[Id] {
    def [A, B] (a: A).fmap(f: A => B): B = f(a)
  }

  given [T] as Functor[Const[T]] {
    def [A, B] (t: T).fmap(f: A => B): T = t
  }

  case class Wrap[T](t: T)
  class Dummy
  type Apply[F[_]] = F[Dummy]
  type Unapply[T] = T match {
    case Wrap[Apply[a]] => Functor[a]
    case Wrap[Dummy] => Functor[Id]
    case Wrap[c] => Functor[Const[c]]
  }

  type LiftP[T[_]] = LiftP0[Apply[T]]

  type LiftP0[T] <: Tuple = T match {
    case Unit => Unit
    case (a *: b) => Unapply[Wrap[a]] *: LiftP0[b]
  }

  type Kind[C, O[_]] = C { type MirroredType = O ; type MirroredElemTypes[_] }
  type Generic[O[_]] = Kind[Mirror, O]
  type ProductGeneric[O[_]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_]] = Kind[Mirror.Sum, O]

  type Instances[F[_]] = ErasedInstances[Functor[F]]
  type ProductInstances[F[_]] = ErasedProductInstances[Functor[F]]
  type CoproductInstances[F[_]] = ErasedCoproductInstances[Functor[F]]

  inline def summonAsArray[T <: Tuple]: Array[Any] =
    summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

  inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
    case _: Unit => arr
    case _: (a *: b) =>
      arr(i) = summonInline[a]
      summonAsArray0[b](i+1, arr)
  }

  abstract class ErasedInstances[FT] {
    def erasedMap(x: Any)(f: (Any, Any) => Any): Any

    def map[F[_], A, B](fa: F[A])(f: [t[_]] => (Functor[t], t[A]) => t[B]): F[B] =
      erasedMap(fa)(f.asInstanceOf).asInstanceOf
  }

  final class ErasedProductInstances[FT](val mirror: Mirror.Product, is: Array[Any]) extends ErasedInstances[FT] {
    import ErasedProductInstances.ArrayProduct

    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
      val n = is.length
      if (n == 0) x0
      else {
        val x = x0.asInstanceOf[Product]
        val arr = new Array[Any](n)
        var i = 0
        while(i < n) {
          arr(i) = f(is(i), x.productElement(i))
          i = i+1
        }
        mirror.fromProduct(ArrayProduct(arr))
      }
    }
  }

  object ErasedProductInstances {
    class ArrayProduct(val elems: Array[Any]) extends Product {
      def canEqual(that: Any): Boolean = true
      def productElement(n: Int) = elems(n)
      def productArity = elems.length
      override def productIterator: Iterator[Any] = elems.iterator
    }

    inline def apply[FT, E <: Tuple](mirror: Mirror.Product): ErasedProductInstances[FT] =
      new ErasedProductInstances[FT](mirror, summonAsArray[E])
  }

  final class ErasedCoproductInstances[FT](mirror: Mirror.Sum, is0: => Array[Any]) extends ErasedInstances[FT] {
    lazy val is = is0

    def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

    def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
      val i = ordinal(x)
      f(i, x)
    }

  }

  object ErasedCoproductInstances {
    inline def apply[FT, E <: Tuple](mirror: Mirror.Sum): ErasedCoproductInstances[FT] =
      new ErasedCoproductInstances[FT](mirror, summonAsArray[E])
  }

  inline given mkInstances[F[_]](using gen: Generic[F]) as Instances[F] =
    inline gen match {
      case p: ProductGeneric[F] => mkProductInstances[F](using p)
      case c: CoproductGeneric[F] => mkCoproductInstances[F](using c)
    }

  inline given mkProductInstances[F[_]](using gen: ProductGeneric[F]) as ProductInstances[F] =
    ErasedProductInstances[Functor[F], LiftP[gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_]](using gen: CoproductGeneric[F]) as CoproductInstances[F] =
    ErasedCoproductInstances[Functor[F], LiftP[gen.MirroredElemTypes]](gen)

  given derived[F[_]](using inst: Instances[F]) as Functor[F] {
    def [A, B] (fa: F[A]).fmap(f: A => B): F[B] =
      inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.fmap(ta)(f))
  }

}
