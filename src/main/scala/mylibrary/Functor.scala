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

  type PolyMirror[C, O[_]] = C { type MirroredType = O ; type MirroredElemTypes[_] }
  type Generic[O[_]] = PolyMirror[Mirror, O]
  type ProductGeneric[O[_]] = PolyMirror[Mirror.Product, O]
  type CoproductGeneric[O[_]] = PolyMirror[Mirror.Sum, O]

  type Derived[F[_]] = DerivedFunctor[Functor[F]]
  type ProductDerived[F[_]] = DerivedProductFunctor[Functor[F]]
  type CoproductDerived[F[_]] = DerivedCoproductFunctor[Functor[F]]

  inline def summonAsArray[T <: Tuple]: Array[Any] =
    summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

  inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
    case _: Unit => arr
    case _: (a *: b) =>
      arr(i) = summonInline[a]
      summonAsArray0[b](i+1, arr)
  }

  abstract class DerivedFunctor[FuncF] {
    def erasedMap(fa: Any)(f: (Any, Any) => Any): Any

    def map[F[_], A, B](fa: F[A])(f: [t[_]] => (Functor[t], t[A]) => t[B]): F[B] =
      erasedMap(fa)(f.asInstanceOf).asInstanceOf
  }

  final class DerivedProductFunctor[FuncF](val mirror: Mirror.Product, is: Array[Any]) extends DerivedFunctor[FuncF] {
    import DerivedProductFunctor.ArrayProduct

    def erasedMap(fa: Any)(f: (Any, Any) => Any): Any = {
      val n = is.length
      if (n == 0) fa
      else {
        val arr = new Array[Any](n)
        var i = 0
        while(i < n) {
          arr(i) = f(is(i), fa.asInstanceOf[Product].productElement(i))
          i = i+1
        }
        mirror.fromProduct(ArrayProduct(arr))
      }
    }
  }

  object DerivedProductFunctor {
    class ArrayProduct(val elems: Array[Any]) extends Product {
      def canEqual(that: Any): Boolean = true
      def productElement(n: Int) = elems(n)
      def productArity = elems.length
      override def productIterator: Iterator[Any] = elems.iterator
    }

    inline def apply[FuncF, E <: Tuple](mirror: Mirror.Product): DerivedProductFunctor[FuncF] =
      new DerivedProductFunctor[FuncF](mirror, summonAsArray[E])
  }

  final class DerivedCoproductFunctor[FuncF](mirror: Mirror.Sum, is0: => Array[Any]) extends DerivedFunctor[FuncF] {
    lazy val is = is0

    def erasedMap(fa: Any)(f: (Any, Any) => Any): Any = {
      val i = is(mirror.ordinal(fa.asInstanceOf))
      f(i, fa)
    }

  }

  object DerivedCoproductFunctor {
    inline def apply[FuncF, E <: Tuple](mirror: Mirror.Sum): DerivedCoproductFunctor[FuncF] =
      new DerivedCoproductFunctor[FuncF](mirror, summonAsArray[E])
  }

  inline given mkDerived[F[_]](using gen: Generic[F]) as Derived[F] =
    inline gen match {
      case p: ProductGeneric[F] => mkProductDerived[F](using p)
      case c: CoproductGeneric[F] => mkCoproductDerived[F](using c)
    }

  inline given mkProductDerived[F[_]](using gen: ProductGeneric[F]) as ProductDerived[F] =
    DerivedProductFunctor[Functor[F], LiftP[gen.MirroredElemTypes]](gen)

  inline given mkCoproductDerived[F[_]](using gen: CoproductGeneric[F]) as CoproductDerived[F] =
    DerivedCoproductFunctor[Functor[F], LiftP[gen.MirroredElemTypes]](gen)

  given derived[F[_]](using derivedFunctor: Derived[F]) as Functor[F] {
    def [A, B] (fa: F[A]).fmap(f: A => B): F[B] =
      derivedFunctor.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.fmap(ta)(f))
  }

}
