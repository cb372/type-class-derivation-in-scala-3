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
    def erasedMap(fa: Any)(f: Any => Any): Any

    def map[F[_], A, B](fa: F[A])(f: A => B): F[B] =
      erasedMap(fa)(f.asInstanceOf).asInstanceOf
  }

  private final class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  final class DerivedProductFunctor[FuncF](mirror: Mirror.Product, elemFunctors: Array[Any]) extends DerivedFunctor[FuncF] {

    def erasedMap(fa: Any)(f: Any => Any): Any = {
      val n = elemFunctors.length
      if (n == 0) fa
      else {
        val arr = new Array[Any](n)
        var i = 0
        while(i < n) {
          val F: Functor[_] = elemFunctors(i).asInstanceOf
          val elem: Any = fa.asInstanceOf[Product].productElement(i)
          arr(i) = F.map(elem.asInstanceOf)(f)
          i = i+1
        }
        mirror.fromProduct(ArrayProduct(arr))
      }
    }
  }

  object DerivedProductFunctor {
    inline def apply[F[_], E <: Tuple](mirror: Mirror.Product): DerivedProductFunctor[Functor[F]] =
      new DerivedProductFunctor[Functor[F]](mirror, summonAsArray[E])
  }

  final class DerivedCoproductFunctor[FuncF](mirror: Mirror.Sum, elemFunctors: Array[Any]) extends DerivedFunctor[FuncF] {

    def erasedMap(fa: Any)(f: Any => Any): Any = {
      val F: Functor[_] = elemFunctors(mirror.ordinal(fa.asInstanceOf)).asInstanceOf
      F.map(fa.asInstanceOf)(f)
    }

  }

  object DerivedCoproductFunctor {
    inline def apply[F[_], E <: Tuple](mirror: Mirror.Sum): DerivedCoproductFunctor[Functor[F]] =
      new DerivedCoproductFunctor[Functor[F]](mirror, summonAsArray[E])
  }

  type PolyMirror[C, O[_]] = C { type MirroredType = O ; type MirroredElemTypes[_] }
  type MirrorOf[O[_]] = PolyMirror[Mirror, O]
  type ProductMirrorOf[O[_]] = PolyMirror[Mirror.Product, O]
  type CoproductMirrorOf[O[_]] = PolyMirror[Mirror.Sum, O]

  inline given mkDerived[F[_]](using mirror: MirrorOf[F]) as Derived[F] =
    inline mirror match {
      case p: ProductMirrorOf[F] => mkProductDerived[F](using p)
      case c: CoproductMirrorOf[F] => mkCoproductDerived[F](using c)
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

  inline given mkProductDerived[F[_]](using mirror: ProductMirrorOf[F]) as ProductDerived[F] =
    DerivedProductFunctor[F, LiftP[mirror.MirroredElemTypes]](mirror)

  inline given mkCoproductDerived[F[_]](using mirror: CoproductMirrorOf[F]) as CoproductDerived[F] =
    DerivedCoproductFunctor[F, LiftP[mirror.MirroredElemTypes]](mirror)

  given derived[F[_]](using derivedFunctor: => Derived[F]) as Functor[F] {
    def [A, B] (fa: F[A]).map(f: A => B): F[B] =
      derivedFunctor.map(fa)(f)
  }

}
