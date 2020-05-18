package mylibrary

trait Functor[F[_]] {
  def [A, B] (fa: F[A]).fmap(f: A => B): F[B]
}

object Functor {

  //given Functor[List] = new Functor[List] {
  //  def [A, B] (fa: List[A]).fmap(f: A => B): List[B] = fa.map(f)
  //}

  import scala.compiletime._
  import scala.deriving.Mirror

  //private inline def summonAll[T[_] <: Tuple]: List[Functor[_]] = erasedValue[T[Int]] match {
    //case _: Unit => Nil
    //case _: (t[Int] *: ts) => summonInline[Functor[t]] :: summonAll[ts]
  //}

  //private inline def foo[T <: Tuple] = inline erasedValue[T] match {
  //  case _: Unit => 1
  //  case _: (t *: ts) => summonInline[Functor[t]]
  //}


  //private def derivedForSum[F[_]](s: Mirror.SumOf[F[_]], elems: List[Functor[_]]): Functor[F] =
    //new Functor[F] {
      //def [A, B] (fa: F[A]).fmap(f: A => B): F[B] = {
        //elems(s.ordinal(fa)).fmap(fa)(f)
      //}
    //}

  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)

  given Functor[Id] {
    def [A, B] (a: A).fmap(f: A => B): B = f(a)
  }

  given [T] as Functor[Const[T]] {
    def [A, B] (t: T).fmap(f: A => B): T = t
  }

  class Dummy
  type Apply[T[_]] = T[Dummy]
  type Unapply[F[_[_]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy] => F[Id]
    case Wrap[c] => F[Const[c]]
  }

  type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_]], T] <: Tuple = T match {
    case Unit => Unit
    case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
  }

  type Kind[C, O[_]] = C { type MirroredType = O ; type MirroredElemTypes[_] }
  type Generic[O[_]] = Kind[Mirror, O]
  type ProductGeneric[O[_]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_]] = Kind[Mirror.Sum, O]

  type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

  inline def summon[T] = summonFrom {
    case t: T => t
  }

  inline def summonAsArray[T <: Tuple]: Array[Any] =
    summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

  inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
    case _: Unit => arr
    case _: (a *: b) =>
      arr(i) = summon[a]
      summonAsArray0[b](i+1, arr)
  }

  abstract class ErasedInstances[FT] {
    def erasedMap(x: Any)(f: (Any, Any) => Any): Any
  }

  abstract class ErasedProductInstances[FT] extends ErasedInstances[FT] {
    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any
  }

  final class ErasedProductInstances1[FT](val mirror: Mirror.Product, i: Any) extends ErasedProductInstances[FT] {
    final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any =
      mirror.fromProduct(Tuple1(f(i, x0.asInstanceOf[Product].productElement(0))))
  }

  final class ErasedProductInstancesN[FT](val mirror: Mirror.Product, is: Array[Any]) extends ErasedProductInstances[FT] {
    import ErasedProductInstances.ArrayProduct

    final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
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

    inline def summonOne[T] = inline erasedValue[T] match {
      case _: Tuple1[a] => summon[a]
    }

    val emptyArray: Array[Any] = new Array(0)

    inline def apply[FT, E <: Tuple](mirror: Mirror.Product): ErasedProductInstances[FT] =
      inline erasedValue[Tuple.Size[E]] match {
        case 0 => new ErasedProductInstancesN[FT](mirror, emptyArray)
        case 1 => new ErasedProductInstances1[FT](mirror, summonOne[E])
        case _ => new ErasedProductInstancesN[FT](mirror, summonAsArray[E])
      }
  }

  inline given mkInstances[F[_[_]], T[_]](using gen: Generic[T]) as Instances[F, T] =
    inline gen match {
      case p: ProductGeneric[T] => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)
    }

  inline given mkProductInstances[F[_[_]], T[_]](using gen: ProductGeneric[T]) as ProductInstances[F, T] =
    ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_]], T[_]](using gen: CoproductGeneric[T]) as CoproductInstances[F, T] =
    ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  final class ErasedCoproductInstances[FT](mirror: Mirror.Sum, is0: => Array[Any]) extends ErasedInstances[FT] {
    lazy val is = is0

    def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

    def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
      val i = ordinal(x)
      f(i, x)
    }

  }

  object ErasedCoproductInstances {
    inline def apply[FT, E <: Tuple](mirror: Mirror.Sum) : ErasedCoproductInstances[FT] =
      new ErasedCoproductInstances[FT](mirror, summonAsArray[E])
  }

  inline def [F[_[_]], T[_], A, R](inst: Instances[F, T]) map(x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
    inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

  given derived[F[_]](using inst: => Instances[Functor, F]) as Functor[F] {
    def [A, B] (fa: F[A]).fmap(f: A => B): F[B] =
      inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.fmap(ta)(f))
  }


}
