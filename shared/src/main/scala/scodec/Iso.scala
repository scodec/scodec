package scodec

import scala.deriving.Mirror

@annotation.implicitNotFound("""Could not prove ${A} is isomorphic to ${B}.""")
trait Iso[A, B] { self =>
  def to(a: A): B
  def from(b: B): A

  final def inverse: Iso[B, A] = new Iso[B, A] {
    def to(b: B) = self.from(b)
    def from(a: A) = self.to(a)
  }
}

private trait IsoLowPriority {
  def instance[A, B](t: A => B)(f: B => A): Iso[A, B] =
    new Iso[A, B] {
      def to(a: A) = t(a)
      def from(b: B) = f(b)
    }

  given inverse[A, B](using iso: Iso[A, B]) as Iso[B, A] = iso.inverse

  inline given productWithUnits[A <: Tuple, B](using 
    m: Mirror.ProductOf[B],
    ev: m.MirroredElemTypes =:= DropUnits[A]
  ) as Iso[A, B] = 
    instance((a: A) => fromTuple(DropUnits.drop(a)))(b => DropUnits.insert(toTuple(b)))

  protected def toTuple[A, B <: Tuple](a: A)(using m: Mirror.ProductOf[A], ev: m.MirroredElemTypes =:= B): B =
    Tuple.fromProduct(a.asInstanceOf[Product]).asInstanceOf[B]
  
  protected def fromTuple[A, B <: Tuple](b: B)(using m: Mirror.ProductOf[A], ev: m.MirroredElemTypes =:= B): A =
    m.fromProduct(b.asInstanceOf[Product]).asInstanceOf[A]
}

/** Companion for [[Iso]]. */
object Iso extends IsoLowPriority {

  /** Identity iso. */
  given id[A] as Iso[A, A] = instance[A, A](identity)(identity)

  given product[A <: Tuple, B](using m: Mirror.ProductOf[B], ev: m.MirroredElemTypes =:= A) as Iso[A, B] =
    instance[A, B](fromTuple)(toTuple)

  given singleton[A, B](using m: Mirror.ProductOf[B], ev: m.MirroredElemTypes =:= A *: EmptyTuple) as Iso[A, B] =
    instance[A, B](a => fromTuple(a *: EmptyTuple))(b => toTuple(b).head)
}
