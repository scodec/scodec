package scodec

import shapeless._
import ops.hlist.RightFolder
import UnaryTCConstraint._

import scalaz.Applicative
import scalaz.syntax.Ops
import scalaz.syntax.applicative._


object HListFunctions {

  object applicativeFolder extends Poly2 {
    implicit def caseApplicative[A, B <: HList, F[_]](implicit app: Applicative[F]) = at[F[A], F[B]] {
      (a, b) => ^(a, b) { _ :: _ }
    }
  }

  def sequence[F[_]: Applicative, L <: HList: *->*[F]#λ, M <: HList](l: L)(implicit folder: RightFolder[L, F[HNil], applicativeFolder.type]) =
    l.foldRight(Applicative[F].point(HNil: HNil))(applicativeFolder)
}

object HListSyntax {
  implicit class HListSyntax[L <: HList](val self: L) extends Ops[L] {

    def sequence[F[_]: Applicative, M <: HList](implicit ev: *->*[F]#λ[L], folder: RightFolder[L, F[HNil], HListFunctions.applicativeFolder.type]) =
      HListFunctions.sequence(self)
  }
}
