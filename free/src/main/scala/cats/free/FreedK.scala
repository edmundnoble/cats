package cats
package free

import cats.arrow.FunctionK

trait FreedK[R[_], T[_[_]], F[_]] {
  val freelyGeneratedTypeclass: T[R]

  def retract[A](fv: R[A])(implicit ev: T[F]): F[A] = foldMap(fv)(FunctionK.id)

  def foldMap[A, G[_]](fv: R[A])(trans: FunctionK[F, G])(implicit ev: T[G]): G[A]
}

object FreedK {
  implicit def catsFreedKInstance[T[_[_]], R[_], F[_]](implicit F: FreedK[R, T, F]): T[R] = F.freelyGeneratedTypeclass
}
