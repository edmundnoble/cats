package cats

import cats.arrow.FunctionK

/**
  * Type constructors `R` which "freely" add
  * the higher-kinded type class constraint `T`
  * to the type constructor `F`.
  *
  * Most of the time, `F` appears as a type parameter in `R`:
  *   e.g., `Freed[Monad, Free[F, A], F]`: `Free[F, A]` is a free monad for all `A`, because:
  *           - no matter what `F[_]` is, Free[F[_], A] supports the `Monoid` interface
  *           - there's an injection `F[A] => Free[F, A]`
  *           - if `F[_]` is a monad, there's a folding function `Free[F, A] => F[A]`
  *
  */
trait FreedK[T[_[_]], R[_], F[_]] {
  val freelyGeneratedTypeclass: T[R]

  def retract[A](fv: R[A])(implicit ev: T[F]): F[A] = foldMap(fv)(FunctionK.id)

  def foldMap[A, G[_]](fv: R[A])(trans: FunctionK[F, G])(implicit ev: T[G]): G[A]

  def lift[A](value: F[A]): R[A]
}

object FreedK {
  implicit def catsFreedKInstance[T[_[_]], R[_], F[_]](implicit F: FreedK[T, R, F]): T[R] = F.freelyGeneratedTypeclass
}