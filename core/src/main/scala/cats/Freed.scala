package cats

/**
  * Data structures `R` which "freely" add
  * the type class constraint `T` to the type `A`.
  *
  * Most of the time, `A` appears as a type parameter in `R`:
  *   e.g., `Freed[Monoid, List[A], A]`: `List[A]` is a free monoid for all `A`, because:
  *           - no matter what `A` is, List[A] supports the `Monoid` interface
  *           - there's an injection `A => List[A]`
  *           - if `A` is a monoid, there's a folding function `List[A] => A`
  *
  */
trait Freed[T[_], R, A] {
  val freelyGeneratedTypeclass: T[R]

  def retract(value: R)(implicit tc: T[A]): A = foldMap(value)(identity)

  def foldMap[B](value: R)(f: A => B)(implicit tc: T[B]): B

  def lift(value: A): R
}

object Freed {
  implicit final def catsFreedInstance[T[_], R, F](implicit F: Freed[T, R, F]): T[R] = F.freelyGeneratedTypeclass
}