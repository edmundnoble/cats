package cats
package free

trait Freed[T[_], R, A] {
  val freelyGeneratedTypeclass: T[R]

  def retract(value: R)(implicit tc: T[A]): A = foldMap(value)(identity)

  def foldMap[B](value: R)(f: A => B)(implicit tc: T[B]): B
}

object Freed {
  implicit final def catsFreedInstance[T[_], R, F](implicit F: Freed[T, R, F]): T[R] = F.freelyGeneratedTypeclass
}
