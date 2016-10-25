package cats
package free

/**
  * A free comonad for some branching functor `S`. Branching is done lazily using Eval.
  * A tree with data at the branches, as opposed to Free which is a tree with data at the leaves.
  * Not an instruction set functor made into a program monad as in Free, but an instruction set's outputs as a
  * functor made into a functorful of the possible worlds reachable using the instruction set.
  */
final case class Cofree[S[_], A](head: A, tailEval: Eval[S[Cofree[S, A]]]) extends Product with Serializable {

  /** Evaluates and returns the tail of the computation. */
  def tailForced: S[Cofree[S, A]] = tailEval.value

  /** Applies `f` to the head and `g` to the tail. */
  def transform[B](f: A => B, g: Cofree[S, A] => Cofree[S, B])(implicit S: Functor[S]): Cofree[S, B] =
    Cofree[S, B](f(head), tailEval.map(S.map(_)(g)))

  /** Map over head and inner `S[_]` branches. */
  def map[B](f: A => B)(implicit S: Functor[S]): Cofree[S, B] =
    transform(f, _.map(f))

  /** Transform the branching functor at the root of the Cofree tree. */
  def mapBranchingRoot(nat: S ~> S)(implicit S: Functor[S]): Cofree[S, A] =
    Cofree[S, A](head, tailEval.map(nat(_)))

  /** Transform the branching functor, using the S functor to perform the recursion. */
  def mapBranchingS[T[_]](nat: S ~> T)(implicit S: Functor[S]): Cofree[T, A] =
    Cofree[T, A](head, tailEval.map(v => nat(S.map(v)(_.mapBranchingS(nat)))))

  /** Transform the branching functor, using the T functor to perform the recursion. */
  def mapBranchingT[T[_]](nat: S ~> T)(implicit T: Functor[T]): Cofree[T, A] =
    Cofree[T, A](head, tailEval.map(v => T.map(nat(v))(_.mapBranchingT(nat))))

  /** Map `f` over each subtree of the computation. */
  def coflatMap[B](f: Cofree[S, A] => B)(implicit S: Functor[S]): Cofree[S, B] =
    Cofree[S, B](f(this), tailEval.map(S.map(_)(_.coflatMap(f))))

  /** Replace each node in the computation with the subtree from that node downwards */
  def coflatten(implicit S: Functor[S]): Cofree[S, Cofree[S, A]] =
    Cofree[S, Cofree[S, A]](this, tailEval.map(S.map(_)(_.coflatten)))

  /** Alias for head. */
  def extract: A = head

  /** Evaluate just the tail. */
  def forceTail: Cofree[S, A] =
    Cofree[S, A](head, Eval.now(tailEval.value))

  /** Evaluate the entire Cofree tree. */
  def forceAll(implicit S: Functor[S]): Cofree[S, A] =
    Cofree[S, A](head, Eval.now(tailEval.map(S.map(_)(_.forceAll)).value))

}

object Cofree extends CofreeInstances {

  /** Cofree anamorphism, lazily evaluated. */
  def unfold[F[_], A](a: A)(f: A => F[A])(implicit F: Functor[F]): Cofree[F, A] =
    Cofree[F, A](a, Eval.later(F.map(f(a))(unfold(_)(f))))

  /** Cofree anamorphism, with customizable evaluation strategy. */
  def unfoldStrategy[F[_], A](a: A)(f: A => F[A],
                                    strategy: (=> F[Cofree[F, A]]) => Eval[F[Cofree[F, A]]])(implicit F: Functor[F]): Cofree[F, A] =
    Cofree[F, A](a, strategy(F.map(f(a))(unfold(_)(f))))

  /** Decompile a comonadic value into another functor's cofree comonad. */
  def decompile[F[_], W[_], A](wa: W[A])(f: W ~> F)(implicit W: Comonad[W]): Cofree[F, A] =
    Cofree[F, A](W.extract(wa), Eval.later(f(W.coflatMap(wa)(decompile(_)(f)))))

  /** Decompile a comonadic value into another functor's cofree comonad. */
  def decompileStrategy[F[_], W[_], A](wa: W[A])
                                      (f: W ~> F, strategy: (=> F[Cofree[F, A]]) => Eval[F[Cofree[F, A]]])
                                      (implicit W: Comonad[W]): Cofree[F, A] =
    Cofree[F, A](W.extract(wa), strategy(f(W.coflatMap(wa)(decompile(_)(f)))))

}

sealed abstract class CofreeInstances4 {
  /** low priority `Foldable1` instance */
  implicit def cofreeFoldable[F[_] : Foldable]: Foldable[Cofree[F, ?]] =
  new CofreeFoldable[F] {
    def F = implicitly
  }
}

sealed abstract class CofreeInstances3 extends CofreeInstances4 {
  /** low priority `Traverse1` instance */
  implicit def cofreeTraverse[F[_] : Traverse]: Traverse[Cofree[F, ?]] =
  new CofreeTraverse[F] {
    def F = implicitly
  }
}

sealed abstract class CofreeInstances2 extends CofreeInstances3 {
  implicit def cofreeFlatMap[F[_] : SemigroupK : Functor]: FlatMap[Cofree[F, ?]] =
    new CofreeFlatMap[F] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class CofreeInstances0 extends CofreeInstances2 {
  implicit def cofreeMonad[F[_] : MonoidK : Functor]: Monad[Cofree[F, ?]] =
    new CofreeMonad[F] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class CofreeInstances extends CofreeInstances0 {
  implicit def cofreeComonad[S[_] : Functor]: Comonad[Cofree[S, ?]] = new CofreeComonad[S] {
    def F = implicitly
  }
}

private trait CofreeComonad[S[_]] extends Comonad[Cofree[S, ?]] {
  implicit def F: Functor[S]

  override final def extract[A](p: Cofree[S, A]): A = p.extract
  override final def coflatMap[A, B](a: Cofree[S, A])(f: Cofree[S, A] => B): Cofree[S, B] = a.coflatMap(f)
  override final def coflatten[A](a: Cofree[S, A]): Cofree[S, Cofree[S, A]] = a.coflatten
  override final def map[A, B](a: Cofree[S, A])(f: A => B): Cofree[S, B] = a.map(f)
}

private trait CofreeFlatMap[F[_]] extends FlatMap[Cofree[F, ?]] with CofreeComonad[F] {
  implicit def F: Functor[F]

  implicit def G: SemigroupK[F]

  override final def flatMap[A, B](fa: Cofree[F, A])(f: A => Cofree[F, B]): Cofree[F, B] = {
    val c = f(fa.head)
    Cofree[F, B](c.head, c.tailEval.map(G.combineK(_, F.map(fa.tailForced)(flatMap(_)(f)))))
  }
  override def tailRecM[A, B](a: A)(f: A => Cofree[F, Either[A, B]]): Cofree[F, B] = {
    Cofree(a, f())
  }
}

private trait CofreeMonad[F[_]] extends Monad[Cofree[F, ?]] with CofreeFlatMap[F] {
  implicit def G: MonoidK[F]

  override final def pure[A](a: A): Cofree[F, A] = Cofree[F, A](a, Eval.now(G.empty))
}

private trait CofreeFoldable[F[_]] extends Foldable[Cofree[F, ?]] {
  implicit def F: Foldable[F]

  override final def foldMap[A, B](fa: Cofree[F, A])(f: A => B)(implicit M: Monoid[B]): B =
    M.combine(f(fa.head), F.foldMap(fa.tailForced)(foldMap(_)(f)))

  override final def foldRight[A, B](fa: Cofree[F, A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    f(fa.head, fa.tailEval.flatMap(F.foldRight(_, z)(foldRight(_, _)(f))))

  override final def foldLeft[A, B](fa: Cofree[F, A], z: B)(f: (B, A) => B): B =
    F.foldLeft(fa.tailForced, f(z, fa.head))((b, cof) => foldLeft(cof, b)(f))

}

private trait CofreeTraverse[F[_]] extends Traverse[Cofree[F, ?]] with CofreeFoldable[F] with CofreeComonad[F] {
  implicit def F: Traverse[F]

  override final def traverse[G[_], A, B](fa: Cofree[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Cofree[F, B]] =
    G.map2(f(fa.head), F.traverse(fa.tailForced)(traverse(_)(f)))((h, t) => Cofree[F, B](h, Eval.now(t)))

}

