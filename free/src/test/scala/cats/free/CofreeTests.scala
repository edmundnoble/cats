package cats
package free

import cats.data.NonEmptyList
import cats.tests.CatsSuite
import cats.laws.discipline.{CartesianTests, SerializableTests}
import cats.syntax.list._

class CofreeTests extends CatsSuite {

  implicit val iso = CartesianTests.Isomorphisms.invariant[Cofree[Option, ?]]

//  checkAll("Cofree[Option, ?]", ComonadTests[Cofree[Option, ?]].comonad[Int, Int, Int])
  checkAll("Comonad[Cofree[Option, ?]]", SerializableTests.serializable(Comonad[Cofree[Option, ?]]))
//
//  test("decompile id") {
//    forAll { x: Cofree[List, Int] =>
//      Cofree.decompile[List, Cofree[List, ?], Int](x)(new (Cofree[List, ?] ~> List) {
//        def apply[A](fa: Cofree[List, A]): List[A] = fa.head :: fa.tail.flatMap(apply)
//      }) should ===(x)
//    }
//  }

}

object CofreeTests extends CofreeTestsInstances {

  type CofNel[A] = Cofree[Option, A]

  val nelToCofNel = new (NonEmptyList ~> CofNel) {
    override def apply[A](fa: NonEmptyList[A]): CofNel[A] =
      Cofree[Option, A](fa.head, Eval.later(fa.tail.toNel.map(apply)))
  }

  val cofNelToNel = new (CofNel ~> NonEmptyList) {
    override def apply[A](fa: CofNel[A]): NonEmptyList[A] =
      NonEmptyList[A](fa.head, fa.tail.fold[List[A]](Nil)(apply(_).toList))
  }

//  implicit def cofNelArbitrary[A: Arbitrary]: Arbitrary[CofNel[A]] =
//    cofNelArbitrary[Function0, A]

//  implicit def cofNelEq[A: Eq]: Eq[CofNel[A]] =
//    freeEq[Function0, A]
}

sealed trait CofreeTestsInstances {

//  import Arbitrary.arbFunction1

//  private def cofreeCogen[F[_] : SemigroupK, A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Cogen[Cofree[F, A]] = {
//    ???
//  }

  //  implicit def cofreeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
  //    Arbitrary(cofreeGen[F, A](4))

  //  implicit def cofreeEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
  //    new Eq[Free[S, A]] {
  //      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
  //        SA.eqv(a.runM(identity),  b.runM(identity))
  //    }
}
