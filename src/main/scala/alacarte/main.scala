package alacarte

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

object main extends App {

  sealed trait Expr[F[_]]
  case class In[F[_]](f: F[Expr[F]]) extends Expr[F]

  case class Val[E](i: Int)
  case class Add[E](l: E, r: E)

  type IntExpr = Expr[Val]
  type AddExpr = Expr[Add]

  def value0(i: Int): IntExpr = In[Val](Val(i))
  def add0(l: AddExpr, r: AddExpr): AddExpr = In[Add](Add(l, r))

  println(value0(42))
  println(add0(add0(null, add0(null, null)), add0(add0(null, null), null)))

  sealed trait Coproduct[F[_], G[_], E]
  case class Inl[F[_], G[_], E](fe: F[E]) extends Coproduct[F, G, E]
  case class Inr[F[_], G[_], E](ge: G[E]) extends Coproduct[F, G, E]

  type Cop[E] = Coproduct[Val, Add, E]

  val addExample = In[Cop](Inr(Add(In[Cop](Inl(Val(118))), In[Cop](Inl(Val(1219))))))

  implicit val functorVal: Functor[Val] = new Functor[Val] {
    def map[A, B](v: Val[A])(f: A => B): Val[B] = Val[B](v.i)
  }

  implicit val functorAdd: Functor[Add] = new Functor[Add] {
    def map[A, B](add: Add[A])(f: A => B): Add[B] = Add(f(add.l), f(add.r))
  }

  implicit def functorCoproduct[F[_]: Functor, G[_]: Functor]: Functor[Coproduct[F, G, ?]] =
    new Functor[Coproduct[F, G, ?]] {
      def map[A, B](fga: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
        fga match {
          case Inl(fa) => Inl[F, G, B](fa map f)
          case Inr(ga) => Inr[F, G, B](ga map f)
        }
    }

  def foldExpr[F[_]: Functor, A](f: F[A] => A)(e: Expr[F]): A =
    e match {
      case In(t) => f(t map foldExpr(f))
    }

  trait Eval[F[_]] {
    implicit val functorEvidence: Functor[F]
    def evalAlgebra(f: F[Int]): Int
  }

  implicit val evalVal: Eval[Val] = new Eval[Val]() {
    def evalAlgebra(v: Val[Int]): Int = v.i
    val functorEvidence: Functor[Val] = implicitly[Functor[Val]]
  }

  implicit val evalAdd: Eval[Add] = new Eval[Add] {
    def evalAlgebra(a: Add[Int]): Int = a.l + a.r
    val functorEvidence: Functor[Add] = implicitly[Functor[Add]]
  }

  implicit def evalCop(implicit ev: Eval[Val], ea: Eval[Add]): Eval[Cop] =
    new Eval[Cop] {
      def evalAlgebra(exp: Cop[Int]): Int = exp match {
        case Inl(v) => ev.evalAlgebra(v)
        case Inr(a) => ea.evalAlgebra(a)
      }
      val functorEvidence: Functor[Cop] = implicitly[Functor[Cop]]
    }

  def eval[E[_]](e: Expr[E])(implicit ev: Eval[E]): Int = {
    import ev.functorEvidence
    foldExpr(ev.evalAlgebra)(e)
  }

  println(eval(addExample))

  // Automating injections

  trait :<:[A[_], B[_]] {
    def inj[E]: A[E] => B[E]
  }

  implicit def reflexive[F[_]](implicit fIsFunctor: Functor[F]): F :<: F =
    new (F :<: F) { def inj[E]: F[E] => F[E] = identity }

  implicit def injectF[F[_]: Functor, G[_]: Functor]: F :<: Coproduct[F, G, ?] =
    new (F :<: Coproduct[F, G, ?]) {
      def inj[E]: F[E] => Coproduct[F, G, E] = fe => Inl[F, G, E](fe)
    }

  implicit def injectFHG[
   F[_]: Functor,
   G[_]: Functor,
   H[_]: Functor
  ](implicit fg: F :<: G): (F :<: Coproduct[H, G, ?]) =
    new (F :<: Coproduct[H, G, ?]) {
      def inj[E]: F[E] => Coproduct[H, G, E] = fe => Inr[H, G, E](fg.inj(fe))
    }

  def inject[G[_], F[_]](gef: G[Expr[F]])(implicit ev: G :<: F): Expr[F] = In(ev.inj(gef))

  def value[F[_]](i: Int)(implicit ev: Val :<: F): Expr[F] = inject[Val, F](Val(i))

  implicit class ExprOps[F[_]](val left: Expr[F]) extends AnyVal {
    def ⊕(right: Expr[F])(implicit ev: Add :<: F): Expr[F] = inject(Add(left, right))
  }

  val x: Expr[Cop] = value[Cop](30000) ⊕ value(1330) ⊕ value(7)

  println(eval(x))
}

