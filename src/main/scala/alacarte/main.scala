package alacarte

//import alacarte.Viewers.render
import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

object main extends App {

  sealed trait Expr[F[_]]
  case class In[F[_]](f: F[Expr[F]]) extends Expr[F]
  def in[F[_]](f: F[Expr[F]]): Expr[F] = In[F](f)

  case class Val[E](i: Int)
  case class Add[E](l: E, r: E)

  type IntExpr = Expr[Val]
  type AddExpr = Expr[Add]

  def value0(i: Int): IntExpr = in[Val](Val(i))
  def add0(l: AddExpr, r: AddExpr): AddExpr = in[Add](Add(l, r))

  println(value0(42))
  println(add0(add0(null, add0(null, null)), add0(add0(null, null), null)))

  sealed trait Coproduct[F[_], G[_], E]
  case class Inl[F[_], G[_], E](fe: F[E]) extends Coproduct[F, G, E]
  case class Inr[F[_], G[_], E](ge: G[E]) extends Coproduct[F, G, E]

  def inl[F[_], G[_], E](fe: F[E]): Coproduct[F, G, E] = Inl[F, G, E](fe)
  def inr[F[_], G[_], E](ge: G[E]): Coproduct[F, G, E] = Inr[F, G, E](ge)
  type Cop[E] = Coproduct[Val, Add, E]

  val addExample = in[Cop](inr(Add(in[Cop](inl(Val(11))), in[Cop](inl(Val(42))))))

//  render("addExample", "add1(add1(val1(42), val1(43)), val1(66))", addExample)

  implicit val ValFunctor: Functor[Val] = new Functor[Val] {
    def map[A, B](v: Val[A])(f: A => B): Val[B] = Val[B](v.i)
  }

  implicit val AddFunctor: Functor[Add] = new Functor[Add] {
    def map[A, B](add: Add[A])(f: A => B): Add[B] = Add(f(add.l), f(add.r))
  }

  implicit def ExpFunctor[F[_]: Functor, G[_]: Functor]: Functor[Coproduct[F, G, ?]] =
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

  implicit val EvalVal: Eval[Val] = new Eval[Val]() {
    def evalAlgebra(v: Val[Int]): Int = v.i
    val functorEvidence: Functor[Val] = implicitly[Functor[Val]]
  }

  implicit val EvalAdd: Eval[Add] = new Eval[Add] {
    def evalAlgebra(a: Add[Int]): Int = a.l + a.r
    val functorEvidence: Functor[Add] = implicitly[Functor[Add]]
  }

  implicit def EvalExp(implicit ev: Eval[Val], ea: Eval[Add]): Eval[Cop] =
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

}

