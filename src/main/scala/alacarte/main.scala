package alacarte

import alacarte.Viewers.render
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

  type Exp[E] = Coproduct[Val, Add, E]

  def val1[E](i: Int): Exp[E] = Inl[Val, Add, E](Val(i))
  def add1[E](l: Exp[E], r: Exp[E]): Exp[Exp[E]] = Inr[Val, Add, Exp[E]](Add[Exp[E]](l, r))

  val exp = add1(add1(val1(42), val1(43)), val1(66))
  render("exp", "add1(add1(val1(42), val1(43)), val1(66))", exp)

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

}

