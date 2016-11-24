package alacarte

import java.nio.file.Paths

import alacarte.Viewers.render
import reftree.core.RefTree.Ref
import reftree.core.{RefTree, ToRefTree}

import scala.language.higherKinds

object Example1 extends App {

  /*
     data Expr = Val Int | Add Expr Expr
  */
  sealed trait Expr
  case class Val(i: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr

  /*
     eval :: Expr -> Int
     eval (Val x)   = x
     eval (Add x y) = eval x + eval y
  */
  def eval(expr: Expr): Int = expr match {
    case Val(i) => i
    case Add(l, r) => eval(l) + eval(r)
  }

  eval { Add(Val(10000), Add(Val(499), Val(1))) }


  /*
     render :: Expr -> String
     render (Val x) = show x
     render (Add x y) = "(" ++ render x ++ "+" ++ render y ++ ")"
  */
  def render(expr: Expr): String = expr match {
    case Val(i) => i.toString
    case Add(l, r) => "(" + render(l) + "+" + render(r) + ")"
  }

  render { Add(Val(10000), Add(Val(499), Val(1))) }
}

object Example2 extends App {

  sealed trait Expr[+F[_]]
  case class In[F[_]](f: F[Expr[F]]) extends Expr[F]

  /*
    data Val e = Val Int
    data Add e = Add e e

    type IntExpr = Expr Val
    type AddExpr = Expr Add
   */

  case class Val[E](i: Int)
  case class Add[E](l: E, r: E)

  type IntExpr = Expr[Val]
  type AddExpr = Expr[Add]

  def value0(i: Int): IntExpr = In[Val](Val(i))
  def add0(l: AddExpr, r: AddExpr): AddExpr = In[Add](Add(l, r))

  println(value0(42))
  println(add0(add0(null, add0(null, null)), add0(add0(null, null), null)))


  /*
    data(f :+: g) e = Inl(fe) | Inr(ge)
  */

  sealed trait :+:[F[_], G[_]]
  case class Inl[F[_], G[_], E](fe: F[E]) extends :+:[F, G]
  case class Inr[F[_], G[_], E](ge: G[E]) extends :+:[F, G]

  type Exp = Val :+: Add

  def val1(i: Int): Exp = Inl[Val, Add, Nothing](Val(i))
  def add1(l: Exp, r: Exp): Exp = Inr[Val, Add, Exp](Add(l, r))

  val exp: Exp = add1(add1(val1(42), val1(43)), val1(66))
  render("exp", exp)



}

