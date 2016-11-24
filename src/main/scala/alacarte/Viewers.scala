package alacarte

import java.nio.file.Paths

import alacarte.Example2._
import reftree.core.RefTree.Ref
import reftree.core.ToRefTree
import reftree.render._
import reftree.diagram._

object Viewers {

  def render(name: String, exp: Exp): Unit = renderer.render(name, Diagram(exp))

  val renderer = Renderer(renderingOptions = RenderingOptions(density = 75), directory = Paths.get("img"))

  implicit def valToRefTree[E]: ToRefTree[Val[E]] = ToRefTree[Val[E]] { (v: Val[E]) =>
    Ref(
      name = "val",
      id = "val",
      children = List(implicitly[ToRefTree[Int]].refTree(v.i)),
      highlight = false,
      elide = false
    )
  }

  implicit def _addToRefTree[E]: ToRefTree[Add[E]] = ToRefTree[Add[E]] { (add: Add[E]) =>
    val t = implicitly[ToRefTree[Exp]]
    Ref(
      name = "add",
      id = "add",
      children = List(t.refTree(add.l.asInstanceOf[Exp]), t.refTree(add.r.asInstanceOf[Exp])),
      highlight = false,
      elide = false
    )
  }

  implicit val _expToRefTree: ToRefTree[Exp] = ToRefTree[Exp] { (exp: Exp) =>
    exp match {
      case Inl(v: Val[_]) =>
        Ref(name = "inl", id = "inl", children = Seq(implicitly[ToRefTree[Val[Any]]].refTree(v)), highlight = false, elide = false)
      case Inr(a: Add[_]) =>
        Ref(name = "inr", id = "inr", children = Seq(implicitly[ToRefTree[Add[Any]]].refTree(a)), highlight = false, elide = false)
    }
  }

}
