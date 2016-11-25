package alacarte

import java.nio.file.Paths

import alacarte.main._
import reftree.core.RefTree.Ref
import reftree.core.ToRefTree
import reftree.render._
import reftree.diagram._

object Viewers {

  def render(name: String, caption: String, exp: Exp): Unit = {
    print("render: " + exp)
    renderer.render(name, Diagram(exp).withCaption(caption))
  }

  val renderer = Renderer(renderingOptions = RenderingOptions(density = 75), directory = Paths.get("img"))

  implicit def valToRefTree[E]: ToRefTree[Val[E]] = ToRefTree[Val[E]] { (v: Val[E]) =>
    Ref(
      name = "Val",
      id = id(v),
      children = List(implicitly[ToRefTree[Int]].refTree(v.i)),
      highlight = false,
      elide = false
    )
  }

  implicit def _addToRefTree[E]: ToRefTree[Add[E]] = ToRefTree[Add[E]] { (add: Add[E]) =>
    val t = implicitly[ToRefTree[Exp]]
    Ref(
      name = "Add",
      id = id(add),
      children = List(t.refTree(add.l.asInstanceOf[Exp]), t.refTree(add.r.asInstanceOf[Exp])),
      highlight = false,
      elide = false
    )
  }

  implicit val _expToRefTree: ToRefTree[Exp] = ToRefTree[Exp] { (exp: Exp) =>
    exp match {
      case inl @ Inl(v: Val[_]) =>
        Ref(name = "Inl", id = id(inl),
          children = Seq(implicitly[ToRefTree[Val[Any]]].refTree(v)),
          highlight = false, elide = false)
      case inr @ Inr(a: Add[_]) =>
        Ref(name = "Inr", id = id(inr),
          children = Seq(_addToRefTree.refTree(a)),
          highlight = false, elide = false)
    }
  }

  private def id(a: Any): String = System.identityHashCode(a).toString

}
