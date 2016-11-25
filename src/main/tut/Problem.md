```haskell
data Expr = Val Int | Add Expr Expr
```

```tut:silent
sealed trait Expr
case class Val(i: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
```

```haskell
eval :: Expr -> Int
eval (Val x)   = x
eval (Add x y) = eval x + eval y
```

```tut
def eval(expr: Expr): Int = expr match {
    case Val(i) => i
    case Add(l, r) => eval(l) + eval(r)
}
```

```haskell
render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"
```

```tut:plain
def render(expr: Expr): String = expr match {
    case Val(i) => i.toString
    case Add(l, r) => "(" + render(l) + " + " + render(r) + ")"
}

val expression = Add(Val(10000), Add(Val(499), Val(1)))

eval(expression)
render(expression)
```
