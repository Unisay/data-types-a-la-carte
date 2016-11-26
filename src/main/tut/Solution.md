```tut:silent
sealed trait Expr[+F[_]]
case class In[F[_]](f: F[Expr[F]]) extends Expr[F]
```

```haskell
data Val e = Val Int
data Add e = Add e e

type IntExpr = Expr Val
type AddExpr = Expr Add
```

```tut:silent
case class Val[E](i: Int)
case class Add[E](l: E, r: E)

type IntExpr = Expr[Val]
type AddExpr = Expr[Add]

def value0(i: Int): IntExpr = In[Val](Val(i))
def add0(l: AddExpr, r: AddExpr): AddExpr = In[Add](Add(l, r))
```

```tut
value0(42)
add0(add0(null, add0(null, null)), add0(add0(null, null), null))
```

```haskell
data(f :+: g) e = Inl(fe) | Inr(ge)
```

```tut:silent
sealed trait :+:[F[_], G[_]]
case class Inl[F[_], G[_], E](fe: F[E]) extends :+:[F, G]
case class Inr[F[_], G[_], E](ge: G[E]) extends :+:[F, G]

type Exp = Val :+: Add

def val1(i: Int): Exp = Inl[Val, Add, Nothing](Val(i))
def add1(l: Exp, r: Exp): Exp = Inr[Val, Add, Exp](Add(l, r))
```

```tut
val exp: Exp = add1(add1(val1(42), val1(43)), val1(66))
```
