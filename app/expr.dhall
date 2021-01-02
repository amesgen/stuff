let ExprF =
      λ(Expr : Type) →
        < LitF : Natural
        | AddF : { _1 : Expr, _2 : Expr }
        | MulF : { _1 : Expr, _2 : Expr }
        >

let Expr = ∀(Expr : Type) → (ExprF Expr → Expr) → Expr

let Lit
    : Natural → Expr
    = λ(n : Natural) →
      λ(Expr : Type) →
      λ(make : ExprF Expr → Expr) →
        make ((ExprF Expr).LitF n)

let Add
    : Expr → Expr → Expr
    = λ(x : Expr) →
      λ(y : Expr) →
      λ(Expr : Type) →
      λ(make : ExprF Expr → Expr) →
        make ((ExprF Expr).AddF { _1 = x Expr make, _2 = y Expr make })

let Mul
    : Expr → Expr → Expr
    = λ(x : Expr) →
      λ(y : Expr) →
      λ(Expr : Type) →
      λ(make : ExprF Expr → Expr) →
        make ((ExprF Expr).MulF { _1 = x Expr make, _2 = y Expr make })

in  { Expr, Lit, Add, Mul }
