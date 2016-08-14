# umplc

```
letrec x:T = t1 in t2
```

desuger to

```
let x:T = fix (λx:T. t1) in t2
```

desuger to

```
(λx:T. t2) (fix (λx:T. t1))
```

if t1 and t2 represent the same term

```
fix (λx:T. e) = (λx:T. e) (fix (λx:T. e))
```

then entail the semantic of fix point

```
e[x → fix (λx:T. e)] ⇓ v
————————————————————————— (Fix)
  fix (λx:T. e) ⇓ v
```

and the type rule is pretty neat

```
  Γ[fix → ∀α. (α → α) → α, x → T] ⊢ e : T
———————————————————————————————————————————— (T-Fix)
            Γ ⊢ fix (λx:T. e) : T
```

lambda term can be easily construct use identifier, parameter type and body expression, so another AST structure to tackle fix point is also reasonable

```
e[x → fix x:T.e] ⇓ v
————————————————————————— (Fix)
  fix x:T.e ⇓ v
```

```
  Γ[fix → ∀α. (α → α) → α, x → T] ⊢ e : T
———————————————————————————————————————————— (T-Fix)
            Γ ⊢ fix x:T.e : T
```

