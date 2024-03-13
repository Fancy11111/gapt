tff(l1,type,
    l1: $real ).

tff(f1,type,
    f1: $real > $real ).

tff(l,type,
    l: $real ).

%% Assertions:
%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l1) ∧ ((if ((x - l1) ≥ 0.0) (x - l1) else -(x - l1)) < delta)) ⇒ ((if ((f1(x) - l) ≥ 0.0) (f1(x) - l) else -(f1(x) - l)) < epsilon))))
tff(formula_1,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l1 )
                & ( $greatereq($difference(X,l1),0.0)
                 => $less($difference(X,l1),Delta) )
                & ( ~ $greatereq($difference(X,l1),0.0)
                 => $less($uminus($difference(X,l1)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l),0.0)
                 => $less($difference(f1(X),l),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l),0.0)
                 => $less($uminus($difference(f1(X),l)),Epsilon) ) ) ) ) ) ).
