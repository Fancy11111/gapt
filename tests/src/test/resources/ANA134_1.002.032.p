%------------------------------------------------------------------------------
% File     : ANA134_1.002.032 : TPTP v8.2.0. Released v8.2.0.
% Domain   : Number theory
% Problem  : composition_tower_f2_sz32__1
% Version  : Especial.
% English  : lim[x -> l1](f_i1(x)) ~ l
%            lim[x -> l2](f_i2(x)) ~ l1
%            lim[x -> l3](f_i3(x)) ~ l2
%            ...
%            lim[x -> l31](f_i31(x)) ~ l30
%            lim[x -> a](f_i32(x)) ~ l31
%            ============================
%            lim[x -> a](f_i1(f_i2(f_i3(...f_i32(x)...)))) ~ l
%            where
%            - 2 functions with arity 1
%            - 32 number of function applications

% Refs     : [Sch22] Schoisswohl (2022), Email to G. Sutcliffe
%          : [KK+23] Korovin et al. (2023), ALASCA: Reasoning in Quantified
% Source   : [Sch22]
% Names    : composition_tower_f2_sz32__1.smt2 [Sch22]

% Status   : Theorem
% Rating   : 1.00 v8.2.0
% Syntax   : Number of formulae    :   68 (   0 unt;  35 typ;   0 def)
%            Number of atoms       :  363 (  33 equ)
%            Maximal formula atoms :   11 (  11 avg)
%            Number of connectives :  429 (  99   ~;   0   |; 132   &)
%                                         (   0 <=>; 198  =>;   0  <=;   0 <~>)
%            Maximal formula depth :   11 (  11 avg)
%            Maximal term depth    :   35 (   3 avg)
%            Number arithmetic     :  957 ( 330 atm; 330 fun; 198 num;  99 var)
%            Number of types       :    1 (   0 usr;   1 ari)
%            Number of type conns  :    2 (   2   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   0 usr;   0 prp; 2-2 aty)
%            Number of functors    :   38 (  35 usr;  34 con; 0-2 aty)
%            Number of variables   :   99 (  66   !;  33   ?;  99   :)
% SPC      : TF0_THM_EQU_ARI

% Comments : Translated from SMT UFLRA by SMTtoTPTP.
%------------------------------------------------------------------------------
%% Declarations:
tff(l14,type,
    l14: $real ).

tff(l4,type,
    l4: $real ).

tff(l5,type,
    l5: $real ).

tff(l30,type,
    l30: $real ).

tff(f0,type,
    f0: $real > $real ).

tff(l31,type,
    l31: $real ).

tff(l16,type,
    l16: $real ).

tff(l18,type,
    l18: $real ).

tff(l24,type,
    l24: $real ).

tff(l20,type,
    l20: $real ).

tff(l21,type,
    l21: $real ).

tff(l2,type,
    l2: $real ).

tff(a,type,
    a: $real ).

tff(l15,type,
    l15: $real ).

tff(l25,type,
    l25: $real ).

tff(f1,type,
    f1: $real > $real ).

tff(l27,type,
    l27: $real ).

tff(l10,type,
    l10: $real ).

tff(l1,type,
    l1: $real ).

tff(l13,type,
    l13: $real ).

tff(l26,type,
    l26: $real ).

tff(l19,type,
    l19: $real ).

tff(l23,type,
    l23: $real ).

tff(l28,type,
    l28: $real ).

tff(l11,type,
    l11: $real ).

tff(l7,type,
    l7: $real ).

tff(l17,type,
    l17: $real ).

tff(l,type,
    l: $real ).

tff(l29,type,
    l29: $real ).

tff(l22,type,
    l22: $real ).

tff(l9,type,
    l9: $real ).

tff(l3,type,
    l3: $real ).

tff(l12,type,
    l12: $real ).

tff(l8,type,
    l8: $real ).

tff(l6,type,
    l6: $real ).

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

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l2) ∧ ((if ((x - l2) ≥ 0.0) (x - l2) else -(x - l2)) < delta)) ⇒ ((if ((f1(x) - l1) ≥ 0.0) (f1(x) - l1) else -(f1(x) - l1)) < epsilon))))
tff(formula_2,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l2 )
                & ( $greatereq($difference(X,l2),0.0)
                 => $less($difference(X,l2),Delta) )
                & ( ~ $greatereq($difference(X,l2),0.0)
                 => $less($uminus($difference(X,l2)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l1),0.0)
                 => $less($difference(f1(X),l1),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l1),0.0)
                 => $less($uminus($difference(f1(X),l1)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l3) ∧ ((if ((x - l3) ≥ 0.0) (x - l3) else -(x - l3)) < delta)) ⇒ ((if ((f0(x) - l2) ≥ 0.0) (f0(x) - l2) else -(f0(x) - l2)) < epsilon))))
tff(formula_3,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l3 )
                & ( $greatereq($difference(X,l3),0.0)
                 => $less($difference(X,l3),Delta) )
                & ( ~ $greatereq($difference(X,l3),0.0)
                 => $less($uminus($difference(X,l3)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l2),0.0)
                 => $less($difference(f0(X),l2),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l2),0.0)
                 => $less($uminus($difference(f0(X),l2)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l4) ∧ ((if ((x - l4) ≥ 0.0) (x - l4) else -(x - l4)) < delta)) ⇒ ((if ((f0(x) - l3) ≥ 0.0) (f0(x) - l3) else -(f0(x) - l3)) < epsilon))))
tff(formula_4,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l4 )
                & ( $greatereq($difference(X,l4),0.0)
                 => $less($difference(X,l4),Delta) )
                & ( ~ $greatereq($difference(X,l4),0.0)
                 => $less($uminus($difference(X,l4)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l3),0.0)
                 => $less($difference(f0(X),l3),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l3),0.0)
                 => $less($uminus($difference(f0(X),l3)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l5) ∧ ((if ((x - l5) ≥ 0.0) (x - l5) else -(x - l5)) < delta)) ⇒ ((if ((f0(x) - l4) ≥ 0.0) (f0(x) - l4) else -(f0(x) - l4)) < epsilon))))
tff(formula_5,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l5 )
                & ( $greatereq($difference(X,l5),0.0)
                 => $less($difference(X,l5),Delta) )
                & ( ~ $greatereq($difference(X,l5),0.0)
                 => $less($uminus($difference(X,l5)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l4),0.0)
                 => $less($difference(f0(X),l4),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l4),0.0)
                 => $less($uminus($difference(f0(X),l4)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l6) ∧ ((if ((x - l6) ≥ 0.0) (x - l6) else -(x - l6)) < delta)) ⇒ ((if ((f1(x) - l5) ≥ 0.0) (f1(x) - l5) else -(f1(x) - l5)) < epsilon))))
tff(formula_6,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l6 )
                & ( $greatereq($difference(X,l6),0.0)
                 => $less($difference(X,l6),Delta) )
                & ( ~ $greatereq($difference(X,l6),0.0)
                 => $less($uminus($difference(X,l6)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l5),0.0)
                 => $less($difference(f1(X),l5),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l5),0.0)
                 => $less($uminus($difference(f1(X),l5)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l7) ∧ ((if ((x - l7) ≥ 0.0) (x - l7) else -(x - l7)) < delta)) ⇒ ((if ((f0(x) - l6) ≥ 0.0) (f0(x) - l6) else -(f0(x) - l6)) < epsilon))))
tff(formula_7,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l7 )
                & ( $greatereq($difference(X,l7),0.0)
                 => $less($difference(X,l7),Delta) )
                & ( ~ $greatereq($difference(X,l7),0.0)
                 => $less($uminus($difference(X,l7)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l6),0.0)
                 => $less($difference(f0(X),l6),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l6),0.0)
                 => $less($uminus($difference(f0(X),l6)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l8) ∧ ((if ((x - l8) ≥ 0.0) (x - l8) else -(x - l8)) < delta)) ⇒ ((if ((f1(x) - l7) ≥ 0.0) (f1(x) - l7) else -(f1(x) - l7)) < epsilon))))
tff(formula_8,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l8 )
                & ( $greatereq($difference(X,l8),0.0)
                 => $less($difference(X,l8),Delta) )
                & ( ~ $greatereq($difference(X,l8),0.0)
                 => $less($uminus($difference(X,l8)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l7),0.0)
                 => $less($difference(f1(X),l7),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l7),0.0)
                 => $less($uminus($difference(f1(X),l7)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l9) ∧ ((if ((x - l9) ≥ 0.0) (x - l9) else -(x - l9)) < delta)) ⇒ ((if ((f1(x) - l8) ≥ 0.0) (f1(x) - l8) else -(f1(x) - l8)) < epsilon))))
tff(formula_9,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l9 )
                & ( $greatereq($difference(X,l9),0.0)
                 => $less($difference(X,l9),Delta) )
                & ( ~ $greatereq($difference(X,l9),0.0)
                 => $less($uminus($difference(X,l9)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l8),0.0)
                 => $less($difference(f1(X),l8),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l8),0.0)
                 => $less($uminus($difference(f1(X),l8)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l10) ∧ ((if ((x - l10) ≥ 0.0) (x - l10) else -(x - l10)) < delta)) ⇒ ((if ((f0(x) - l9) ≥ 0.0) (f0(x) - l9) else -(f0(x) - l9)) < epsilon))))
tff(formula_10,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l10 )
                & ( $greatereq($difference(X,l10),0.0)
                 => $less($difference(X,l10),Delta) )
                & ( ~ $greatereq($difference(X,l10),0.0)
                 => $less($uminus($difference(X,l10)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l9),0.0)
                 => $less($difference(f0(X),l9),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l9),0.0)
                 => $less($uminus($difference(f0(X),l9)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l11) ∧ ((if ((x - l11) ≥ 0.0) (x - l11) else -(x - l11)) < delta)) ⇒ ((if ((f0(x) - l10) ≥ 0.0) (f0(x) - l10) else -(f0(x) - l10)) < epsilon))))
tff(formula_11,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l11 )
                & ( $greatereq($difference(X,l11),0.0)
                 => $less($difference(X,l11),Delta) )
                & ( ~ $greatereq($difference(X,l11),0.0)
                 => $less($uminus($difference(X,l11)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l10),0.0)
                 => $less($difference(f0(X),l10),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l10),0.0)
                 => $less($uminus($difference(f0(X),l10)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l12) ∧ ((if ((x - l12) ≥ 0.0) (x - l12) else -(x - l12)) < delta)) ⇒ ((if ((f0(x) - l11) ≥ 0.0) (f0(x) - l11) else -(f0(x) - l11)) < epsilon))))
tff(formula_12,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l12 )
                & ( $greatereq($difference(X,l12),0.0)
                 => $less($difference(X,l12),Delta) )
                & ( ~ $greatereq($difference(X,l12),0.0)
                 => $less($uminus($difference(X,l12)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l11),0.0)
                 => $less($difference(f0(X),l11),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l11),0.0)
                 => $less($uminus($difference(f0(X),l11)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l13) ∧ ((if ((x - l13) ≥ 0.0) (x - l13) else -(x - l13)) < delta)) ⇒ ((if ((f1(x) - l12) ≥ 0.0) (f1(x) - l12) else -(f1(x) - l12)) < epsilon))))
tff(formula_13,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l13 )
                & ( $greatereq($difference(X,l13),0.0)
                 => $less($difference(X,l13),Delta) )
                & ( ~ $greatereq($difference(X,l13),0.0)
                 => $less($uminus($difference(X,l13)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l12),0.0)
                 => $less($difference(f1(X),l12),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l12),0.0)
                 => $less($uminus($difference(f1(X),l12)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l14) ∧ ((if ((x - l14) ≥ 0.0) (x - l14) else -(x - l14)) < delta)) ⇒ ((if ((f0(x) - l13) ≥ 0.0) (f0(x) - l13) else -(f0(x) - l13)) < epsilon))))
tff(formula_14,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l14 )
                & ( $greatereq($difference(X,l14),0.0)
                 => $less($difference(X,l14),Delta) )
                & ( ~ $greatereq($difference(X,l14),0.0)
                 => $less($uminus($difference(X,l14)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l13),0.0)
                 => $less($difference(f0(X),l13),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l13),0.0)
                 => $less($uminus($difference(f0(X),l13)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l15) ∧ ((if ((x - l15) ≥ 0.0) (x - l15) else -(x - l15)) < delta)) ⇒ ((if ((f0(x) - l14) ≥ 0.0) (f0(x) - l14) else -(f0(x) - l14)) < epsilon))))
tff(formula_15,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l15 )
                & ( $greatereq($difference(X,l15),0.0)
                 => $less($difference(X,l15),Delta) )
                & ( ~ $greatereq($difference(X,l15),0.0)
                 => $less($uminus($difference(X,l15)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l14),0.0)
                 => $less($difference(f0(X),l14),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l14),0.0)
                 => $less($uminus($difference(f0(X),l14)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l16) ∧ ((if ((x - l16) ≥ 0.0) (x - l16) else -(x - l16)) < delta)) ⇒ ((if ((f0(x) - l15) ≥ 0.0) (f0(x) - l15) else -(f0(x) - l15)) < epsilon))))
tff(formula_16,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l16 )
                & ( $greatereq($difference(X,l16),0.0)
                 => $less($difference(X,l16),Delta) )
                & ( ~ $greatereq($difference(X,l16),0.0)
                 => $less($uminus($difference(X,l16)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l15),0.0)
                 => $less($difference(f0(X),l15),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l15),0.0)
                 => $less($uminus($difference(f0(X),l15)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l17) ∧ ((if ((x - l17) ≥ 0.0) (x - l17) else -(x - l17)) < delta)) ⇒ ((if ((f1(x) - l16) ≥ 0.0) (f1(x) - l16) else -(f1(x) - l16)) < epsilon))))
tff(formula_17,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l17 )
                & ( $greatereq($difference(X,l17),0.0)
                 => $less($difference(X,l17),Delta) )
                & ( ~ $greatereq($difference(X,l17),0.0)
                 => $less($uminus($difference(X,l17)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l16),0.0)
                 => $less($difference(f1(X),l16),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l16),0.0)
                 => $less($uminus($difference(f1(X),l16)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l18) ∧ ((if ((x - l18) ≥ 0.0) (x - l18) else -(x - l18)) < delta)) ⇒ ((if ((f0(x) - l17) ≥ 0.0) (f0(x) - l17) else -(f0(x) - l17)) < epsilon))))
tff(formula_18,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l18 )
                & ( $greatereq($difference(X,l18),0.0)
                 => $less($difference(X,l18),Delta) )
                & ( ~ $greatereq($difference(X,l18),0.0)
                 => $less($uminus($difference(X,l18)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l17),0.0)
                 => $less($difference(f0(X),l17),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l17),0.0)
                 => $less($uminus($difference(f0(X),l17)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l19) ∧ ((if ((x - l19) ≥ 0.0) (x - l19) else -(x - l19)) < delta)) ⇒ ((if ((f1(x) - l18) ≥ 0.0) (f1(x) - l18) else -(f1(x) - l18)) < epsilon))))
tff(formula_19,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l19 )
                & ( $greatereq($difference(X,l19),0.0)
                 => $less($difference(X,l19),Delta) )
                & ( ~ $greatereq($difference(X,l19),0.0)
                 => $less($uminus($difference(X,l19)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l18),0.0)
                 => $less($difference(f1(X),l18),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l18),0.0)
                 => $less($uminus($difference(f1(X),l18)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l20) ∧ ((if ((x - l20) ≥ 0.0) (x - l20) else -(x - l20)) < delta)) ⇒ ((if ((f0(x) - l19) ≥ 0.0) (f0(x) - l19) else -(f0(x) - l19)) < epsilon))))
tff(formula_20,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l20 )
                & ( $greatereq($difference(X,l20),0.0)
                 => $less($difference(X,l20),Delta) )
                & ( ~ $greatereq($difference(X,l20),0.0)
                 => $less($uminus($difference(X,l20)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l19),0.0)
                 => $less($difference(f0(X),l19),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l19),0.0)
                 => $less($uminus($difference(f0(X),l19)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l21) ∧ ((if ((x - l21) ≥ 0.0) (x - l21) else -(x - l21)) < delta)) ⇒ ((if ((f1(x) - l20) ≥ 0.0) (f1(x) - l20) else -(f1(x) - l20)) < epsilon))))
tff(formula_21,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l21 )
                & ( $greatereq($difference(X,l21),0.0)
                 => $less($difference(X,l21),Delta) )
                & ( ~ $greatereq($difference(X,l21),0.0)
                 => $less($uminus($difference(X,l21)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l20),0.0)
                 => $less($difference(f1(X),l20),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l20),0.0)
                 => $less($uminus($difference(f1(X),l20)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l22) ∧ ((if ((x - l22) ≥ 0.0) (x - l22) else -(x - l22)) < delta)) ⇒ ((if ((f0(x) - l21) ≥ 0.0) (f0(x) - l21) else -(f0(x) - l21)) < epsilon))))
tff(formula_22,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l22 )
                & ( $greatereq($difference(X,l22),0.0)
                 => $less($difference(X,l22),Delta) )
                & ( ~ $greatereq($difference(X,l22),0.0)
                 => $less($uminus($difference(X,l22)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l21),0.0)
                 => $less($difference(f0(X),l21),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l21),0.0)
                 => $less($uminus($difference(f0(X),l21)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l23) ∧ ((if ((x - l23) ≥ 0.0) (x - l23) else -(x - l23)) < delta)) ⇒ ((if ((f1(x) - l22) ≥ 0.0) (f1(x) - l22) else -(f1(x) - l22)) < epsilon))))
tff(formula_23,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l23 )
                & ( $greatereq($difference(X,l23),0.0)
                 => $less($difference(X,l23),Delta) )
                & ( ~ $greatereq($difference(X,l23),0.0)
                 => $less($uminus($difference(X,l23)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l22),0.0)
                 => $less($difference(f1(X),l22),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l22),0.0)
                 => $less($uminus($difference(f1(X),l22)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l24) ∧ ((if ((x - l24) ≥ 0.0) (x - l24) else -(x - l24)) < delta)) ⇒ ((if ((f1(x) - l23) ≥ 0.0) (f1(x) - l23) else -(f1(x) - l23)) < epsilon))))
tff(formula_24,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l24 )
                & ( $greatereq($difference(X,l24),0.0)
                 => $less($difference(X,l24),Delta) )
                & ( ~ $greatereq($difference(X,l24),0.0)
                 => $less($uminus($difference(X,l24)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l23),0.0)
                 => $less($difference(f1(X),l23),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l23),0.0)
                 => $less($uminus($difference(f1(X),l23)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l25) ∧ ((if ((x - l25) ≥ 0.0) (x - l25) else -(x - l25)) < delta)) ⇒ ((if ((f1(x) - l24) ≥ 0.0) (f1(x) - l24) else -(f1(x) - l24)) < epsilon))))
tff(formula_25,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l25 )
                & ( $greatereq($difference(X,l25),0.0)
                 => $less($difference(X,l25),Delta) )
                & ( ~ $greatereq($difference(X,l25),0.0)
                 => $less($uminus($difference(X,l25)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l24),0.0)
                 => $less($difference(f1(X),l24),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l24),0.0)
                 => $less($uminus($difference(f1(X),l24)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l26) ∧ ((if ((x - l26) ≥ 0.0) (x - l26) else -(x - l26)) < delta)) ⇒ ((if ((f0(x) - l25) ≥ 0.0) (f0(x) - l25) else -(f0(x) - l25)) < epsilon))))
tff(formula_26,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l26 )
                & ( $greatereq($difference(X,l26),0.0)
                 => $less($difference(X,l26),Delta) )
                & ( ~ $greatereq($difference(X,l26),0.0)
                 => $less($uminus($difference(X,l26)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l25),0.0)
                 => $less($difference(f0(X),l25),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l25),0.0)
                 => $less($uminus($difference(f0(X),l25)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l27) ∧ ((if ((x - l27) ≥ 0.0) (x - l27) else -(x - l27)) < delta)) ⇒ ((if ((f0(x) - l26) ≥ 0.0) (f0(x) - l26) else -(f0(x) - l26)) < epsilon))))
tff(formula_27,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l27 )
                & ( $greatereq($difference(X,l27),0.0)
                 => $less($difference(X,l27),Delta) )
                & ( ~ $greatereq($difference(X,l27),0.0)
                 => $less($uminus($difference(X,l27)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l26),0.0)
                 => $less($difference(f0(X),l26),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l26),0.0)
                 => $less($uminus($difference(f0(X),l26)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l28) ∧ ((if ((x - l28) ≥ 0.0) (x - l28) else -(x - l28)) < delta)) ⇒ ((if ((f0(x) - l27) ≥ 0.0) (f0(x) - l27) else -(f0(x) - l27)) < epsilon))))
tff(formula_28,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l28 )
                & ( $greatereq($difference(X,l28),0.0)
                 => $less($difference(X,l28),Delta) )
                & ( ~ $greatereq($difference(X,l28),0.0)
                 => $less($uminus($difference(X,l28)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l27),0.0)
                 => $less($difference(f0(X),l27),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l27),0.0)
                 => $less($uminus($difference(f0(X),l27)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l29) ∧ ((if ((x - l29) ≥ 0.0) (x - l29) else -(x - l29)) < delta)) ⇒ ((if ((f1(x) - l28) ≥ 0.0) (f1(x) - l28) else -(f1(x) - l28)) < epsilon))))
tff(formula_29,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l29 )
                & ( $greatereq($difference(X,l29),0.0)
                 => $less($difference(X,l29),Delta) )
                & ( ~ $greatereq($difference(X,l29),0.0)
                 => $less($uminus($difference(X,l29)),Delta) ) )
             => ( ( $greatereq($difference(f1(X),l28),0.0)
                 => $less($difference(f1(X),l28),Epsilon) )
                & ( ~ $greatereq($difference(f1(X),l28),0.0)
                 => $less($uminus($difference(f1(X),l28)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l30) ∧ ((if ((x - l30) ≥ 0.0) (x - l30) else -(x - l30)) < delta)) ⇒ ((if ((f0(x) - l29) ≥ 0.0) (f0(x) - l29) else -(f0(x) - l29)) < epsilon))))
tff(formula_30,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l30 )
                & ( $greatereq($difference(X,l30),0.0)
                 => $less($difference(X,l30),Delta) )
                & ( ~ $greatereq($difference(X,l30),0.0)
                 => $less($uminus($difference(X,l30)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l29),0.0)
                 => $less($difference(f0(X),l29),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l29),0.0)
                 => $less($uminus($difference(f0(X),l29)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = l31) ∧ ((if ((x - l31) ≥ 0.0) (x - l31) else -(x - l31)) < delta)) ⇒ ((if ((f0(x) - l30) ≥ 0.0) (f0(x) - l30) else -(f0(x) - l30)) < epsilon))))
tff(formula_31,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != l31 )
                & ( $greatereq($difference(X,l31),0.0)
                 => $less($difference(X,l31),Delta) )
                & ( ~ $greatereq($difference(X,l31),0.0)
                 => $less($uminus($difference(X,l31)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l30),0.0)
                 => $less($difference(f0(X),l30),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l30),0.0)
                 => $less($uminus($difference(f0(X),l30)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = a) ∧ ((if ((x - a) ≥ 0.0) (x - a) else -(x - a)) < delta)) ⇒ ((if ((f0(x) - l31) ≥ 0.0) (f0(x) - l31) else -(f0(x) - l31)) < epsilon))))
tff(formula_32,axiom,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != a )
                & ( $greatereq($difference(X,a),0.0)
                 => $less($difference(X,a),Delta) )
                & ( ~ $greatereq($difference(X,a),0.0)
                 => $less($uminus($difference(X,a)),Delta) ) )
             => ( ( $greatereq($difference(f0(X),l31),0.0)
                 => $less($difference(f0(X),l31),Epsilon) )
                & ( ~ $greatereq($difference(f0(X),l31),0.0)
                 => $less($uminus($difference(f0(X),l31)),Epsilon) ) ) ) ) ) ).

%% ∀ epsilon:Real ((0.0 < epsilon) ⇒ ∃ delta:Real ((0.0 < delta) ∧ ∀ x:Real ((¬(x = a) ∧ ((if ((x - a) ≥ 0.0) (x - a) else -(x - a)) < delta)) ⇒ ((if ((f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(x)))))))))))))))))))))))))))))))) - l) ≥ 0.0) (f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(x)))))))))))))))))))))))))))))))) - l) else -(f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(x)))))))))))))))))))))))))))))))) - l)) < epsilon))))
tff(formula_33,conjecture,
    ! [Epsilon: $real] :
      ( $less(0.0,Epsilon)
     => ? [Delta: $real] :
          ( $less(0.0,Delta)
          & ! [X: $real] :
              ( ( ( X != a )
                & ( $greatereq($difference(X,a),0.0)
                 => $less($difference(X,a),Delta) )
                & ( ~ $greatereq($difference(X,a),0.0)
                 => $less($uminus($difference(X,a)),Delta) ) )
             => ( ( $greatereq($difference(f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(X)))))))))))))))))))))))))))))))),l),0.0)
                 => $less($difference(f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(X)))))))))))))))))))))))))))))))),l),Epsilon) )
                & ( ~ $greatereq($difference(f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(X)))))))))))))))))))))))))))))))),l),0.0)
                 => $less($uminus($difference(f1(f1(f0(f0(f0(f1(f0(f1(f1(f0(f0(f0(f1(f0(f0(f0(f1(f0(f1(f0(f1(f0(f1(f1(f1(f0(f0(f0(f1(f0(f0(f0(X)))))))))))))))))))))))))))))))),l)),Epsilon) ) ) ) ) ) ).

%------------------------------------------------------------------------------
