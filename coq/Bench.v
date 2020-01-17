

Require Import Coq.Unicode.Utf8.

Definition Nat : Set := ∀ (N : Set), (N → N) → N → N.
Definition n2  : Nat := λ N s z, s (s z).
Definition n5  : Nat := λ N s z, s (s (s (s (s z)))).
Definition mul (a b : Nat) : Nat := λ N s z, a N (b N s) z.
Definition suc (a : Nat) : Nat := λ N s z, s (a N s z).

Definition n10     := mul n2 n5.
Definition n10b    := mul n5 n2.
Definition n20     := mul n2 n10.
Definition n20b    := mul n2 n10b.
Definition n21     := suc n20.
Definition n21b    := suc n20b.
Definition n22     := suc n21.
Definition n22b    := suc n21b.
Definition n100    := mul n10   n10.
Definition n100b   := mul n10b  n10b.
Definition n10k    := mul n100  n100.
Definition n10kb   := mul n100b n100b.
Definition n100k   := mul n10k  n10.
Definition n100kb  := mul n10kb n10b.
Definition n1M     := mul n10k  n100.
Definition n1Mb    := mul n10kb n100b.
Definition n5M     := mul n1M   n5.
Definition n5Mb    := mul n1Mb  n5.
Definition n10M    := mul n1M   n10.
Definition n10Mb   := mul n1Mb  n10b.
Definition n20M    := mul n10M  n2.
Definition n20Mb   := mul n10Mb n2.

Definition Tree : Set := ∀ (T : Set), T → (T → T → T) → T.
Definition leaf : Tree := λ T l n, l.
Definition node (t1 t2 : Tree) : Tree := λ T l n, n (t1 T l n) (t2 T l n).
Definition fullTree (n : Nat) : Tree := n _ (λ t, node t t) leaf.

Definition t2M  := fullTree n20.
Definition t4M  := fullTree n21.
Definition t8M  := fullTree n22.
Definition t2Mb := fullTree n20b.
Definition t4Mb := fullTree n21b.
Definition t8Mb := fullTree n22b.

Inductive ITree : Set :=
| ileaf : ITree
| inode : ITree → ITree → ITree.

Definition normTree (t : Tree) : bool :=
  match t ITree ileaf inode with
  | inode _ _ => true
  | _ => false
  end.

(* Definition t2Mconv : t2M = t2Mb := eq_refl. *)
(* Definition t4Mconv : t4M = t4Mb := eq_refl. *)
(* Definition t8Mconv : t8M = t8Mb := eq_refl. *)
(* Eval compute in (normTree t2M). *)
(* Eval compute in (normTree t4M). *)
(* Eval compute in (normTree t8M). *)
