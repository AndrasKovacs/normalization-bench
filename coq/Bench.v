

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

(* Goal True. idtac "------------- cbv (same as compute)". Abort. *)
(* Goal True. Time let x := eval cbv in n5M in idtac. Abort. *)
(* Goal True. Time let x := eval cbv in n10M in idtac. Abort. *)
(* Goal True. Time idtac "N/A". Abort. *)
(* Goal True. Time idtac "N/A". Abort. *)
(* Goal True. Time let x := eval cbv in t2M in idtac. Abort. *)
(* Goal True. Time let x := eval cbv in t4M in idtac. Abort. *)
(* Goal True. Time let x := eval cbv in t8M in idtac. Abort. *)
(* Goal True. Time idtac "N/A". Abort. *)
(* Goal True. Time idtac "N/A". Abort. *)
(* Goal True. Time idtac "N/A". Abort. *)

(* Goal True. idtac "------------- lazy". Abort. *)
(* Goal True. Time let x := eval lazy in n5M in idtac. Abort. *)
(* Goal True. Time let x := eval lazy in n10M in idtac. Abort. *)
(* Time Definition foo_l0 : n5M  = n5Mb  := eq_refl. *)
(* Time Definition foo_l1 : n10M = n10Mb := eq_refl. *)
(* Goal True. Time let x := eval lazy in t2M in idtac. Abort. *)
(* Goal True. Time let x := eval lazy in t4M in idtac. Abort. *)
(* Goal True. Time let x := eval lazy in t8M in idtac. Abort. *)
(* Time Definition foo_l2 : t2M = t2Mb := eq_refl. *)
(* Time Definition foo_l3 : t4M = t4Mb := eq_refl. *)
(* Time Definition foo_l4 : t8M = t8Mb := eq_refl. *)

(* Goal True. idtac "------------- vm_compute". Abort. *)
(* Goal True. Time let x := eval vm_compute in n5M in idtac. Abort. *)
(* Goal True. Time let x := eval vm_compute in n10M in idtac. Abort. *)
(* N.B. This isn't exactly conversion, as we end up fully normalizing n5M three times and n5Mb once *)
(* Time Definition foo_v0 := eq_refl n5M  <: n5M  = n5Mb. *)
(* Time Definition foo_v1 := eq_refl n10M <: n10M = n10Mb. *)
(* Goal True. Time let x := eval vm_compute in t2M in idtac. Abort. *)
(* Goal True. Time let x := eval vm_compute in t4M in idtac. Abort. *)
(* Goal True. Time let x := eval vm_compute in t8M in idtac. Abort. *)
(* Time Definition foo_v2 := eq_refl t2M <: t2M = t2Mb. *)
(* Time Definition foo_v3 := eq_refl t4M <: t4M = t4Mb. *)
(* Time Definition foo_v4 := eq_refl t8M <: t8M = t8Mb. *)


(* Goal True. idtac "------------- native_compute". Abort. *)
(* Goal True. Time let x := eval native_compute in n5M in idtac. Abort. *)
(* Goal True. Time let x := eval native_compute in n10M in idtac. Abort. *)
(* N.B. This isn't exactly conversion, as we end up fully normalizing n5M three times and n5Mb once *)
(* Time Definition foo_n0 := eq_refl n5M  <<: n5M  = n5Mb. *)
(* Time Definition foo_n1 := eq_refl n10M <<: n10M = n10Mb. *)
(* Goal True. Time let x := eval native_compute in t2M in idtac. Abort. *)
(* Goal True. Time let x := eval native_compute in t4M in idtac. Abort. *)
(* Goal True. Time let x := eval native_compute in t8M in idtac. Abort. *)
(* Time Definition foo_n2 := eq_refl t2M <<: t2M = t2Mb. *)
(* Time Definition foo_n3 := eq_refl t4M <<: t4M = t4Mb. *)
(* Time Definition foo_n4 := eq_refl t8M <<: t8M = t8Mb. *)

(* /usr/bin/time -v coqc -impredicative-set Bench.v *)
