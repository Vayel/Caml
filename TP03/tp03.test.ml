include "tp03.ml";;

(* II. Représentation *)
(* II.1. *)
naturel [1;3;15;0;0];; (* [1;3;5;1] *)

(* II.2. *)
let n = naturel [5;4;20;0;0;33;0;0;0];;
nb_chiffres n;; (* 7 *)

(* II.3. *)
let n = naturel [5;4;20;0;0;3;0];;
chiffre n 3;; (* 2 *)

(* III. Opérations élémentaires *)
(* III.1. *)
let n1 = naturel [1;9;8;2;4;6;0;0];;
let n2 = naturel [3;32];;
ajoute n1 n2;; (* [4;1;2;3;4;6] *)

(* III.2. *)
let n1 = naturel [1;9;8;2;4;6;0;0];;
let n2 = naturel [3;32;1;2;7];;
soustrait n1 n2;; (* [8;6;4;0;7;5] *)

(* III.3. *)
compare (naturel [1;2;3]) (naturel [1;2;3;0;0]);; (* 0 *)
compare (naturel [1;2;3;4]) (naturel [1;2;3;5]);; (* -1 *)
compare (naturel [5;6;2;7]) (naturel [2;3;5;2]);; (* 1 *)

(* III.4. *)
let n1 = naturel [1;9;8;2;4;6;0;0];;
mulc 3 n1;; (* [3;7;6;8;2;9;1] *)

(* III.5. *)
let n1 = naturel [22];;
mulb n1 2;; (* [0;0;2;2] *)

(* III.6. *)
let n1 = naturel [15];;
let n2 = naturel [5;0];;
mul_naive n1 n2;; (* [5;7] = 75 *)
mul_naive n1 n1;; (* [5;2;2] = 225 *)

(* III.7. *)


(* III.8. *)
let n1 = naturel [15];;
let n2 = naturel [5;0];;
divise_base n1 0;; (* ([5;1], []) *)
divise_base n1 1;; (* ([1], [5]) *)
divise_base n1 2;; (* ([], [5;1]) *)
divise_base n1 3;; (* ([], [5;1]) *)

(* IV.1. *)
let n1 = naturel [1;2;3;4;5;0];;
split n1;;

(* IV.2. *)


(* IV.3. *)

