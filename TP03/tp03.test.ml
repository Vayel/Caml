include "tp03.ml";;

(* II. Représentation *)
(* II.1. *)
print_string "\n naturel :";;
naturel [1;3;15;0;0];; (* [1;3;5;1] *)

(* II.2. *)
print_string "\n nb_chiffres :";;
let n = naturel [5;4;20;0;0;33;0;0;0];;
nb_chiffres n;; (* 7 *)

(* II.3. *)
print_string "\n chiffre :";;
let n = naturel [5;4;20;0;0;3;0];;
chiffre n 3;; (* 2 *)

(* III. Opérations élémentaires *)
(* III.1. *)
print_string "\n ajoute :";;
let n1 = naturel [1;9;8;2;4;6;0;0];;
let n2 = naturel [3;32];;
ajoute n1 n2;; (* [4;1;2;3;4;6] *)

(* III.2. *)
print_string "\n soustrait :";;
let n1 = naturel [1;9;8;2;4;6;0;0];;
let n2 = naturel [3;32;1;2;7];;
soustrait n1 n2;; (* [8;6;4;0;7;5] *)

(* III.3. *)
print_string "\n compare :";;
compare (naturel [1;2;3]) (naturel [1;2;3;0;0]);; (* 0 *)
compare (naturel [1;2;3;4]) (naturel [1;2;3;5]);; (* -1 *)
compare (naturel [5;6;2;7]) (naturel [2;3;5;2]);; (* 1 *)
compare (naturel [5;1]) (naturel [3]);; (* 1 *)

(* III.4. *)
print_string "\n mulc :";;
let n1 = naturel [1;9;8;2;4;6;0;0];;
mulc 3 n1;; (* [3;7;6;8;2;9;1] *)

(* III.5. *)
print_string "\n mulb :";;
let n1 = naturel [22];;
mulb n1 2;; (* [0;0;2;2] *)

(* III.6. *)
print_string "\n mul_naive :";;
let n1 = naturel [15];;
let n2 = naturel [5;0];;
mul_naive n1 n2;; (* [5;7] = 75 *)
mul_naive n1 n1;; (* [5;2;2] = 225 *)

(* III.7. *)
print_string "\n divise :";;
divise (naturel []) 5;; (* ([], 0) *)
divise (naturel [3;0]) 5;; (* ([], 3) *)
divise (naturel [5;1]) 3;; (* ([5], 0) *)
divise (naturel [8;3]) 3;; (* ([2;1], 2) *)

(* III.8. *)
print_string "\n divise_base :";;
let n1 = naturel [15];;
let n2 = naturel [5;0];;
divise_base n1 0;; (* ([5;1], []) *)
divise_base n1 1;; (* ([1], [5]) *)
divise_base n1 2;; (* ([], [5;1]) *)
divise_base n1 3;; (* ([], [5;1]) *)

(* IV.1. *)
print_string "\n slice :";;
slice (naturel []) 0 0;; (* [] *)
slice (naturel [1]) 0 1;; (* [1] *)
slice (naturel [1;2;3;4;5]) 1 4;; (* [2;3;4] *)

print_string "\n split :";;
let n1 = naturel [];;
let n2 = naturel [1];;
let n3 = naturel [1;2;3;4;5];;
split n1;; (* (0, [], []) *)
split n2;; (* (1, [1], []) *)
split n3;; (* (3, [1;2;3], [4;5]) *)

(* IV.2. *)
let n1 = naturel [];;
let n2 = naturel [1];;
let n3 = naturel [5];;
let n4 = naturel [13];;
carre n1;; (* [] *)
carre n2;; (* [1] *)
carre n3;; (* [5;2] *)
carre n4;; (* [9;6;1] *)

(* IV.3. *)
let n1 = naturel [];;
let n2 = naturel [1];;
let n3 = naturel [5];;
let n4 = naturel [3];;
mul n1 n2;; (* [] *)
mul n2 n3;; (* [5] *)
mul n3 n4;; (* [5;1] *)
