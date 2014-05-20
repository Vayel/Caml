#use "tp04bis.ml";;

print_string "\n min_array :";;
let a = [|4;6;2;4;1;8;10|];;
min_array a;; (* 1 *)

(* 1. *)
print_string "\n distance_naive :";;
distance_naive [||] [||];; (* 0 *)
distance_naive [|1;1;1|] [|1;2;1|];; (* 1 *)
distance_naive [|1;1;1;2|] [|1;1|];; (* 2 *)

(*
(* 2. *)
print_string "\n distance_memo :";;
distance_memo [||] [||];; (* 0 *)
distance_memo [|1;1;1|] [|1;2;1|];; (* 1 *)
distance_memo [|1;1;1;2|] [|1;1|];; (* 2 *)

(* 3. *)
print_string "\n distance :";;
distance [||] [||];; (* 0 *)
distance [|1;1;1|] [|1;2;1|];; (* 1 *)
distance [|1;1;1;2|] [|1;1|];; (* 2 *)

(* 4. *)
print_string "\n distance_opt :";;
distance_opt [||] [||];; (* 0 *)
distance_opt [|1;1;1|] [|1;2;1|];; (* 1 *)
distance_opt [|1;1;1;2|] [|1;1|];; (* 2 *)
*)

