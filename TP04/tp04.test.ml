#use "tp04.ml";;

let a = [|4;6;2;4;1;8;10|];;
min_array a;; (* 1 *)

distance_naive [||] [||];; (* 0 *)
distance_naive [|1;1;1|] [|1;2;1|];; (* 1 *)
distance_naive [|1;1;1;2|] [|1;1|];; (* 2 *)

distance [||] [||];; (* 0 *)
distance [|1;1;1|] [|1;2;1|];; (* 1 *)
distance [|1;1;1;2|] [|1;1|];; (* 2 *)
