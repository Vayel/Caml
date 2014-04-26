include "dm01.ml";;

print_string "Préliminaires\n";;
print_string "1.\n";;
let a = [|4;2;3|];;
min_array a;; (* 2 *)

print_string "2.\n";;
let l = [4;8;5];;
min_list l;; (* 4 *)

print_string "3.\n";;
pow 2 10;; (* 1024 *)


print_string "I.\n";;

print_string "I.1.\n";;
let a = [|5;2;3;4|];;
cout_une_tete a;; (* 5 + 3 + 1 + 1 = 10 *)

print_string "I.2.a.\n";;
let req = [|5;2;4|];;
let dep = [|0;1;0|];;
cout_array req dep;; (* 5 + 2 + 1 = 8 *)

print_string "I.2.b.\n";;
let req = [5;2;4];;
let dep = [0;0;1];;
cout_list req dep;; (* 5 + 3 + 4 = 12 *)


print_string "II.\n";;

print_string "II.1.\n";;
let req = (3, 1);;
cout_opt_2 req;; (* 3 + 1 pour (0, 1) *)

print_string "II.2.\n";;
let req = [|5;2;4;1;2|];;
cout_glouton req;; (* 5 + 2 + 1 + 1 + 1 = 10 *)


print_string "III.\n";;

print_string "III.2.a.\n";;
bin_of_dec 0;; (* 0 *)
bin_of_dec 1;; (* 1 *)
bin_of_dec 4;; (* 100 *)
bin_of_dec 13;; (* 1101 *)
enum 2;; (* [[0;0]; [0;1]; [1;0]; [1;1]] *)
enum 3;; (* [[0;0;0]; [0;0;1]; [0;1;0]; [0;1;1]; [1;0;0]; 
[1;0;1]; [1;1;0]; [1;1;1]] *)

print_string "III.2.b.\n";;
let req = [5;3;7];;
force_brute_list req;; (* 5+3+2 = 10 *)

print_string "III.3.a.\n";;
binaire 12 5;; (* [|0;1;1;0;0|] *)
binaire 19 5;; (* [|1;0;0;1;1|] *)

print_string "III.3.b.\n";;
let req = [|5;3;7|];;
force_brute_array req;; (* 5+3+2 = 10 *)

print_string "IV.2.d.\n";;
let req = [|0;12;5;1|];;
update [|19;17|] req 2;; (* [|23; 21; 20|] *)

print_string "IV.3.\n";;
let req = [|12;5;1|];;
cout_opt req;; (* 12+7+1 = 20 *)



















