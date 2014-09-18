#use "tp01.ml";;

(* 2. *)
print_string "\n ******2******";;
let t = [|1;2;3;4;5;6|];;
insert_vect 3 t 2 10;; (* [|1;10;2;3;5;6|] *) 
t;;

(* 3. *)
print_string "\n ******3******";;
let t = {
    keys = [|0;1;2;3;4;5|];
    size = 3;
    vals = Values [|1;2;3;4;5;6|];    
};;

let s = is_full t;; (* false *)

(* 4. *)
print_string "\n ******4******";;

