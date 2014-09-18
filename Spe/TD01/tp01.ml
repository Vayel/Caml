let param = 3;;

type ('k, 'v) btree = {
    mutable keys: 'k array;
    mutable size: int;
    mutable vals: ('k, 'v) intext;
}

and ('k, 'v) intext =
    | Values of 'v array
    | Sons of ('k, 'v) btree array
;;

let empty () = {
    size = 0;
    keys = [||];
    vals = Values [||];
};;

(* 1. *)

(* 2. *)
let insert_vect n u i v = 
    for i = n downto (i+1) do
        u.(i) <- u.(i-1)
    done;
    u.(i) <- v
;;

(* 3. *)
let is_full tree = tree.size = 2 * param;;

(* 4. *)
let lookup t n k =
    let i = ref 0 in
    while i < n && k >= t.(i) do     
        incr i         
    done;
    i!    
;;
