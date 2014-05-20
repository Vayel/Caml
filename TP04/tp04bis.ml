(* 'a array -> 'a *)
let min_array a =
  let m = ref a.(0) in
  let len = (Array.length a) in
  for i = 1 to (len-1) do
    m := min a.(i) !m
  done;
  !m
;;

(* bool -> int *)
let int_of_bool b =
  match b with
    | false -> 0
    | true -> 1
;;

(*
int -> int -> int

Retourne le coefficient cij, valant 0 si x = y, 1 sinon.
*)
let cij x y = 
  int_of_bool (x != y)
;;

(*
TODO
*)
let dl helper (x, y, i, j) =
  match i, j with
    | 0, _ -> j
    | _, 0 -> i
    | _, _ -> min_array [|
                1 + helper (x, y, (i-1), j);
                1 + helper (x, y, i, j-1);
                (cij x.(i-1) y.(j-1)) + helper (x, y, i-1, j-1)
              |]
;;

(* 1. *)
let distance_naive x y =
  let rec aux (x, y, i, j) =
    dl aux (x, y, i, j)
  in
  dl aux (x, y, Array.length x, Array.length y)
;;

(* 2. *)
let rec memo op x = 
  try  
    Hashtbl.find t x
  with 
    | Not_found -> let v = op (memo op) x in 
                   Hashtbl.add t x v; 
                   v
;;

let distance_memo x y =
  let xLen = Array.length x in
  let yLen = Array.length y in
  let tbl = Hashtbl.create ((xLen+1) * (yLen+1)) in
  
;;
  
(*



let distance_memo = memo dl;;

let helper old cur curi (x, y, i, j) =
  assert (i=curi || i=curi-1)
  if i = curi then cur.(j) else old.(j)
;;

let update col x y i =
  let n = Array.length y in
  let newcol = Array.make n in
  for j = 0 to n-1 do
    newcol.(j) <- dl (helper col newcol (i+1))
  done

*)  
