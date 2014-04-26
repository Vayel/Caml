(*
	LEFOULON Vincent
	Ocaml
*)

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

(* 1. *)
(* 'a array -> 'a array -> int *)
let distance_naive x y =
	let rec aux i j = (* Renvoie dl(xi, yj) *)
		match i, j with
			| 0, _ -> j
		  | _, 0 -> i
			| _, _ -> let cij = int_of_bool (x.(i-1) != y.(j-1)) in
									min_array [|
										aux (i-1) j + 1;
										aux i (j-1) + 1;
										aux (i-1) (j-1) + cij 
									|]
	in
	aux (Array.length x) (Array.length y)
;;

(* 2. *)
let distance_memo x y =
	let xLen = Array.length x in
	let yLen = Array.length y in
	let tbl = Hashtbl.create ((xLen+1) * (yLen+1)) in
	let memo_aux i j =
		try
			Hashtbl.find tbl (i, j)
		with
			| Not_found -> begin
											let a = aux i j in
											Hashtbl.add tbl (i, j) a;
											a
										 end
	and rec aux i j = (* Renvoie dl(xi, yj) *)
		match i, j with
			| 0, _ -> j
		  | _, 0 -> i
			| _, _ -> let cij = int_of_bool (x.(i-1) != y.(j-1)) in
								
									min_array [|
										a + 1;
										b + 1;
										c + cij 
									|]
	in
	aux (Array.length x) (Array.length y)
;;

(* 3. *)
let distance x y =
	let xLen = Array.length x in
	let yLen = Array.length y in
	let matDefault = -1 in
	let mat = Array.make_matrix (xLen+1) (yLen+1) matDefault in
	let rec aux i j = (* Renvoie dl(xi, yj) *)
		match i, j with
			| 0, _ -> j
		  | _, 0 -> i
			| _, _ -> let cij = int_of_bool (x.(i-1) != y.(j-1)) in
									if mat.(i-1).(j) = matDefault then mat.(i-1).(j) <- aux (i-1) j;
									if mat.(i).(j-1) = matDefault then mat.(i).(j-1) <- aux i (j-1);
									if mat.(i-1).(j-1) = matDefault then mat.(i-1).(j-1) <- aux (i-1) (j-1);
									min_array [|
										mat.(i-1).(j) + 1;
										mat.(i).(j-1) + 1;
										mat.(i-1).(j-1) + cij 
									|]
	in
	aux xLen yLen
;;

(* 4. *)
let distance_opt x y =
	"TODO"
;;
