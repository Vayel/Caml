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
  let rec aux_memo i j =
    try
      Hashtbl.find tbl (i, j)
    with
      | Not_found -> begin
                      let a = aux i j in
                      Hashtbl.add tbl (i, j) a;
                      a
                     end
  and aux i j = (* Renvoie dl(xi, yj) *)
    match i, j with
      | 0, _ -> j
      | _, 0 -> i
      | _, _ -> let cij = int_of_bool (x.(i-1) != y.(j-1)) in
                  min_array [|
                    aux_memo (i-1) j + 1;
                    aux_memo i (j-1) + 1;
                    aux_memo (i-1) (j-1) + cij 
                  |]
  in
  aux (Array.length x) (Array.length y)
;;

(* 3. *)
(*
  On remplit la matrice et retourne l'élément en bas à droite.
*)
let distance x y =
  let xLen = Array.length x in
  let yLen = Array.length y in
  let matDefault = -1 in
  let mat = Array.make_matrix (xLen+1) (yLen+1) matDefault in
  for i = 0 to xLen do
    for j = 0 to yLen do
      mat.(i).(j) <- if j = 0 then i
                     else if i = 0 then j
                     else begin 
                       let cij = int_of_bool (x.(i-1) != y.(j-1)) in
                       min_array [|
                         mat.(i-1).(j) + 1;
                         mat.(i).(j-1) + 1;
                         mat.(i-1).(j-1) + cij 
                       |]
                     end
    done;
  done;
  mat.(xLen).(yLen)
;;

(* 4. *)
(*
  Pour obtenir dl(x_{i}, y_{j}), il n'est pas nécessaire de tout connaître,
  seulement dl(x_{i}, y_{j-1}), dl(x_{i-1}, y_{j}), dl(x_{i-1}, y_{j-1}).
  Du coup, on ne conserve qu'une matrice (n, 2).
*)
let distance_opt x y =
  let xLen = Array.length x in
  let yLen = Array.length y in
  let currentCol = Array.make (yLen+1) (-1) in
  for i = 0 to xLen do
    let oldCol = Array.copy currentCol in
    for j = 0 to yLen do (* On remplit la ième colonne. *)
      currentCol.(j) <- if j = 0 then i
                         else if i = 0 then j
                         else begin 
                           let cij = int_of_bool (x.(i-1) != y.(j-1)) in
                           min_array [|
                             oldCol.(j) + 1;
                             currentCol.(j-1) + 1;
                             oldCol.(j-1) + cij 
                           |]
                         end
    done;
  done;
  currentCol.(yLen)
;;
