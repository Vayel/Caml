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
(* 
'a array -> 'a array -> int 

Complexité en temps :

Soit (i, j) \in N^2.
Notons T(i, j) le temps d'exécution de (aux i j).
On a alors : T(i, j) = T(i-1, j) + T(i, j-1) + T(i-1, j-1) + C 
*)
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
(*
'a array -> 'a array -> int 

Complexité en temps :

Complexité en mémoire :
*)
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
'a array -> 'a array -> int 

On remplit la matrice et retourne l'élément en bas à droite.

Complexité en temps :
Notons n = max(Array.length x, Array.length y).
- Calcul de xLen : O(1)
- Calcul de yLen : O(1)
- Création de mat : O(n^2)
- Parcours des boucles : O(n^2)
Donc distance est un O(n^2).

Complexité en mémoire :
On a quelques variables plus une matrice de taille n * n donc 
distance est clairement un O(n^2).
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
'a array -> 'a array -> int 

Pour obtenir dl(x_{i}, y_{j}), il n'est pas nécessaire de tout connaître,
mais seulement dl(x_{i}, y_{j-1}), dl(x_{i-1}, y_{j}), dl(x_{i-1}, y_{j-1}).
Du coup, on ne conserve qu'une matrice (n, 2).

Complexité en temps :
Notons n = max(Array.length x, Array.length y).
- Calcul de xLen : O(1)
- Calcul de yLen : O(1)
- Création de currentCol : O(n)
- Parcours de la première boucle : 
  - Copie du tableau : O(n)
  - Parcours de la seconde boucle : O(n^2)
Donc distance_opt est un O(n^2).

Complexité en mémoire :
On a quelques variables plus deux tableaux de taille n donc 
distance_opt est clairement un O(n).
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

