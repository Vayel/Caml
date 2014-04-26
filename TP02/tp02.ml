(* I. Préliminaires *)
(* I.1. *)
let rec range_aux n l = match n with
  | 0 -> 0::l
  | _ -> range_aux (n-1) (n::l)
;;

let range n = match n with
  | 0 -> []
  | _ -> range_aux (n-1) []
;;

(* I.2. *)
Random.self_init();;
let rec random_list_aux n p l = match n with
  | 0 -> (Random.int p)::l
  | _ -> random_list_aux (n-1) p ((Random.int p)::l)
;;

let random_list n p = match n with
  | 0 -> []
  | _ -> random_list_aux (n-1) p []
;;

(* I.3. *)
let rec est_triee_aux n l = match l with
  | [] -> true
  | a::r -> (n <= a) && (est_triee_aux a r)
;;

let est_triee l = match l with
  | [] -> true
  | a::r -> est_triee_aux a r
;;

(* II. Trie par insertion *)
(* II.1. *)
let rec insere el l = match l with
  | [] -> el::l
  | a::r -> if el <= a then el::l else a::(insere el r)
;;

(* II.2. *)
let rec tri_insertion_aux lb le = match lb with
  | [] -> le
  | a::r -> tri_insertion_aux r (insere a le)
;;

let tri_insertion l = tri_insertion_aux l [];;

(* III. Trie par pivot *)
(* III.1. *)
let rec partitionne_aux u v x l = match l with
  | [] -> (u, v)
  | a::r -> if a < x then partitionne_aux (a::u) v x r else partitionne_aux u (a::v) x r
;;

let partitionne x l = partitionne_aux [] [] x l;;

(* III.2. *)
let rec tri_pivot l = match l with
  | [] -> []
  | x::r -> let (u, v) = partitionne x r in
				tri_pivot u @ (x::tri_pivot v)
;;

(* IV. Tri fusion *)
(* IV.1. *)
let rec partage_aux i n u v l = match l with
  | [] -> (u, v)
  | a::r -> if i < n then partage_aux (i+1) n (a::u) v r else partage_aux (i+1) n u (a::v) r
;;

let partage l = partage_aux 0 ((List.length l)/2) [] [] l;;

(* IV.2. *)
let rec fusionne_aux a b l = match (a, b) with
  | ([], []) -> l
  | ([], r) | (r, []) -> (l @ r)
  | (af::a', bf::b') -> if af < bf then fusionne_aux a' b (l @ [af]) else fusionne_aux a b' (l @ [bf])
;;

let fusionne a b = fusionne_aux a b [];;

(* IV.3. *)
let rec tri_fusion l = match l with 
  | [] | [_] -> l
  | _ -> let (u, v) = partage l in
						fusionne (tri_fusion u) (tri_fusion v)		
;;
