(* II. Représentation *)

let kilo = 1000;;
let base = 10;; (* TODO *)
type naturel = Naturel of int list;;

(* II.1. *)
let diveucli a b =
  let q1 = a / b in
  let r = a - b * q1 in
  let q = 
    if r < 0 && b > 0 then q1 - 1
    else if r < 0 && b < 0 then q1 + 1
    else q1
  in (q, a - b * q)
;;

let cons x l = match x, l with
  | 0, [] -> []
  | _ -> x::l
;;

let rec normalise l c = match l with
  | [] -> cons c []
  | x::l' -> let q, r = diveucli (x + c) base in
               cons r (normalise l' q)
;;

let naturel t = Naturel (normalise t 0);;

(* II.2. *)
let nb_chiffres n = match n with
  | Naturel l -> list_length l
;;

(* II.3. *)
let rec ieme_elem l i = match i, l with
  | _, [] -> 0
  | 0, x::r -> x
  | _, x::r -> ieme_elem r (i-1)
;;

let chiffre n i = match n with
  | Naturel l -> ieme_elem l i
;;

(* III. Opérations élémentaires *)
(* III.1. *)
let rec ajoute_aux l1 l2 l = match l1, l2 with
  | [], [] -> rev l
  | [], x::r -> ajoute_aux [] r ((x+0)::l)
  | x::r, [] -> ajoute_aux r [] ((x+0)::l)
  | x::r, x'::r' -> ajoute_aux r r' ((x+x')::l)
;;

let ajoute n1 n2 = match n1, n2 with
  | Naturel l1, Naturel l2 -> naturel(ajoute_aux l1 l2 [])
;;

(* III.2. *)
let rec soustrait_aux l1 l2 l = match l1, l2 with
  | [], [] -> rev l
  | [], x::r -> failwith "soustrait(): n1 < n2"
  | x::r, [] -> soustrait_aux r [] ((x-0)::l)
  | x::r, x'::r' -> soustrait_aux r r' ((x-x')::l)
;;

let soustrait n1 n2 = match n1, n2 with
  | Naturel [], Naturel [] -> Naturel []
  | Naturel l1, Naturel l2 -> naturel(soustrait_aux l1 l2 [])
;;

(* III.3. *)
let rec compare_aux l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], x::r -> -1
  | x::r, [] -> 1
  | x::r, x'::r' -> let d = (x-x') in
			                if d = 0 then compare_aux r r'
                      else if d < 0 then -1
                      else 1
;;

let compare p q = match p, q with
  | Naturel l1, Naturel l2 -> compare_aux (rev l1) (rev l2)
;;

(* III.4. *)
let rec mulc_aux p n l = match n with
  | [] -> rev l
  | x::r -> mulc_aux p r ((x * p)::l)
;;

let mulc p n = match p, n with
  | 0, Naturel l -> Naturel []
  | _, Naturel l -> naturel (mulc_aux p l [])
;;

(* III.5. *)
let rec prepend n l p = match p with
  | 0 -> l
  | _ -> prepend n (n::l) (p-1)
;;

let mulb n p = match n, p with
  | Naturel [], _ -> Naturel []
  | _, 0 -> n
  | Naturel l, _ -> Naturel (prepend 0 l p)
;;

(* III.6. *)
let rec mul_naive_aux l1 l2 l = match l1, l2 with
  | _ -> Naturel []
;;

let mul_naive n1 n2 = match n1, n2 with
  | Naturel l1, Naturel [] -> Naturel []
  | Naturel [], Naturel l2 -> Naturel []
  | Naturel l1, Naturel l2 -> mul_naive_aux l1 l2 []
;;
