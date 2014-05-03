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

let cons x l = 
  match x, l with
    | 0, [] -> []
    | _ -> x::l
;;

let rec normalise l c = 
  match l with
    | [] -> cons c []
    | x::l' -> let q, r = diveucli (x + c) base in
                  cons r (normalise l' q)
;;

let naturel t = Naturel (normalise t 0);;

(* II.2. *)
let nb_chiffres n = 
  match n with
    | Naturel l -> list_length l
;;

(* II.3. *)
let rec ieme_elem l i = 
  match i, l with
    | _, [] -> 0
    | 0, x::r -> x
    | _, x::r -> ieme_elem r (i-1)
;;

let chiffre n i = match n with
  | Naturel l -> ieme_elem l i
;;

(* III. Opérations élémentaires *)
(*
	Le pattern matching est un O(1).
	Un cons est un O(1).
	rev est un O(n).
	+, -, *... sont un O(1).
*)

(* III.1. *)
(*
	Complexité :
	ajoute() fait uniquement un appel à ajoute_aux().
	ajoute_aux() parcoure l1 et l2 puis fait appel à rev. 
	Donc ajoute() est un O(n).
*)
let rec ajoute_aux l1 l2 l = 
  match l1, l2 with
    | [], [] -> rev l
    | x::r, [] -> ajoute_aux r [] (x::l)
    | [], x::r -> ajoute_aux [] r (x::l)
    | x1::r1, x2::r2 -> ajoute_aux r1 r2 ((x1+x2)::l)
;;

let ajoute n1 n2 = 
  match n1, n2 with
    | Naturel l1, Naturel l2 -> naturel(ajoute_aux l1 l2 [])
;;

(* III.2. *)
(*
	Complexité :
	Par un raisonnement analogue à ajoute(), soustrait() est un O(n).
*)
let rec soustrait_aux l1 l2 l = 
  match l1, l2 with
    | [], [] -> rev l
    | [], x::r -> failwith "soustrait(): n1 < n2"
    | x::r, [] -> soustrait_aux r [] ((x-0)::l)
    | x::r, x'::r' -> soustrait_aux r r' ((x-x')::l)
;;

let soustrait n1 n2 = 
  match n1, n2 with
    | Naturel [], Naturel [] -> Naturel []
    | Naturel l1, Naturel l2 -> naturel(soustrait_aux l1 l2 [])
;;

(* III.3. *)
(*
	Complexité :
	nb_chiffres() est un O(n).
	compare_aux() parcoure les listes et ne fait que des opérations 
	élémentaires à chaque tour. Elle est donc un O(n).
	compare() est donc un O(n).
*)
let rec compare_aux l1 l2 = 
  match l1, l2 with
    | [], [] -> 0
    | [], x::r -> -1
    | x::r, [] -> 1
    | x::r, x'::r' -> let d = (x-x') in
                        if d = 0 then compare_aux r r'
                        else if d < 0 then -1
                        else 1
;;

let compare p q = 
  match p, q with
    | Naturel l1, Naturel l2 -> let pLen = nb_chiffres p in
                                let qLen = nb_chiffres q in
                                if pLen > qLen then 1
                                else if qLen > pLen then -1
                                (* 
                                  Sinon, on compare chaque chiffre en commençant
                                  par ceux de poids le plus fort.
                                *)
                                else compare_aux (rev l1) (rev l2)
;;

(* III.4. *)
(*
	Complexité :
	mulc() fait un appel à mulc_aux() qui parcoure l.
	A chaque tour, des opérations élémentaires sont faites.
	Donc mulc() est un O(n).
*)
let rec mulc_aux p n l = 
  match n with
    | [] -> rev l
    | x::r -> mulc_aux p r ((x * p)::l)
;;

let mulc p n = 
  match p, n with
    | 0, Naturel l -> Naturel []
    | _, Naturel l -> naturel (mulc_aux p l [])
;;

(* III.5. *)
(*
	Complexité :
	mulb() fait un appel à prepend().
	prepend() s'appelle elle-même p fois et effectue des opérations
	élémentaires à chaque tour.
	mulb() est donc un O(p).
*)
let rec prepend x l n = 
  match n with
    | 0 -> l
    | _ -> prepend x (x::l) (n-1)
;;

let mulb n p = 
  match n, p with
    | Naturel [], _ -> naturel []
    | Naturel l, _ -> naturel (prepend 0 l p)
;;

(* III.6. *)
let rec mul_naive_aux n1 l2 k acc = 
  match l2 with
    | [] -> acc
    | x::r -> mul_naive_aux n1 r (k+1) (ajoute acc (mulb (mulc x n1) k)) 
;;

let mul_naive n1 n2 = 
  match n1, n2 with
    | Naturel l1, Naturel [] -> naturel []
    | Naturel [], Naturel l2 -> naturel []
    | _, Naturel l2 -> mul_naive_aux n1 l2 0 (naturel [])
;;

(* III.7. *)
let rec divise_aux d q r =
  match compare r d with
    | -1 -> (q, r) (* r < d *)
    | _ -> divise_aux d (ajoute (naturel [1]) q) (soustrait r d)
;;

let divise n d =
  divise_aux (naturel [d]) (naturel []) n
;;

(* III.8. *)
(*
	Complexité :
	divise_base() appelle divise_base_quotient() qui est 
	clairement un O(k).
	Puis la fonction appelle soustrait() qui est un O(n) et 
	mulb() qui est un O(k).
	divise_base() est donc un O(n) ou O(k), selon l'étude.
*)
let rec divise_base_quotient l k =
  match k, l with
    | 0, _ -> naturel l
    | _, [] -> naturel []
    | _, x::r -> divise_base_quotient r (k-1) 
;;

let divise_base n k = 
  match n with
    | Naturel l -> let q = divise_base_quotient l k in
                    (q, soustrait n (mulb q k))
;;

(* IV. Méthode de Karatsuba *)
let seuil = 3;;

(* IV.1. *)
let slice n b e = 
  let rec aux i l =
    if i < b then naturel l
    else aux (i-1) ((chiffre n i)::l)
  in
  aux (e-1) []
;;

(*
  n = a + bB
  k' = ceil(k/2)
  a contient les k' premiers chiffres de n et b ceux restant.
  
  Complexité :
  - appel à nb_chiffres() : O(n)
	- deux appels à slice() : O(n)
	split() est donc un O(n).
*)
let split n =
  let k = nb_chiffres n in
  let k' = if (k mod 2) = 0 then k/2 else (k+1)/2 in 
  let a = slice n 0 k' in
  let b = slice n k' k in
    (k', a, b)
;;

(* IV.2. *)
(*
  n = a + bB
  Donc n*n = a² + ((a + b)² - a² - b²)B + b²B²
*)
let rec carre n =
  let len = nb_chiffres n in
  if len < seuil then mul_naive n n
  else begin
    let (k, a, b) = split n in
    let a2 = carre a in
    let b2 = carre b in
      ajoute (ajoute a2 (mulb (soustrait (soustrait (carre (ajoute a b)) a2) b2) k)) (mulb b2 (2*k))
  end
;;

(* IV.3. *)
(*
  n1 * n2 = ((n1 + n2)² - (n1 - n2)²)/4
  On pose : a = n1 + n2 et b = n1 - n2.
  Alors n1 * n2 = (a² - b²)/4
*)
let min_nat n1 n2 =
  if (compare n1 n2) >= 0 then n2
  else n1
;;

let mul n1 n2 =
  let p, q = if (min_nat n1 n2) = n1 then n2, n1 else n1, n2 in
  let a = ajoute p q in
  let b = soustrait p q in
  let c1 = carre a in
  let c2 = carre b in
  let (q, r) = divise (soustrait c1 c2) 4 in
    q
;;
