(* II. *)
(* II.1. 
ttc = ht + 0.196*ht
Donc ht = ttc/1.196
*)
let ht_de_ttc ttc = ttc /. 1.196;;

(* II.2. *)
let rec fact_aux n accum =
  if n <= 1 then accum
  else fact_aux (n-1) (n*accum)
;;

let fact n = fact_aux n 1;;

(* II.3. *)
let rec pgcd_aux a b = match b with
  | 0 -> a
  | _ -> pgcd_aux b (a mod b)
;;

let pgcd a b = pgcd_aux (abs a) (abs b);;

(* II.4. *)
let rec bezout_aux a b ua ub va vb =
  if b = 0 then (ua, va)
  else bezout_aux (b) (a mod b) (ub) (ua - (a/b) * ub) (vb) (va - (a/b) * vb)
;;

let bezout a b = bezout_aux a b 1 0 0 1;;

(* II.5. *)
let rec puiss_aux x n i accum = 
  (*
    accum = x**i
    n est le nombre de fois qu'il reste à multiplier accum par x
  *)
  if n = 0 then accum
  else if ((i > 0) && (n >= i)) then puiss_aux x (n-i) (2*i) (accum * accum)
  else puiss_aux x (n-1) (i+1) (accum * x)
;;

let puiss x y = match (x, y) with
  | (_, 0) -> 1
  | (0, _) -> 0
  | (1, _) -> 1
  | _ -> puiss_aux x y 0 1
;;

(* II.6. 
3**42 :
accum * x = 3
accum * accum = 3**2
accum * accum = 3**4
accum * accum = 3**8
accum * accum = 3**16
accum * accum = 3**32
accum * x = 3**33
accum * x = 3**34
accum * x = 3**35
accum * x = 3**36
accum * x = 3**37
accum * x = 3**38
accum * x = 3**39
accum * x = 3**40
accum * x = 3**41
accum * x = 3**42
*)

(* III. *)
(* III.1. *)
let rec moyenne_aux l n accum = match l with
  | [] -> accum /. (float_of_int n)
  | a::r -> moyenne_aux r (n+1) (accum +. a)
;;

let moyenne l = match l with
  | [] -> failwith "Error from moyenne(): empty list"
  | _ -> moyenne_aux l 0 0.
;;

(* III.2. *)
(*
etc [1;2;3] vaut [(1-2)² + (2-2)² + (3-2)²]/3
avec 2 la moyenne des valeurs
*)
let rec etc_aux l moy n accum = match l with
  | [] -> accum /. (float_of_int n)
  | a::r -> etc_aux r moy (n+1) (accum +. (a -. moy)**(2.))
;;

let etc l = match l with
  | [] -> failwith "Error from etc(): empty list"
  | _ -> etc_aux l (moyenne l) 0 0.
;;

(* III.3. *)
let rec nieme l n = match l with
  | _ when n < 0 -> failwith "Error from nieme(): n >= 0"
  | [] -> failwith "Error from nieme(): out of range"
  | a::r when n = 0 -> a
  | a::r -> nieme r (n-1)
;;

(* IV. *)
type signe =
  | Carreau
  | Coeur
  | Pique
  | Trefle
;;

type genre =
  | Roi
  | Dame
  | Valet
  | Numero of int
;;

type carte = {
  g : genre;
  s : signe;
};;

let valeur a c = 
	if c.s = a then begin
		if c.g = Valet then 20
		else if c.g = Numero 9 then 14
		else if c.g = Numero 1 then 11
		else if c.g = Numero 10 then 10
		else if c.g = Roi then 4
		else if c.g = Dame then 3
		else 0
	end
	else begin
		if c.g = Numero 1 then 11
		else if c.g = Numero 10 then 10
		else if c.g = Roi then 4
		else if c.g = Dame then 3
		else if c.g = Valet then 2 
		else 0
	end	
;;

(* V. *)
(* V.1. *)
let rec longueur_aux l n = match l with
  | [] -> n
  | a::r -> longueur_aux r (n+1)
;;

let longueur l = longueur_aux l 0;;

(* V.2. *)
let rec reverse_aux l1 l2 = match l1 with
  | [] -> l2
  | a::r -> reverse_aux r (a::l2)
;;

let reverse l = reverse_aux l [];;
