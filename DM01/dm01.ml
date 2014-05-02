(* Préliminaires *)
(* 1. *)
(*
  'a vect -> 'a
  Renvoie le minimum du tableau a.
*)
let min_array a =
  let m = ref a.(0) in
  let len = (vect_length a) in
  for i = 1 to (len-1) do
    m := min a.(i) !m
  done;
  !m
;;

(* 2. *)
(*
  'a list -> 'a -> 'a
  Renvoie le minimum de la liste l.
*)
let rec min_list_aux l m = 
  match l with
    | [] -> m
    | x::r -> min_list_aux r (min m x)
;;

(*
  'a list -> 'a
  Renvoie le minimum de la liste l.
*)
let min_list l =
  min_list_aux l (hd l)
;;

(* 3. *)
(*
  int -> bool
*)
let rec even n = 
  match n with
    | 0 -> true
    | _ -> odd (n-1)
and odd n = 
  match n with
    | 0 -> false
    | _ -> even (n-1)
;;

(*
  int -> int -> int
  Calcule x^n par exponentiation rapide.
*)
let rec pow x n = 
  match x, n with
    | 0, _ | 1, _-> x
    | _, 0 -> 1
    | _, _ -> if (even n) then pow (x*x) (n/2)
              else x * pow (x*x) ((n-1)/2)
;;


(* I. *)
(* I.1. *)
(*
  int vect -> int
  Renvoie le coût de la séquence de requêtes satisfaite par une seule tête. 
*)
let cout_une_tete a =
  let acc = ref a.(0) in
  let len = (vect_length a) in
  for i = 1 to (len-1) do
    acc := !acc + abs (a.(i) - a.(i-1))
  done;
  !acc
;;

(* I.2.a. *)
(*
  int vect -> int vect - int
  Renvoie le coût de la séquence de déplacements dep pour la série de
  requêtes req.
*)
let cout_array req dep = 
  let acc = ref 0 in
  let pos = [|0;0|] in
  let len = (vect_length req) in
  for i = 0 to (len-1) do
    let head = dep.(i) in
      let last = pos.(head) in
        acc := !acc + abs (last-req.(i));
        pos.(head) <- req.(i)
  done;
  !acc
;;

(* I.2.b. *)
(*
  int list -> int list -> int -> int -> int -> int
  Renvoie le coût de la séquence de déplacements dep pour la série de
  requêtes req. 
*)
let rec cout_list_aux req dep last0 last1 acc = 
  match req, dep with
    | [], _ | _, [] -> acc
    | pos::r, head::d -> if head = 1 then cout_list_aux r d last0 pos (acc + abs (last1-pos))
                         else cout_list_aux r d pos last1 (acc + abs (last0-pos))                   
;;

(*
  int list -> int list -> int
  Renvoie le coût de la séquence de déplacements dep pour la série de
  requêtes req.
*)
let cout_list req dep =
  cout_list_aux req dep 0 0 0
;;

(* II. *)
(* II.1. *)
(* 
  int * int -> int
  Renvoie le coût minimal pour satisfaire une séquence de eux requêtes.
  
  On regarde toutes les possibilités et retourne le coût minimal.
  Or on a :
  cout_list req [0;0] = cout_list req [1;1]
  cout_list req [0;1] = cout_list req [1;0] 
*)
let cout_opt_2 req = 
  match req with
    | r1, r2 -> let r = [r1;r2] in
                min (cout_list r [0;0]) (cout_list r [0;1])
;;

(* II.2. *)
(*
  bool -> int
*)
let int_of_bool b = 
  match b with
    | false -> 0
    | true -> 1
;;

(*
  int vect -> int
  Renvoie le coût de la séquence de déplacements donnée par la stratégie gloutonne.
*)
let cout_glouton req =
  let acc = ref 0 in
  let pos = [|0;0|] in
  let len = (vect_length req) in
  for i = 0 to (len-1) do
    let n = req.(i) in
      let head = int_of_bool (abs (pos.(0)-n) > abs (pos.(1)-n)) in
        let last = pos.(head) in
          acc := !acc + abs (last-req.(i));
          pos.(head) <- req.(i)
  done;
  !acc
;;

(* II.3. *)
(*
  Requête : [5;3;6]
  Glouton : déplacement [0;0;0], soit un coût de 5+2+3 = 10
  Autre : déplacement [0;1;0], soit un coût de 5+3+1 = 9
*)

(* III. *)
(* III.1. *)
(*
  La séquence contiendra n valeurs et chacune peut
  valoir soit 0 soit 1. Il y a donc 2^n possibilités.
*)

(* III.2.a. *)
(*
  int -> int list -> int list
  Emploie la méthode des divisions successives pour stocker dans l les chiffres
  de n converti en binaire.
*)
let rec bin_of_dec_aux n l =
  match n with
    | 0 -> if l = [] then [0] else l
    | _ -> bin_of_dec_aux (n / 2) ((n mod 2)::l)
;;

(*
  int -> int list
  Renvoie la liste des chiffres de n converti en binaire.
*)
let bin_of_dec n =
  bin_of_dec_aux n []
;;

(*
  'a list -> int -> 'a -> 'a list
  Complète l par la gauche avec x de sorte que (list_length l) = n.
*)
let rec complete_list_aux l n x = 
  match n with
    | 0 -> l
    | _ -> complete_list_aux (x::l) (n-1) x
;;

(*
  'a list -> int -> 'a -> 'a list
  Complète l par la gauche avec x de sorte que (list_length l) = n.
*)
let complete_list l size x =
  let len = (list_length l) in
  if len >= size then l
  else complete_list_aux l (size-len) x
;;

(*
  int -> int -> int list -> int list
  Renvoie la liste [b;b+1;...;e].
*)
let rec range_aux b e acc =
  if b <= e then range_aux b (e-1) (e::acc)
  else acc
;;

(*
  int -> int -> int list
  Renvoie la liste [b;b+1;...;e].
*)
let range b e = 
  range_aux b e []
;;

(*
  int -> (int -> int list)
*)
let enum_map n = 
  fun i -> complete_list (bin_of_dec i) n 0
;;

(*
  int -> int list list
  
  On calcule les nombres compris entre 0 et 2^n - 1.
  Puis on stocke les chiffres dans une liste : [1;0;0].
  On la complète avec des 0 à gauche : si n = 4, [0;1;0;0].
*)  
let enum n =
  map (enum_map n) (range 0 ((pow 2 n) - 1))
;;

(* III.2.b. *)
(*
  int list -> (int list -> int)
*)
let force_brute_map req =
  fun dep -> cout_list req dep
;;  

(*
  int list -> int
  On génère toutes les possibilités de déplacements via enum(),
  puis on calcule le coût de chaque séquence et retourne le minimum.
*)
let force_brute_list req =
  let e = enum (list_length req) in
  let costs = (map (force_brute_map req) e) in
    min_list costs
;;

(* III.3.a. *)
(*
  int -> int -> int vect
*)
let binaire n nb_bits =
  vect_of_list ((enum_map nb_bits) n)
;;

(* III.3.b. *)
(*
  int vect -> int
  On génère toutes les possibilités de séquences de déplacements 
  puis on calcule le coût pour chacune et retourne le minimum.
*)
let force_brute_array req =
  let len = vect_length req in
  let r = vect_of_list (range 0 ((pow 2 len) - 1)) in
  let m = ref (cout_array req (binaire r.(0) len)) in
  for i = 1 to (len-1) do
    m := min (!m) (cout_array req (binaire r.(i) len))
  done;
  !m
;;

(* III.4. *)
(*
  Il suffit de supprimer les cas où les 1 remplacent les 0 et inversement.
  Par exemple, ces deux séquences ont le même coût : [1;0;0;1;0;1;1] et [0;1;1;0;1;0;0].
  Or [1;0;0;1;0;1;1] = [1;1;1;1;1;1;1] - [0;1;1;0;1;0;0]
  En notant C(dep) le coût de la séquence de déplacements dep, on a, pour n = 3 :
  C([1;1;1]) = C([0;0;0])
  C([1;1;0]) = C([0;0;1])
  C([1;0;1]) = C([0;1;0])
  C([1;0;0]) = C([0;1;1])
  Ainsi, dans enum, il ne faut pas appeler range 0 ((pow 2 n) - 1) mais seulement 
  range 0 ((pow 2 (n-1)) - 1).
*)

(* III.5. *)
(*
  force_brute_list :
  	- Appel à enum() : O() + O() + O() = O()
  	- Calcul du coût de chaque séquence de déplacements : O()
  	- Renvoi le minimum d'une liste : O(n) 
  	force_brute_list() est donc un O().
  	TODO
  	
  force_brute_array :
  	- Calcul de la taille de req : O(n)
  	- Appel à range() : O()
  	- len appels à binaire() puis cout_array() : O()
  	force_brute_array() est donc un O().
  	TODO
*)

(* IV. *)
(* IV.1. *)
(*
  \lbrace \lbrace 0, r_{n} \rbrace \rbrace \cup \lbrace \lbrace r_{k}, r_{n} \rbrace, k \in ℕ, 1 \leq k \leq n-1 \rbrace
*)

(* IV.2.a. *)
(*
  t_{3} = (t_{3, 0}, t_{3, 1}, t_{3, 2}) = (23, 21, 20)
*)

(* IV.2.b. *)
(*
  TODO
*)

(* IV.2.c. *)
(*
  TODO
*)

(* IV.2.d. *)
(*
  int vect -> int vect -> int -> int vect
  On génère simplement t_{k+1} selon IV.2.c.
*)
let update tk req k =
  let t = make_vect (k+1) 0 in
  let m = ref (tk.(0) + abs (req.(k+1) - req.(0))) in
  let delta = abs (req.(k+1) - req.(k)) in
  for i = 0 to (k-1) do
    t.(i) <- tk.(i) + delta;
    m := min (!m) (tk.(i) + abs (req.(k+1) - req.(i)))
  done;
  t.(k) <- !m;
  t
;;

(* IV.2.e. *)
(*
  update() est clairement un O(k).
*)

(* IV.3. *)
(*
  int vect -> int
  Retourne le coût optimal pour la séquence de requêtes req.
  req est de la forme [|r_{1};r_{2};r_{3};...|].
  On calcule t_{n} et retourne son minimum.
*)
let cout_opt req =
  let r = concat_vect [|0|] req in
  let n = vect_length req in
  let tk = ref [|r.(1)|] in
  for k = 1 to (n-1) do
    (* tk = t_{k} *)
    tk := update (!tk) r k
    (* tk = t_{k+1} *)
  done;
  (* tk = t_{n} *)
  min_array !tk
;;

(* IV.4. *)
(*
  - appel à vect_length : O(n)
  - environ n appels à update() : O($\sum_{k=1}^{n-1} (k)$) = O(n*n)
  - min_array : O(n)
  cout_opt() est donc un O(n*n).
  
  En espace, cout_opt() TODO
  
  En effectuant 10^{9} opérations à la seconde, on en fait 10^{6} en
  une milliseconde. 
  On peut donc espérer traiter une séquence de requêtes d'une taille de l'ordre 
  de $ \sqrt[2]{10^{6}} = 1000$. 
*)

(* V. *)
(*
  TODO
*)
