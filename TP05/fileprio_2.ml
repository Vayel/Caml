type 'a fileprio = {
	data : 'a array; 
	mutable size : int;
	compare : 'a -> 'a -> int;
};;

let cree n c x =
	{
		data = Array.make n x;
		size = 0;
		compare = c;
	}
;;

(* swap a i j inverse a.(i) et a.(j) *)
let swap a i j =
	let tmp = a.(i) in
	a.(i) <- a.(j);
	a.(j) <- tmp
;;

(* 
	a est un tableau trié.
	pos a x renvoie l'index où doit être inséré x dans 
	a[:n+1] pour conserver a[:n+1] trié selon c.
*)
let pos a c x n =
	let k = ref 0 in
  for i = 0 to n do
		(* Si x est plus grand que a.(i). *)
		if (c x a.(i)) >= 0 then k := i+1
  done;
  !k
;;

let ajoute q x =
	let p = pos q.data q.compare x (q.size - 1) in
	q.data.(q.size) <- x;
	for i = p to q.size - 1 do
		swap q.data i q.size;
	done;
	q.size <- q.size + 1
;;

(* On retourne le dernier élément et décrémente la taille *)
let retire_max q =
	q.size <- q.size - 1;
	q.data.(q.size)
;;

let est_vide q = 
	q.size <= 0
;;
	
