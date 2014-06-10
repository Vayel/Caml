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

let ajoute q x =
	q.data.(q.size) <- x;
	q.size <- q.size + 1
;;

let max_array_index c a n =
	let k = ref 0 in
  for i = 0 to n do
		(* Si a.(i) est plus grand que le maximum courant. *)
		if (c a.(i) a.(!k)) > 0 then k := i
  done;
  !k
;;

let retire_max q =
	let max_index = max_array_index q.compare q.data (q.size - 1) in
	let max_value = q.data.(max_index) in
	q.data.(max_index) <- q.data.(q.size - 1); (* On met le dernier élément à la place du max. *)
	q.size <- q.size - 1;
	max_value
;;

let est_vide q = 
	q.size <= 0
;;
	
