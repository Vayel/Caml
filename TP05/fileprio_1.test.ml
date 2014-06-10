#use "fileprio_1.ml";;

let print_array a =
	let len = Array.length a in	
	for i=0 to (len-1) do
		print_int a.(i)
	done;
;;

let queue = cree 5 compare 0;;

