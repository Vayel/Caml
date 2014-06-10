#use "fileprio_2.ml";;

let print_array a =
	let len = Array.length a in	
	for i=0 to (len-1) do
		print_int a.(i)
	done;
;;

let q = cree 5 compare 0;;
ajoute q 3;;
ajoute q 2;;
ajoute q 4;;
ajoute q 1;;

print_array q.data;;
