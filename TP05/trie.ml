(* trie un tableau [t] d'élements par ordre croissant (pour l'ordre naturel pour Caml)
   en utilisant une file de priorité.

   Pour cela, on crée une file de priorité, on y met tous les éléments
   du tableau puis on récupère les éléments.
 *)

let trie t =
  let n = Array.length t in
  if n <> 0 then
    let file = cree n (fun x y -> compare x y) t.(0) in
    begin
      for i=0 to n-1 do
				ajoute file t.(i);
      done;
      for i=n-1 downto 0 do
				t.(i) <- retire_max file;
      done
    end
;;

(* trie_gen : ('a -> 'a -> int) -> 'a vect -> unit
   [trie c t] trie un tableau [t] d'éléments par ordre croissant pour
   l'ordre donné par la fonction [c] : [c x y] doit rendre une valeur
   strictement positive si $x>y$, strictement négative si $x<y$ et
   zéro si $x=y$.

  En fait, c'est une généralisation immédiate de [trie], qui pourrait
  s'écrire [t -> tri_gen compare t].

 *)
let trie_gen c t =
  let n = Array.length t in
  if n <> 0 then
    let file = cree n (fun x y -> c x y) t.(0) in
    begin
      for i=0 to n-1 do
				ajoute file t.(i);
      done;
      for i=n-1 downto 0 do
				t.(i) <- retire_max file;
      done
    end
;;
