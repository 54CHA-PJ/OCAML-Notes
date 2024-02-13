type arbre = V| N of int*arbre*arbre;;

let rec taille a = match a with
	|V -> 0
	|N(e,g,d) -> 1 + taille g + taille d;;

let rec hauteur a = match a with
	|V -> -1
	|N(e,g,d) -> 1 + (max (hauteur g) (hauteur d));;

let rec ajoute x abr = match abr with
	|V -> N(x,V,V)
	|N(e,g,d) when e>x -> N(e,ajoute x g, d)
	|N(e,g,d) 			 -> N(e, g,ajoute x d);;

let rec aol l = match l with
	|[] -> V
	|t::q -> ajoute t (aol q);;

let loa a = 
	let rec aux a acc = match a with
	|V -> acc
	|N(e,g,d) -> e::(aux g (aux d acc))
	in aux a [];;

let rec egalite a1 a2 = match a1, a2 with
	|V,V -> true
	|V,_ |_,V -> false
	|N(e1,g1,d1),N(e2,g2,d2) -> (e1=e2)&&(egalite g1 g2)&&(egalite d1 d2);;
	
let profondeur a p = 
	(* envoie une liste de tous les noueuds de profonder n *)
	let rec aux a ind acc = match a with
		|V -> acc
		|N(e,g,d) when p = ind -> e::acc
		|N(e,g,d) -> aux g (ind+1) (aux d (ind+1) acc)
	in aux a 0 [];;

let rec est_sous_arbre b a = match a with
	|V -> (b=V)
	|N(e,g,d) -> (egalite a b)||(est_sous_arbre b g)||(est_sous_arbre b d);;

let testabr a = 
	let rec aux a = match a with
	(*renvoie bool, min a, max a*)
	|V -> (true, max_int, min_int)
	|N(e,g,d) -> let (boolg, ming, maxg), (boold, mind, maxd) =
						  (aux g, aux d) in
						  (boolg&&boold&&(maxg<e)&&(mind>e), min e ming, max e maxd)
	in let (bool,_,_) = aux a in bool;;
















	
