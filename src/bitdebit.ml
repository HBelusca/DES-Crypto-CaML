(* traduction ASCII --> bits  pour le message *)
let bitation(message : string) =
	let messageLength = string_length message in
	let messageBits = make_vect (8*messageLength) 0 and Byte = ref 0 in
	for i=0 to messageLength-1 do
		Byte := int_of_char(message.[i]);
		for j=(8*i+7) downto (8*i) do
			messageBits.(j) <- !Byte mod 2;
			Byte := !Byte/2;
		done;
	done;
messageBits;;

(* traduction ASCII --> bits  pour la clé *)
let bitationKey(key : string) =
	let keyLength = 7 in
	let keyBits = make_vect (8*keyLength) 0 and Byte = ref 0 in
	for i=0 to (min keyLength (string_length key))-1 do
		Byte := int_of_char(key.[i]);
		for j=(8*i+7) downto (8*i) do
			keyBits.(j) <- !Byte mod 2;
			Byte := !Byte/2;
		done;
	done;
keyBits;;

(* bitation vecteur initial *)
let bitationVector(vector : string) =
	let vectorBits = make_vect 64 0 and Byte = ref 0 in
	for i=0 to (min 8 (string_length vector))-1 do
		Byte := int_of_char(vector.[i]);
		for j=(8*i+7) downto (8*i) do
			vectorBits.(j) <- !Byte mod 2;
			Byte := !Byte/2;
		done;
	done;
vectorBits;;

(* traduction bits --> ASCII  pour le message *)
let debitation(v : int vect) =
	let n = vect_length v in
	if ((n mod 8) <> 0) then failwith "erreur" else begin
		let w = make_vect (n/8) 0 in
		for i = 0 to ((n/8)-1) do
			let a = ref 1 and b = ref 0 in
			for j = (8*i+7) downto (8*i) do
				b := !b  + (!a * v.(j));
				a := !a * 2;
			done;
			w.(i) <- !b;
		done;
		
		(*passage en chaîne de caractères*)
		(* Vérifie s'il n'y a pas des caractères nuls à la fin. *)
		let indice = ref ((vect_length w)-1) in
		while (!indice >= 0) && (w.(!indice) = 0) do
			indice:=!indice-1;
		done;

		let mot = make_string (!indice+1) `\000` in
		for i = 0 to !indice do
			mot.[i] <- char_of_int(w.(i));
		done;
		
		mot;
	end;
;;
