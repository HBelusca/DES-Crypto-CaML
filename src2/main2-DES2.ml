include "perminit.ml";;
include "fonction f.ml";;
include "doublekeyschedule.ml";;

let DES (input : int vect) (key : int vect) (crypt : bool) =
	if ((vect_length input = 64) && (vect_length key = 56)) then begin
		let cipherVector = ref (perminit input) and subKeys = KeySchedule (key) in
		
		if crypt then
			for i=0 to 15 do
				cipherVector := FeistelScheme !cipherVector subKeys.(i);
			done
		else
			for i=15 downto 0 do
				cipherVector := FeistelScheme !cipherVector subKeys.(i);
			done;
		
		cipherVector := PreOutput !cipherVector;
		cipherVector := Invert_perminit !cipherVector;
		!cipherVector;
		
	end
	else
		failwith "<DES> : erreur de taille des vecteurs : ENTREE : 64 bits. CLE : 56 bits."
;;

let DES2 (input : int vect) (key : int vect) (crypt : bool) =
	if ((vect_length input = 64) && (vect_length key = 112)) then
		let subKeys2 = doubleks (key) in
		if crypt then
			let a = DES (input subKeys2.(0) true) in
			let b = DES (a subKeys2.(1) true)
		else
			let a = DES (input subKeys2.(1) false) in
			let b = DES (a subKeys2.(0) false)
	else
	failwith "<DES2> : erreur de taille des vecteurs : ENTREE : 64 bits. CLE : 112 bits.";
b;;
