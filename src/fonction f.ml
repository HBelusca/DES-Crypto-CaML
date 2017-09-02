include "S-box.ml";;
include "vectors.ml";;

(* ========================= Déclaration des procédures utilisées par la fonction F. ========================= *)

(* scission des blocs *)
let bloc_g(v : int vect) = match (vect_length v) with
|64 -> let w = make_vect 32 0 in
	   for i = 0 to 31 do
		w.(i) <- v.(i);
	   done;
	   w;
|_ -> failwith "<bloc_g> : erreur";;

let bloc_d(v : int vect) = match (vect_length v) with
|64 -> let w = make_vect 32 0 in
	   for i = 0 to 31 do
		w.(i) <- v.(i+32);
	   done;
	   w;
|_ -> failwith "<bloc_d> : erreur";;

(* Réunion de 2 blocs *)
let reunion (left : int vect) (right : int vect) = match (vect_length left, vect_length right) with
|(32, 32) -> let z = make_vect 64 0 in
				for i=0 to 31 do
					z.(i) <- left.(i);
				done;
				for i=0 to 31 do
					z.(i+32) <- right.(i);
				done;
				z;
|_ -> failwith "<reunion> : erreur de taille.";;

(* fonction de développement E*)
let F_DevelopE(v : int vect) =
	let w = make_vect 48 0 in
	w.(0) <- v.(31);
	w.(1) <- v.(0);
	w.(2) <- v.(1);
	w.(3) <- v.(2);
	w.(4) <- v.(3);
	w.(5) <- v.(4);
	
	w.(6) <- v.(3);
	w.(7) <- v.(4);
	w.(8) <- v.(5);
	w.(9) <- v.(6);
	w.(10) <- v.(7);
	w.(11) <- v.(8);
	
	w.(12) <- v.(7);
	w.(13) <- v.(8);
	w.(14) <- v.(9);
	w.(15) <- v.(10);
	w.(16) <- v.(11);
	w.(17) <- v.(12);
	
	w.(18) <- v.(11);
	w.(19) <- v.(12);
	w.(20) <- v.(13);
	w.(21) <- v.(14);
	w.(22) <- v.(15);
	w.(23) <- v.(16);
	
	w.(24) <- v.(15);
	w.(25) <- v.(16);
	w.(26) <- v.(17);
	w.(27) <- v.(18);
	w.(28) <- v.(19);
	w.(29) <- v.(20);
	
	w.(30) <- v.(19);
	w.(31) <- v.(20);
	w.(32) <- v.(21);
	w.(33) <- v.(22);
	w.(34) <- v.(23);
	w.(35) <- v.(24);
	
	w.(36) <- v.(23);
	w.(37) <- v.(24);
	w.(38) <- v.(25);
	w.(39) <- v.(26);
	w.(40) <- v.(27);
	w.(41) <- v.(28);
	
	w.(42) <- v.(27);
	w.(43) <- v.(28);
	w.(44) <- v.(29);
	w.(45) <- v.(30);
	w.(46) <- v.(31);
	w.(47) <- v.(0);
w;;

(* addition bit à bit. vecteurs de même taille. *)
let xor1 (u : int vect) (v : int vect) = match (vect_length u = vect_length v) with
|true -> let length = vect_length u in
	let w = make_vect length 0 in
	for i=0 to (length-1) do
		w.(i) <- ((u.(i)+v.(i)) mod 2);
	done;
	w;
|_ -> failwith ("<xor1> : erreur de taille : u = " ^ string_of_int(vect_length(u)) ^ " ; v = " ^ string_of_int(vect_length(v)));;

let xor2 (dest : int vect) (source : int vect) = match (vect_length dest = vect_length source) with
|true -> let length = vect_length dest in
	for i=0 to (length-1) do
		dest.(i) <- ((dest.(i)+source.(i)) mod 2);
	done;
|_ -> failwith ("<xor2> : erreur de taille : dest = " ^ string_of_int(vect_length(dest)) ^ " ; source = " ^ string_of_int(vect_length(source)));;

(* Permutation P*)
let F_PermP(vector : int vect) = match (vect_length vector) with
|32 -> let tempVector = make_vect 32 0 in
		CopyVector tempVector vector;
		
		vector.(0) <- tempVector.(15);
		vector.(1) <- tempVector.(6);
		vector.(2) <- tempVector.(19);
		vector.(3) <- tempVector.(20);

		vector.(4) <- tempVector.(28);
		vector.(5) <- tempVector.(11);
		vector.(6) <- tempVector.(27);
		vector.(7) <- tempVector.(16);

		vector.(8) <- tempVector.(0);
		vector.(9) <- tempVector.(14);
		vector.(10) <- tempVector.(22);
		vector.(11) <- tempVector.(25);

		vector.(12) <- tempVector.(4);
		vector.(13) <- tempVector.(17);
		vector.(14) <- tempVector.(30);
		vector.(15) <- tempVector.(9);

		vector.(16) <- tempVector.(1);
		vector.(17) <- tempVector.(7);
		vector.(18) <- tempVector.(23);
		vector.(19) <- tempVector.(13);

		vector.(20) <- tempVector.(31);
		vector.(21) <- tempVector.(26);
		vector.(22) <- tempVector.(2);
		vector.(23) <- tempVector.(8);

		vector.(24) <- tempVector.(18);
		vector.(25) <- tempVector.(12);
		vector.(26) <- tempVector.(29);
		vector.(27) <- tempVector.(5);

		vector.(28) <- tempVector.(21);
		vector.(29) <- tempVector.(10);
		vector.(30) <- tempVector.(3);
		vector.(31) <- tempVector.(24);

|_ -> failwith "<F_PermP> : erreur de taille du vecteur";;

(* Utilisation des S-Box *)
let Use_SBox(motB : int vect) = match (vect_length motB) with
|48 -> let C = make_vect 32 0 in
		for i=0 to 7 do
			let line = 2*motB.(6*i) + motB.(6*i+5) and
			column = 8*motB.(6*i+1) + 4*motB.(6*i+2) + 2*motB.(6*i+3) + motB.(6*i+4) in
			let number = SBox.(i).(line).(column) in
			C.(4*i) <- number/8 mod 2;
			C.(4*i+1) <- number/4 mod 2;
			C.(4*i+2) <- number/2 mod 2;
			C.(4*i+3) <- number mod 2;
		done;
		C;
|_ -> failwith "<Use_SBox> : Erreur de taille";;


(* ========================= FONCTION F ========================= *)
let FonctionF (r : int vect) (k : int vect) = match (vect_length r, vect_length k) with
|(32, 48) -> let R_Developpe = (F_DevelopE r) in
			 xor2 R_Developpe k;
			 let Mixed_SBoxed = Use_SBox(R_Developpe) in
			 F_PermP(Mixed_SBoxed);
			 Mixed_SBoxed;
|_ -> failwith "<FonctionF> : erreur";;


(* ================ SCHÉMA DE FEISTEL ================== *)
let FeistelScheme (cipherVector : int vect) (subKey : int vect) = match (vect_length cipherVector, vect_length subKey) with
|(64, 48) -> let L = bloc_g cipherVector and R = bloc_d cipherVector in
			 let FdeR = (FonctionF R subKey) in
			 xor2 FdeR L;
			CopyVector cipherVector (reunion R FdeR);
|_ -> failwith "<FeistelScheme> : erreur de taille.";;


(* ================ PRÉSORTIE ================== *)
let PreOutput(cipherVector : int vect) = match (vect_length cipherVector) with
|64 -> let L = bloc_g cipherVector and R = bloc_d cipherVector in
		CopyVector cipherVector (reunion R L);
|_ -> failwith "<PreOutput> : erreur de taille.";;
