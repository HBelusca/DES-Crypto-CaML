include "fonction f.ml";;
include "key schedule.ml";;

(* permutation initiale *)
let Perminit(vector : int vect) = 
	let n = (vect_length vector) in
	if n <> 64 then
		failwith "<Perminit> : erreur" 
	else
	let w = make_vect 64 0 in
	w.(0) <- vector.(57);
	w.(1) <- vector.(49);
	w.(2) <- vector.(41);
	w.(3) <- vector.(33);
	w.(4) <- vector.(25);
	w.(5) <- vector.(17);
	w.(6) <- vector.(9);
	w.(7) <- vector.(1);
	
	w.(8) <- vector.(59);
	w.(9) <- vector.(51);
	w.(10) <- vector.(43);
	w.(11) <- vector.(35);
	w.(12) <- vector.(27);
	w.(13) <- vector.(19);
	w.(14) <- vector.(11);
	w.(15) <- vector.(3);
	
	w.(16) <- vector.(61);
	w.(17) <- vector.(53);
	w.(18) <- vector.(45);
	w.(19) <- vector.(37);
	w.(20) <- vector.(29);
	w.(21) <- vector.(21);
	w.(22) <- vector.(13);
	w.(23) <- vector.(5);
	
	w.(24) <- vector.(63);
	w.(25) <- vector.(55);
	w.(26) <- vector.(47);
	w.(27) <- vector.(39);
	w.(28) <- vector.(31);
	w.(29) <- vector.(23);
	w.(30) <- vector.(15);
	w.(31) <- vector.(7);
	
	w.(32) <- vector.(56);
	w.(33) <- vector.(48);
	w.(34) <- vector.(40);
	w.(35) <- vector.(32);
	w.(36) <- vector.(24);
	w.(37) <- vector.(16);
	w.(38) <- vector.(8);
	w.(39) <- vector.(0);
	
	w.(40) <- vector.(58);
	w.(41) <- vector.(50);
	w.(42) <- vector.(42);
	w.(43) <- vector.(34);
	w.(44) <- vector.(26);
	w.(45) <- vector.(18);
	w.(46) <- vector.(10);
	w.(47) <- vector.(2);
	
	w.(48) <- vector.(60);
	w.(49) <- vector.(52);
	w.(50) <- vector.(44);
	w.(51) <- vector.(36);
	w.(52) <- vector.(28);
	w.(53) <- vector.(20);
	w.(54) <- vector.(12);
	w.(55) <- vector.(4);
	
	w.(56) <- vector.(62);
	w.(57) <- vector.(54);
	w.(58) <- vector.(46);
	w.(59) <- vector.(38);
	w.(60) <- vector.(30);
	w.(61) <- vector.(22);
	w.(62) <- vector.(14);
	w.(63) <- vector.(6);
w;;


(* Inversion de la permutation initiale *)
let Invert_Perminit (vector : int vect) = match (vect_length vector) with
|64 -> let tempVector = make_vect 64 0 in
		CopyVector tempVector vector;
		
		(*40*)
		vector.(0) <- tempVector.(39);
		vector.(1) <- tempVector.(7);
		vector.(2) <- tempVector.(47);
		vector.(3) <- tempVector.(15);
		vector.(4) <- tempVector.(55);
		vector.(5) <- tempVector.(23);
		vector.(6) <- tempVector.(63);
		vector.(7) <- tempVector.(31);

		(*39*)
		vector.(8) <- tempVector.(38);
		vector.(9) <- tempVector.(6);
		vector.(10) <- tempVector.(46);
		vector.(11) <- tempVector.(14);
		vector.(12) <- tempVector.(54);
		vector.(13) <- tempVector.(22);
		vector.(14) <- tempVector.(62);
		vector.(15) <- tempVector.(30);

		(*38*)
		vector.(16) <- tempVector.(37);
		vector.(17) <- tempVector.(5);
		vector.(18) <- tempVector.(45);
		vector.(19) <- tempVector.(13);
		vector.(20) <- tempVector.(53);
		vector.(21) <- tempVector.(21);
		vector.(22) <- tempVector.(61);
		vector.(23) <- tempVector.(29);

		(*37*)
		vector.(24) <- tempVector.(36);
		vector.(25) <- tempVector.(4);
		vector.(26) <- tempVector.(44);
		vector.(27) <- tempVector.(12);
		vector.(28) <- tempVector.(52);
		vector.(29) <- tempVector.(20);
		vector.(30) <- tempVector.(60);
		vector.(31) <- tempVector.(28);

		(*36*)
		vector.(32) <- tempVector.(35);
		vector.(33) <- tempVector.(3);
		vector.(34) <- tempVector.(43);
		vector.(35) <- tempVector.(11);
		vector.(36) <- tempVector.(51);
		vector.(37) <- tempVector.(19);
		vector.(38) <- tempVector.(59);
		vector.(39) <- tempVector.(27);

		(*35*)
		vector.(40) <- tempVector.(34);
		vector.(41) <- tempVector.(2);
		vector.(42) <- tempVector.(42);
		vector.(43) <- tempVector.(10);
		vector.(44) <- tempVector.(50);
		vector.(45) <- tempVector.(18);
		vector.(46) <- tempVector.(58);
		vector.(47) <- tempVector.(26);

		(*34*)
		vector.(48) <- tempVector.(33);
		vector.(49) <- tempVector.(1);
		vector.(50) <- tempVector.(41);
		vector.(51) <- tempVector.(9);
		vector.(52) <- tempVector.(49);
		vector.(53) <- tempVector.(17);
		vector.(54) <- tempVector.(57);
		vector.(55) <- tempVector.(25);

		(*33*)
		vector.(56) <- tempVector.(32);
		vector.(57) <- tempVector.(0);
		vector.(58) <- tempVector.(40);
		vector.(59) <- tempVector.(8);
		vector.(60) <- tempVector.(48);
		vector.(61) <- tempVector.(16);
		vector.(62) <- tempVector.(56);
		vector.(63) <- tempVector.(24);

|_ -> failwith "<Invert_Perminit> : erreur";;


(* Algorithme principal *)
let DES (input : int vect) (key : int vect) (crypt : bool) = match (vect_length input, vect_length key) with
|(64, 56) -> let cipherVector = ref (Perminit input) and subKeys = (KeySchedule (KeyExtendParity(key))) in
			if crypt then
				for i=0 to 15 do
					FeistelScheme !cipherVector subKeys.(i);
				done
			else
				for i=15 downto 0 do
					FeistelScheme !cipherVector subKeys.(i);
				done;

			PreOutput(!cipherVector);
			Invert_Perminit(!cipherVector);
			!cipherVector;

|_ -> failwith "<DES> : erreur de taille des vecteurs : ENTREE : 64 bits. CLE : 56 bits.";;


(* Nécessaire pour crypter ou décrypter un fichier. *)
let kernCryptOrDecryptFile (inputFileName : string) (outputFileName : string) (key : int vect) (choice : int) (callbackFunc : (int->unit)) =
	callbackFunc 0;
	let init_Vector = make_vect 64 0 in
	let cipherBits = ref (make_vect 64 0) and fileBits = make_vect 64 0 in
	
	(* Ouvre les fichiers *)
	print_newline();
	print_endline "Ouverture des fichiers...";
	let inputFileHandle = open_in_bin inputFileName and outputFileHandle = open_out_bin outputFileName in
	
	let z=ref 0 in
	if (choice = 2) then z:=1;
	let fileSize = (in_channel_length inputFileHandle)-8*(!z) in
		if (fileSize <= 0) then failwith "Erreur de taille du fichier !!";
	
	(* =============== CRYPTAGE =============== *)
	
	(* Génération des sous-clés*)
	print_newline();
	print_endline "Génération des sous-clés...";
	let subKeys = KeySchedule (KeyExtendParity(key)) in
	
	if (choice=1) then begin
		(* Initialise une première fois le vecteur d'initialisation, et l'enregistre. *)
		print_endline "Initialisation du vecteur d'initialisation...";
		let Byte = ref 0 in
		for i=0 to (vect_length init_Vector)-1 do
			init_Vector.(i) <- ((random__int 0xdead) mod 2);
			!cipherBits.(i) <- init_Vector.(i);
		done;
		(* Convertit les bits en octets. *)
		print_endline "Enregistrement du vecteur d'initialisation...";
		for i=0 to ((vect_length init_Vector)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for j=(8*i+7) downto (8*i) do
				Byte := !Byte + (!a * init_Vector.(j));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
	end
	else begin
		(* Lit le vecteur : convertit des octets vers les bits. *)
		print_endline "Lecture du vecteur d'initialisation...";
		let Byte = ref 0 in
		for i=0 to 7 do
			Byte := input_byte inputFileHandle;
			for j=(8*i+7) downto (8*i) do
				init_Vector.(j) <- !Byte mod 2;
				!cipherBits.(j) <- init_Vector.(j);
				Byte := !Byte/2;
			done;
		done;
	end;
	
	
	print_newline();
	print_endline "Cryptage...";
	
	
	let Byte = ref 0 and nbOfFileBlocks = fileSize/8 in
	for i=0 to nbOfFileBlocks-1 do
		callbackFunc (800*i/fileSize);
		for j=0 to 7 do
			Byte := input_byte inputFileHandle;
			for k=(8*j+7) downto (8*j) do
				fileBits.(k) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		xor2 !cipherBits fileBits;
		cipherBits := Perminit !cipherBits;
		if (choice=1) then
			for i=0 to 15 do
				FeistelScheme !cipherBits subKeys.(i);
			done
		else
			for i=15 downto 0 do
				FeistelScheme !cipherBits subKeys.(i);
			done;
		PreOutput(!cipherBits);
		Invert_Perminit(!cipherBits);
		xor2 !cipherBits init_Vector;
		
		for j=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for k=(8*j+7) downto (8*j) do
				Byte := !Byte + (!a * !cipherBits.(k));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		(* Copie les bits du vecteur fileBits, pour l'utiliser plus tard. *)
		CopyVector init_Vector fileBits;
		
		ZeroVector(fileBits);
	done;
	
	ZeroVector(fileBits);
	
	(* Crypte la fin *)
	if (fileSize-nbOfFileBlocks*8 > 0) then begin
		for i=0 to fileSize-nbOfFileBlocks*8-1 do
			Byte := input_byte inputFileHandle;
			for j=(8*i+7) downto (8*i) do
				fileBits.(j) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		xor2 !cipherBits fileBits;
		cipherBits := Perminit !cipherBits;
		if (choice=1) then
			for i=0 to 15 do
				FeistelScheme !cipherBits subKeys.(i);
			done
		else
			for i=15 downto 0 do
				FeistelScheme !cipherBits subKeys.(i);
			done;
		PreOutput(!cipherBits);
		Invert_Perminit(!cipherBits);
		xor2 !cipherBits init_Vector;
		
		for i=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for j=(8*i+7) downto (8*i) do
				Byte := !Byte + (!a * !cipherBits.(j));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		(* Copie les bits du vecteur fileBits, pour l'utiliser plus tard. *)
		CopyVector init_Vector fileBits;
	end;
	
	print_endline "Fin de cryptage.";
	callbackFunc 100;
	
	(* Ferme les fichiers *)
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	close_in inputFileHandle;
	close_out outputFileHandle;
;;

(* Nécessaire pour crypter ou décrypter une image. *)
let kernCryptOrDecryptBitmap (inputFileName : string) (outputFileName : string) (key : int vect) (init_Vector : int vect) (choice : int) (callbackFunc : (int->unit)) =
	callbackFunc 0;
	let cipherBits = ref (make_vect 64 0) and fileBits = make_vect 64 0 in
	
	(* Ouvre les images *)
	print_newline();
	print_endline "Ouverture des fichiers...";
	let inputFileHandle = open_in_bin inputFileName and outputFileHandle = open_out_bin outputFileName in
	
	(* TODO : Vérifier si (in_channel_length Handle) est >= ou < 54 *)
	let bitmapSize = (in_channel_length inputFileHandle)-54 in
		if (bitmapSize <= 0) then failwith "Erreur de taille du fichier bitmap !!";
	
	(* Copie l'en-tête de l'image. *)
	print_endline "Copie de l'en-tête...";
	for i=0 to 53 do
		output_byte outputFileHandle (input_byte inputFileHandle);
	done;
	
	(* =============== CRYPTAGE =============== *)
	
	(* Génération des sous-clés*)
	print_newline();
	print_endline "Génération des sous-clés...";
	let subKeys = (KeySchedule (KeyExtendParity(key))) in
	
	CopyVector !cipherBits init_Vector;
	
	print_newline();
	print_endline "Cryptage...";
	
	let Byte = ref 0 and nbOfFileBlocks = bitmapSize/8 in
	for i=0 to nbOfFileBlocks-1 do
		callbackFunc (800*i/bitmapSize);
		for j=0 to 7 do
			Byte := input_byte inputFileHandle;
			for k=(8*j+7) downto (8*j) do
				fileBits.(k) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		xor2 !cipherBits fileBits;
		cipherBits := Perminit !cipherBits;
		if (choice=1) then
			for i=0 to 15 do
				FeistelScheme !cipherBits subKeys.(i);
			done
		else
			for i=15 downto 0 do
				FeistelScheme !cipherBits subKeys.(i);
			done;
		PreOutput(!cipherBits);
		Invert_Perminit(!cipherBits);
		xor2 !cipherBits init_Vector;

		for j=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for k=(8*j+7) downto (8*j) do
				Byte := !Byte + (!a * !cipherBits.(k));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		(* Copie les bits du vecteur fileBits, pour l'utiliser plus tard. *)
		CopyVector init_Vector fileBits;
		
		ZeroVector(fileBits);
	done;
	
	ZeroVector(fileBits);
	
	(* Crypte la fin *)
	if (bitmapSize-nbOfFileBlocks*8 > 0) then begin
		for i=0 to bitmapSize-nbOfFileBlocks*8-1 do
			Byte := input_byte inputFileHandle;
			for j=(8*i+7) downto (8*i) do
				fileBits.(j) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		xor2 !cipherBits fileBits;
		cipherBits := Perminit !cipherBits;
		if (choice=1) then
			for i=0 to 15 do
				FeistelScheme !cipherBits subKeys.(i);
			done
		else
			for i=15 downto 0 do
				FeistelScheme !cipherBits subKeys.(i);
			done;
		PreOutput(!cipherBits);
		Invert_Perminit(!cipherBits);
		xor2 !cipherBits init_Vector;
			
		for i=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for j=(8*i+7) downto (8*i) do
				Byte := !Byte + (!a * !cipherBits.(j));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		(* Copie les bits du vecteur fileBits, pour l'utiliser plus tard. *)
		CopyVector init_Vector fileBits;
	end;
	
	print_endline "Fin de cryptage.";
	callbackFunc 100;
	
	(* Ferme les images *)
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	close_in inputFileHandle;
	close_out outputFileHandle;
;;
