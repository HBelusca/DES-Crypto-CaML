include "des.ml";;
include "bitdebit.ml";;
include "key schedule.ml";;

let choiceForAction() =
	let choice = ref 0 in
	while ((!choice <> 1) && (!choice <> 2)) do
		print_newline();
		print_endline "1- Crypter.";
		print_endline "2- Décrypter.";
		print_string ">> ";
		
		choice := read_int();
	done;
	!choice;
;;

let inputKey() =
	print_newline();
	print_endline "Entrez la clé :";
	print_string ">> ";
	
	let key = read_line() in
	if key = "quit();;" then quit();
key;;

let initialVector() =
	print_newline();
	print_endline "Entrez le vecteur d'initialisation:";
	print_string ">> ";
	
	let initvector = read_line() in
	if initvector = "quit();;" then quit();
initvector;;

(* =========================================== MESSAGES =========================================== *)
let inputMessage() =
	print_newline();
	print_endline "Entrez votre message :";
	print_string ">> ";
	
	let message = read_line() in
	if message = "quit();;" then quit();
message;;

(* =========================================== FICHIERS =========================================== *)
let inputFileName() =
	print_newline();
	print_endline "Entrez le chemin d'accès du fichier source :";
	print_string ">> ";
	
	let fileName = read_line() in
	if fileName = "quit();;" then quit();
fileName;;

let outputFileName() =
	print_newline();
	print_endline "Entrez le chemin d'accès du fichier cible :";
	print_string ">> ";
	
	let fileName = read_line() in
	if fileName = "quit();;" then quit();
fileName;;

(* =================================== Point d'entrée pour crypter des messages. =================================== *)
let mainMessages() =
	(* Récupération des différentes chaînes de caractères nécessaires. *)
	let messageACrypter = inputMessage() and inputKey = inputKey() in
	(* Conversion en tableaux de bits. *)
	let messageBits = bitation messageACrypter and Key = bitationKey inputKey in
	(* Calcul du nombre de blocs. *)
	let nbOfBlocks = ((vect_length (messageBits)-1)/64)+1 in
	(* Création du Vecteur du message à crypter. *)
	let messageVector = make_matrix nbOfBlocks 64 0 in
	(* Création du Vecteur du message crypté. *)
	let cipherMsg = make_vect nbOfBlocks [||] in
	
	(* =========================================== CRYPTAGE =========================================== *)
	(* Création des blocs de bits. *)
	for i=0 to (vect_length messageBits)-1 do
		messageVector.(i/64).(i mod 64) <- messageBits.(i);
	done;
	
	(* Cryptage du message... *)
	for i=0 to nbOfBlocks-1 do
		cipherMsg.(i) <- DES messageVector.(i) Key true;
	done;
	
	(* Affichage du message crypté. *)
	print_newline();
	print_newline();
	print_endline "Message crypté :";
	let messageCrypte = ref "" in
	for i=0 to nbOfBlocks-1 do
		messageCrypte := !messageCrypte ^ debitation cipherMsg.(i);
	done;
	print_endline !messageCrypte;
	
	
	(* ========================================== DECRYPTAGE ========================================== *)
	(* Decryptage du message *)
	let messageBits2 = bitation !messageCrypte in
	let nbOfBlocks2 = ((vect_length (messageBits2)-1)/64)+1 in
	let messageVector2 = make_matrix nbOfBlocks2 64 0 in
	let DecipherMsg = make_vect nbOfBlocks2 [||] in
	
	(* Création des blocs de bits. *)
	for i=0 to (vect_length messageBits2)-1 do
		messageVector2.(i/64).(i mod 64) <- messageBits2.(i);
	done;
	
	(* Déryptage du message... *)
	for i=0 to nbOfBlocks2-1 do
		DecipherMsg.(i) <- DES messageVector2.(i) Key false;
	done;
	
	(* Affichage du message décrypté. *)
	print_newline();
	print_newline();
	print_endline "Message décrypté :";
	let messageDecrypte = ref "" in
	for i=0 to nbOfBlocks2-1 do
		messageDecrypte := !messageDecrypte ^ debitation DecipherMsg.(i);
	done;
	print_endline !messageDecrypte;
	
	print_newline();
;;




(* =================================== Point d'entrée pour crypter des fichiers. =================================== *)
let mainFiles() =
	let choice = choiceForAction() in
	let inputFileName = inputFileName() and outputFileName = outputFileName() and Key = bitationKey (inputKey())
	and init_Vector = make_vect 64 0 in
	
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
	let subKeys = KeySchedule (KeyExtendParity(Key)) in
	
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
		print_endline (string_of_int(800*i/fileSize) ^ " %");
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
	
	print_endline "100 %";
	
	(* Ferme les fichiers *)
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	close_in inputFileHandle;
	close_out outputFileHandle;
;;

(* =================================== Point d'entrée pour crypter des images bitmap. =================================== *)
let mainBitmap() =
	let choice = choiceForAction() in
	let inputFileName = inputFileName() and outputFileName = outputFileName() and Key = bitationKey (inputKey())
	and init_Vector = bitationVector (initialVector()) in
	
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
	let subKeys = (KeySchedule (KeyExtendParity(Key))) in
	
	(*if (choice=1) then begin
		(* Initialise une première fois le vecteur d'initialisation, et l'enregistre. *)
		let Byte = ref 0 in
		for i=0 to (vect_length init_Vector)-1 do
			init_Vector.(i) <- 1;
			!cipherBits.(i) <- init_Vector.(i);
		done;
		(* Convertit les bits en octets. *)
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
		let Byte = ref 0 in
		for i=0 to 7 do
			Byte := input_byte inputFileHandle;
			for j=(8*i+7) downto (8*i) do
				init_Vector.(j) <- !Byte mod 2;
				!cipherBits.(j) <- init_Vector.(j);
				Byte := !Byte/2;
			done;
		done;
	end;*)
	CopyVector !cipherBits init_Vector;
	
	print_newline();
	print_endline "Cryptage...";
	
	let Byte = ref 0 and nbOfFileBlocks = bitmapSize/8 in
	for i=0 to nbOfFileBlocks-1 do
		print_endline (string_of_int(800*i/bitmapSize) ^ " %");
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
	
	print_endline "100 %";
	
	(* Ferme les images *)
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	close_in inputFileHandle;
	close_out outputFileHandle;
;;





(* =================================== Point d'entrée du programme. =================================== *)
let main() =
	let choice = ref 0 in
	while (!choice <> 9) do
		print_newline();
		print_endline "Voulez-vous crypter ou décrypter :";
		print_endline "1- Un message.";
		print_endline "2- Un fichier.";
		print_endline "3- Une image bitmap 24 bits non compressée.";
		print_newline();
		print_endline "9- Quitter.";
		print_string ">> ";
		
		choice := read_int();
		begin match !choice with
		|1 -> mainMessages();
		|2 -> mainFiles();
		|3 -> mainBitmap();
		|_ -> ();
		end;
	done;
;;
