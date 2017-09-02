include "des.ml";;
include "bitdebit.ml";;

let choiceForMsgsOrFiles() =
	print_newline();
	print_endline "Voulez-vous crypter ou décrypter :";
	print_endline "1- Un message.";
	print_endline "2- Un fichier.";
	print_endline "3- Une image bitmap 24 bits non compressée.";
	print_newline();
	print_endline "9- Quitter.";
	print_string ">> ";
	
	match read_int() with
	|9 -> 9;
	|1 -> 1;
	|2 -> 2;
	|3 -> 3;
	|_ -> 0;
;;

let choiceForAction() =
	print_newline();
	print_endline "1- Crypter.";
	print_endline "2- Décrypter.";
	print_newline();
	print_endline "9- Quitter.";
	print_string ">> ";
	
	match read_int() with
	|9 -> 9;
	|1 -> 1;
	|2 -> 2;
	|_ -> 0;
;;

let inputKey() =
	print_newline();
	print_endline "Entrez la clé :";
	print_string ">> ";
	
	let key = read_line() in
	if key = "quit();;" then quit();
key;;

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





let ZeroVector (vector : int vect) =
	for i=0 to vect_length(vector)-1 do
		vector.(i) <- 0;
	done;
;;

(* =================================== Point d'entrée pour crypter des fichiers. =================================== *)
let mainFiles() =
	let choice = choiceForAction() in
	let inputFileName = inputFileName() and outputFileName = outputFileName() and inputKey = inputKey() in
	let Key = bitationKey inputKey in
	
	let cipherBits = ref (make_vect 64 0) and fileBits = make_vect 64 0 in
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	
	(* Ouvre les fichiers *)
	print_newline();
	print_endline "Ouverture des fichiers...";
	let inputFileHandle = open_in_bin inputFileName and outputFileHandle = open_out_bin outputFileName in
	let inputFileSize = in_channel_length inputFileHandle in
	
	(* Cryptage *)
	print_newline();
	print_endline "Cryptage...";
	let Byte = ref 0 and nbOfFileBlocks = inputFileSize/8 in
	for i=0 to nbOfFileBlocks-1 do
		print_endline (string_of_int(800*i/inputFileSize) ^ " %");
		for j=0 to 7 do
			Byte := input_byte inputFileHandle;
			for k=(8*j+7) downto (8*j) do
				fileBits.(k) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		cipherBits := DES fileBits Key (choice=1);
		
		for j=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for k=(8*j+7) downto (8*j) do
				Byte := !Byte + (!a * !cipherBits.(k));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		ZeroVector(!cipherBits);
		ZeroVector(fileBits);
	done;
	
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	
	(* Crypte la fin *)
	if (inputFileSize-nbOfFileBlocks*8 > 0) then begin
		for i=0 to inputFileSize-nbOfFileBlocks*8-1 do
			Byte := input_byte inputFileHandle;
			for j=(8*i+7) downto (8*i) do
				fileBits.(j) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		cipherBits := DES fileBits Key (choice=1);
		for i=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for j=(8*i+7) downto (8*i) do
				Byte := !Byte + (!a * !cipherBits.(j));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
	end;
	
	print_endline "100 %";
	
	(* Ferme les fichiers *)
	close_in inputFileHandle;
	close_out outputFileHandle;
;;

(* =================================== Point d'entrée pour crypter des images bitmap. =================================== *)
let mainBitmap() =
	let choice = choiceForAction() in
	let inputFileName = inputFileName() and outputFileName = outputFileName() and inputKey = inputKey() in
	let Key = bitationKey inputKey in
	
	let cipherBits = ref (make_vect 64 0) and fileBits = make_vect 64 0 in
	ZeroVector(!cipherBits);
	ZeroVector(fileBits);
	
	(* Ouvre les images *)
	print_newline();
	print_endline "Ouverture des fichiers...";
	let inputFileHandle = open_in_bin inputFileName and outputFileHandle = open_out_bin outputFileName in
	let inputFileSize = in_channel_length inputFileHandle in
	
	(* TODO : Vérifier si (in_channel_length Handle) est >= ou < 54 *)
	let bitmapSize = (in_channel_length inputFileHandle)-54 in
		if (bitmapSize <= 0) then failwith "Erreur de taille du fichier bitmap !!";
	
	(* Copie l'en-tête de l'image. *)
	for i=0 to 53 do
		output_byte outputFileHandle (input_byte inputFileHandle);
	done;
	
	(* Cryptage *)
	print_newline();
	print_endline "Cryptage...";
	let Byte = ref 0 and nbOfFileBlocks = bitmapSize/8 in
	for i=0 to nbOfFileBlocks-1 do
		print_endline (string_of_int(800*i/inputFileSize) ^ " %");
		for j=0 to 7 do
			Byte := input_byte inputFileHandle;
			for k=(8*j+7) downto (8*j) do
				fileBits.(k) <- !Byte mod 2;
				Byte := !Byte/2;
			done;
		done;
		
		cipherBits := DES fileBits Key (choice=1);
		
		for j=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for k=(8*j+7) downto (8*j) do
				Byte := !Byte + (!a * !cipherBits.(k));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
		
		ZeroVector(!cipherBits);
		ZeroVector(fileBits);
	done;
	
	ZeroVector(!cipherBits);
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
		
		cipherBits := DES fileBits Key (choice=1);
		
		for i=0 to ((vect_length !cipherBits)/8)-1 do
			Byte := 0;
			let a = ref 1 in
			for j=(8*i+7) downto (8*i) do
				Byte := !Byte + (!a * !cipherBits.(j));
				a := !a * 2;
			done;
			output_byte outputFileHandle !Byte;
		done;
	end;
	
	print_endline "100 %";
	
	(* Ferme les images *)
	close_in inputFileHandle;
	close_out outputFileHandle;
;;





(* =================================== Point d'entrée du programme. =================================== *)
let main() =
	match choiceForMsgsOrFiles() with
	|1 -> mainMessages();
	|2 -> mainFiles();
	|3 -> mainBitmap();
	|9 -> quit();
;;
