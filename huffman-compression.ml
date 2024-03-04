type arbre =
| C of char
| N of arbre * arbre
type mot = int list;;
(* Question 1 *)
let t = N(N(N(C 'n' , C 'f') , C 'a' ) , N(C 's' , N(C 'i' , C 't') ) )  ;; 
(* Question 2 *)
let rec taille arbre =
  match arbre with
  | C _ -> 1  (* nœud feuille, on renvoie 1 *)
  | N (gauche, droite) -> taille gauche + taille droite ;; (* on calcule la taille des sous-arbres gauche et droit et on les additionne *)

(* test : print le resultat de taille avec notre arbre t *)
 Printf.printf " le nombre de caractère : %d\n" (taille t) ;;
 let () = assert(taille t = 6) ;;

(* Question 3 *)
let rec contient c arbre =
  match arbre with
  | C x -> x = c
  | N (gauche, droite) -> contient c gauche || contient c droite ;;

(* test : print le resultat de contient avec notre arbre t *) 
Printf.printf("le caractère 'a' est-il dans l'arbre ? : %b\n") (contient 'a' t) ;;
Printf.printf( "le caractère 'x' est-il dans l'arbre ? : %b\n") (contient 'x' t) ;;
let () = assert (contient 'a' t = true && contient 'x' t = false) ;;

(* Question 4  : *)
let rec code_char c arbre =
  match arbre with
  | C x -> if x = c then [] else failwith "erreur"
  | N (gauche , droite) -> if contient c gauche then 0::code_char c gauche else 1::code_char c droite ;;



let () = assert(code_char 'i' t = [1;1;0]) ;; 
 
  (* Question 5 : 
  La complexité de la fonction code_char dépend de la profondeur de la feuille correspondant au caractère c dans l'arbre de Huffman. 
  A chaque nœud rencontré lors de la descente dans l'arbre, 
  la fonction doit effectuer une comparaison pour savoir si c se trouve dans le sous-arbre gauche ou droit , 
  donc , plus la feuille correspondant à c est profonde dans l'arbre, 
  plus la fonction devra effectuer de comparaisons et donc aura une complexité élevée.

  On peut facilement lier cette complexité au nombre de caractères dans l'arbre en remarquant que le nombre maximal de 
  comparaisons effectuées par la fonction code_char correspond à la profondeur maximale des feuilles de notre arbre.
   Or, cette profondeur maximale est atteinte pour une feuille qui est à une distance maximale de la racine, 
   Alors , si l'arbre contient n feuilles, la profondeur maximale des feuilles est log2(n), 
   et la fonction code_char a donc une complexité en O(log n).   

  
*)

  (* Question 6 *) 

  let rec reconnait m arbre =
    match arbre with
    | C c -> m = []
    | N (gauche , droite) ->
        match m with
        | [] -> false
        | 0::qs -> reconnait qs gauche
        | 1::qs -> reconnait qs droite
        | _ -> failwith "erreur"

  ;;
  
  
(* test : print le resultat de reconnait avec notre arbre  , et en inserant des testes sous forme de assert*)
let test_reconnait m =
  let result = reconnait m t in
  Printf.printf "Mot %s reconnu ? %b\n" (String.concat "" (List.map string_of_int m)) result 
  ;;

let () =
  test_reconnait [0;0;0];
  test_reconnait [0;0;1];
  ;;

  let () = assert ( reconnait [0;0;0] t = true && reconnait [0;0;1] t = true && reconnait[0;0;0;0] t = false ) ;; 



(* Question 7  *)
let rec decode_mot_simple mot arbre = 
  match mot ,  arbre with 
  | [] , C c -> c 
  | _::_ , C _ | [] , N _ -> failwith "erreur"
  | x::aa , N (gauche, droite) -> if x = 0 then decode_mot_simple aa gauche
  else decode_mot_simple aa droite ;; 

  let ( ) = assert ( decode_mot_simple [0;0;0] t = 'n' ) ;;
  let () = assert (decode_mot_simple [1;0] t = 's' ) ;; 




(* Question 8 : 
   En supposant que les entrées "mot" et "arbre" ont des tailles respectives "n" et "m", 
   la complexité dans le pire des cas de "decode_mot_simple" dépendra de la profondeur de 
   l'arbre et de la longueur de l'entrée "mot". 
Dans le pire des cas, l'arbre sera un arbre avec tous les nœuds sur un côté, 
   ce qui rendra la complexité  de l'ordre de  O(n).

, si l'arbre est équilibré, la complexité dans le pire des cas serait de O(log(m)), 
puisque la fonction effectue essentiellement une recherche binaire sur l'arbre.

la complexité  de "decode_mot_simple" peut donc varier de O(n) à O(log(m)), 
en fonction de la structure de l'arbre.

*)


(* Partie 2*)

(* Question 9 : *)
let rec code_texte l t =
  match l with
  | [] -> []
  | c :: l' -> (code_char c t) @ (code_texte l' t)

let t1 = N(N(N (C 'n',C 'f') , C 'a') , N(C 's', N(C 'i',C 't') ) )  ;; 

(*test code_test *)
let message = ['s';'a';'t';'i'; 's';'f';'a'; 'i';'s';'a';'n'; 't'] ;;
let message1 = ['a' ;'i' ; 't'];; 

let () = assert( code_texte message t = [1;0;0;1;1;1;1;1;1;0;1;0;0;0;1;0;1;1;1;0;1;0;0;1;0;0;0;1;1;1]);;
 let() = assert (code_texte message1 t = [0;1;1;1;0;1;1;1]  ) ;;

(*Question 10 : 
Soit un mot binaire m, et supposons qu'il existe deux entiers k1 et k2 tels que les k1 premiers bits de m sont le code valide de deux 
caractères c1 et c2 respectivement. 
Par définition, cela implique que les k1 premiers bits de m sont une chaîne de bits valide pour l'arbre de Huffman t.
Comme l'arbre de Huffman est un arbre binaire, le code de chaque caractère est une chaîne de bits qui ne peut etre préfixe d'un autre 
code de caractère.
Donc, les k1 premiers bits de m ne peuvent être le début du code d'aucun autre caractère.

Alors, s'il existe deux entiers k1 et k2 tels que les k1 premiers bits de m sont le code valide de deux caractères différents, 
cela signifie que les k1 premiers bits de m sont le code valide de c1, mais il ne peut y avoir de caractère codé par les k2-k1 bits 
suivants. 
cela implique que les k2 premiers bits de m sont le code valide de c2, et quil ne peut y avoir de caractère codé par 
les k1-k2 bits suivants. 
Donc, m ne peut pas représenter deux codes valides de caractères différents avec un même préfixe.
Alors, il ne peut y avoir au plus qu'un seul entier k tel que les k premiers bits de m soient le code valide d'un caractère.
*)

(* Question 11 :*)

let decode_mot mot arbre =
  let rec aux mot arbre acc =
    match mot, arbre with
    | [], C c -> (c, acc)
    | _::_, C c -> (c, mot)
    | x::aa, N (gauche, droite) ->
        if x = 0 then aux aa gauche acc
        else aux aa droite acc
    | [], N _ -> failwith "erreur"
  in
  aux mot arbre []


(* testes *)
let test1 = decode_mot [0;1] t1;;
assert (test1 = ('a', []));;
let test2 = decode_mot [0;0;1; 1; 1] t1;;
assert (test2 = ('f', [1; 1]));;

(*Question 12 : *)
let decode_texte mot arbre =
  let rec aux mot ss =
    if mot = [] then
      List.rev ss
    else
      let (c, mm) = decode_mot mot arbre in
      aux mm (c :: ss)
  in
  aux mot []

(* Testes : *)
let test1 = decode_texte [1; 0; 0; 1; 1; 1; 1; 1; 1; 0; 1; 0; 0; 0; 1; 0; 1; 1; 1; 0; 1; 0; 0; 1; 0; 0;0;1;1;1] t1;;
assert (test1 = ['s'; 'a'; 't'; 'i'; 's'; 'f'; 'a'; 'i'; 's'; 'a'; 'n'; 't']);;
let test2 = decode_texte [0;1;1;1;0;1;1;1] t1;; 
Printf.printf("Test 2: %s\n") (String.concat "" (List.map (String.make 1) test2));;
assert (test2 = ['a' ; 'i' ; 't']) ;; 

(*Partie 3 *)
  
  (* Question 13 : 
  Pour former l'arbre exemple de la premiere page ( l'abre t1 declaré en haut ) on suit ces etapes dans l'ordre : 
     1- On commennce par regrouper 'f' et 'n' pour creer un arbre N(C 'n' , C 'f' ) 
     2- Ensuite , on regroupe 'i' et 't' pour creer un autre arbre N(C 'i' , C 't' )
     3- Puis  , pour creer N(N(C 'n' , C'f' ) C'a' ) , il faut qu'on regroupe 'a' avec l'abre précédant N(C 'n' , C 'f' )
     4- Apres , il fait qu'on regroupe le 's' avec l'abre qu'on a eu a la 2 eme etapes N(C 'i' , C 't' ) , pour avoir l'arbre N(C 's', N(C 'i', C 't'))
     5-et au final , il faut qu'on fusionne les 2 arbres obtenues a l'etapes 3 et 4 pour obtenir l'arbre  t1 : N(N(N (C 'n',C 'f') , C 'a') , N(C 's', N(C 'i',C 't') ) ) 
  
  *)

  


(* Question 14 : *)
let rec poids arbre stats =
  match arbre with
  | C c -> stats.(Char.code c)
  | N (gauche, droite) -> poids gauche stats + poids droite stats


(* Testes : *)
let stats = Array.make 256 0
let () =
  stats.(Char.code 'a') <- 3;
  stats.(Char.code 'f') <- 1;
  stats.(Char.code 'i') <- 2;
  stats.(Char.code 'n') <- 1;
  stats.(Char.code 's') <- 3;
  stats.(Char.code 't') <- 2


let test1 = poids t1 stats
let () = assert (test1 = 12)


let t2 = C 'a'

let test2 = poids t2 stats
let () = assert (test2 = 3)


let t3 = N(C 'a', C 's')


let test3 = poids t3 stats
let () = assert (test3 = 6)


let () =
  Printf.printf "Test 1 (poids de t1): %d\n" test1;
  Printf.printf "Test 2 (poids de t2): %d\n" test2;
  Printf.printf "Test 3 (poids de t3): %d\n" test3

  (* Question 15 : 
  le type de file dans ce code est 'a *arbre ,
  'a ici c'est le type des priorités comme dans l'enoncé 
  
  *)


  (*Question 16 
  On pourrait avoir des cas ou notre code va echoué : 
  par exemple ,  si stats est vide ou ne contient que des occurrences nulles , ou bien des valeurs negatives dans le tableau , la fonction huffman ne pourra pas construire un arbre de Huffman correcte, entraînant une erreur.
un autre cas aussi  , si la fonction extract_min est appelée alors que la file de priorité est vide, cela peut provoquer une erreur .

     
  
  *)


  (*Partie 4 *)
  type 'a tas =
    | E
    | N of 'a * 'a tas * 'a tas
    ;;

    let b = N (3 , N (4 , N (8 , E , E ) , N (5 , E , E )) , N (6 , N (7 , E , E ) , E )) ;; 
    
;;
  
    (*Question 17 : 
    
    Dans un tas de Braun , l'élément minimal trouve à la racine de l'arbre ( tout en haut ) , 
    et l'element maximal se trouve au niveau d'une feuille 
     
    *)

   (** (*Question 18 : 
    Pour chaque taille, il n'y a qu'un seul arbre de Braun possible, car les arbres de Braun ont une structure très contrainte 
    *)
  
    (*arbre de taille 2 *)
   let x2 =  N (_, E, N (_, E, E)) ;; 
    (*arbre de taille 3 *)
    let x3 = N ( _ , N ( _ , E, E), N (_, E, E))

    (*arbre de taille 4 *)
    let x4 = N (_, N (_, N (_, E, E), E), N (_, E, E))
    (*arbre de taille 5 *)
    let x5 = N (_, N (_, N (_, E, E), N (_, E, E)), N (_, E, E))
    (*arbre de taille 6 *)
    let x6 = N (_, N (_, N (_, E, E), N (_, E, E)), N (_, N (_, E, E), E))
    (*arbre de taille 7 *)
    let x7 = N (_, N (_, N (_, E, E), N (_, E, E)), N (_, N (_, E, E), N (_, E, E)))
  *)
    
    (*Question 19 
    La taille d'un arbre de Braun est le nombre total de nœuds qu'il contient, tandis que la hauteur ou profondeur d'un arbre de Braun
    est le nombre maximal de nœuds le long d'un chemin de la racine à une feuille.   
    La relation entre la taille  et la hauteur d'un arbre de Braun peut être exprimée de la manière suivante :
   Taile = 2**Hauteur - 1 

    *)

    (*Question 20 : *) 
    let rec ajoute x = function
  | E -> N (x, E, E)
  | N (n, t1, t2) ->
    if x <= n then
      N (x, ajoute n t2, t1)
    else
      N (n, ajoute x t2, t1)

(* Testes  *)
let tasB = E ;;
let tasB = ajoute 3 tasB ;;
let tasB = ajoute 3 tasB ;;
let tasB = ajoute 10 tasB ;;
let tasB = ajoute 2 tasB ;;
let tasB = ajoute 3 tasB ;;
let tasB = ajoute 8 tasB ;;
(* on teste taille de notre tas qui doit etre a 7*)
(* on cree la fonction taille *)
let rec taille = function
  | E -> 0
  | N (_, t1, t2) -> 1 + taille t1 + taille t2
  ;; 

  let t_taille = taille tasB ;;

  Printf.printf "La taille du tas est : %d\n" t_taille;;

  (*Question 21 *)
  (* On applique la fonction ajoute a l'élément 3 et au tas de Braun precedent *)
  let tas  = N (3 , N (4 , N (8 , E , E ) , N (5 , E , E )) , N (6 , N (7 , E , E ) , E )) ;; 
  let tas = ajoute 3 tas ;;

  let t_taille = taille tas ;;

  Printf.printf "La taille du tas est : %d\n" t_taille;; 


  (*Question 22 :
  on a t un tas de Braun , 
  Nous allons démontrer que la taille de ajoute (x , t ) est égale à la taille de t plus 1 par recurrence structurelle sur t :
  Initialisation : 
  On a t = E ( tas vide )
  alors (x,E ) = N(x , E , E ) la taille de t est 0 et la taille de ajoute (x , t ) est 1 
  donc la propriété est vraie pour t = E
  Hérédité :
  On suppose que la propriété est vraie pour t de taille n 
  donc ajoute ( x, t ) =  n + 1 est vrai et on vérifie pour t de taille n + 1
  
Étape de récurrence :
 Considérons un tas t de taille n + 1. 
 Le tas t est de la forme N(n, t1, t2) où t1 et t2 sont des tas de Braun. 
 D'après la définition de la fonction ajoute, nous avons deux cas possibles :

Si x ≤ n, alors ajoute(x, N(n, t1, t2)) = N(x, ajoute(n, t2), t1). 
Par l'hypothèse de récurrence, la taille de ajoute(n, t2) est taille(t2) + 1. 
Alors, la taille de ajoute(x, N(n, t1, t2)) est (taille(t2) + 1) + taille(t1) = taille(t1) + taille(t2) + 1 = taille(N(n, t1, t2)) + 1, 
ce qui est en accord à notre hypothèse de récurrence.

Sinon, ajoute(x, N(n, t1, t2)) = N(n, ajoute(x, t2), t1). 
Par l'hypothèse de récurrence, la taille de ajoute(x, t2) est taille(t2) + 1. 
Alors, la taille de ajoute(x, N(n, t1, t2)) est (taille(t2) + 1) + taille(t1) = taille(t1) + taille(t2) + 1 = taille(N(n, t1, t2)) + 1, 
ce qui aussi en accord à notre hypothèse de récurrence.

Dans les deux cas, la taille de ajoute(x, t) est 1 de plus que la taille de t. 
Par conséquent, nous avons démontré par récurrence structurelle sur t que la taille de ajoute(x, t) est exactement 1 de plus que 
la taille de t.
  
  *)

  (*Question 23 : *)


  let rec extrait_gauche = function
  | E -> failwith "le tas est vide  "
  | N (n, E, E) -> (n, E)
  | N (n, t1, t2) -> let (e, t1') = extrait_gauche t1 in (e, N (n, t2, t1'))
  ;; 
  (* Testes  *)

let tasB = E in
let tasB = ajoute 3 tasB in
let tasB = ajoute 3 tasB in
let tasB = ajoute 10 tasB in
let tasB = ajoute 2 tasB in
let tasB = ajoute 3 tasB in
let tasB = ajoute 8 tasB in
let (gauche1, tasB1) = extrait_gauche tasB in
Printf.printf "gauche1 = %d\n" gauche1 ;
let (gauche2, tasB2) = extrait_gauche tasB1 in
let (gauche3, tasB3) = extrait_gauche tasB2 in
let (gauche4, tasB4) = extrait_gauche tasB3 in
let (gauche5, tasB5) = extrait_gauche tasB4 in
let (gauche6, _) = extrait_gauche tasB5 in
(gauche1, gauche2, gauche3, gauche4, gauche5, gauche6) ;;


(*Question 24 : 
Le problème ici , avec cette définition de la fonction fusion , on est pas sur que la propriété de l'arbre de Braun sera conservé apres la fusion .
Dans un arbre de Braun, la taille du sous arbre gauche doit etre égale ou supérieure à la taille du sous-arbre droit pour chaque nœud. 
Cependant, dans la définition donnée, il n'y a pas de vérification pour s'assurer que cette propriété est respectée après la fusion des deux tas.
En  fusionnant les deux tas de Braun sans vérifier cette propriété, il est possible d'obtenir un arbre qui ne respecte pas
la propriété d'un arbre de Braun.
  
*)

(*Question 25 : *)
 let rec fusion a b =
  match a, b with
  | E, _ -> b
  | _, E -> a
  | N (na, a1, a2), N (nb, b1, b2) ->
    if na <= nb then
      let aa = fusion a2 b in
      N (na, a1, aa)
    else
      let bb = fusion a b2 in
      N (nb, b1, bb) 

      ;; 

(* tests *)
let tasA = N (3, N (5, E, E), N (7, E, E))
let tasB = N (4, N (6, E, E), E)

let tas_fusionne = fusion tasB tasA;; (* Terminal OCaml on aura cette definition de tas_fusionne :   N (3, N (5, E, E), N (4, N (6, E, E), N (7, E, E))) *)
