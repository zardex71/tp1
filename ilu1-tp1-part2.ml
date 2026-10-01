(* =====================================================================
   UPS - Lic.Info - ILU1.OCaml - TP1 - §2/2
   Fonctions, n-uplets, et typage fort !
   Learn OCaml — exercice ilu1-tp1-part2 — Note : 54 pts (tout terminé)
   ===================================================================== *)


(* ---------------------------------------------------------------------
   Exercice 1 (Stdlib + arith)

   - multiple_of : prend deux entiers n et d, détermine si n est un
     multiple de d. Renvoie un booléen. Sans récursion, avec ( mod ).
   - integer_square_root : racine carrée entière d'un entier positif n,
     i.e. le plus grand entier r tel que r * r <= n.

   Indication 1 : on peut utiliser les flottants, mais il faut convertir
   explicitement entre float et int (typage fort, pas de conversion
   implicite).
   Indication 2 : https://v2.ocaml.org/api/Stdlib.html
   --------------------------------------------------------------------- *)

let multiple_of n d =
  n mod d = 0

let integer_square_root n =
  let r = int_of_float (sqrt (float_of_int n)) in
  let r = if r * r > n then r - 1 else r in
  if (r + 1) * (r + 1) <= n then r + 1 else r


(* ---------------------------------------------------------------------
   Exercice 2 (Stdlib + string)

   - last_character : renvoie le dernier caractère d'une chaîne
     (on suppose, sans vérifier, qu'elle n'est pas vide).
   - string_of_bool : convertit un booléen en sa représentation string.
   --------------------------------------------------------------------- *)

let last_character s =
  s.[String.length s - 1]

let string_of_bool b =
  if b then "true" else "false"


(* ---------------------------------------------------------------------
   Exercice 3 (Quadruplets 2 à 2 distincts)

   pairwise_distinct : 'a * 'a * 'a * 'a -> bool
   renvoie true si les 4 composantes sont 2 à 2 distinctes.

   Question pour l'encadrant : cette fonction est-elle polymorphe ?
   Qu'est-ce que cela signifie ?
   --------------------------------------------------------------------- *)

let pairwise_distinct (a, b, c, d) =
  a <> b && a <> c && a <> d && b <> c && b <> d && c <> d


(* ---------------------------------------------------------------------
   Exercice 4 (Intermède : autour de la synthèse de type)

   Deviner le résultat de l'interprète, puis vérifier dans le Toplevel :

   # (1, false) ;;                         - : int * bool = (1, false)
   # (2., 1 + 4, 1 = 1, "salut") ;;
   # (('a', 1), ('b', 2), ('c', 3)) ;;
   # (1, 2, 3) ;;
   # (1, (2, 3)) ;;
   # ((1, 2), 3) ;;
   # (1, 2, 3) = (2, 1, 3) ;;
   # (1, (2, 3)) = (1, 2, 3) ;;
   # fun x f -> f x ;;
   --------------------------------------------------------------------- *)


(* ---------------------------------------------------------------------
   Exercice 5 (Plus dur : autour de la synthèse d'expression)

   Expressions ayant exactement le type :
     e1 : (int * bool) * int
     e2 : ('a -> 'a) * char

   Fonctions ayant exactement le type (sans annotation de type) :
     f1 : int -> bool -> (int * bool)
     f2 : 'a -> bool
     f3 : 'a -> 'a -> int
     f4 : 'a * 'b -> 'b * 'a
     f5 : ('a -> 'b) -> ('a -> 'b -> 'c) -> 'a -> 'c
     f6 : ('a -> 'b) * ('a * 'b -> 'c) * 'a -> 'c
   --------------------------------------------------------------------- *)

let e1 = ((1, true), 2)
let e2 = ((fun x -> x), 'a')

let f1 n b = (n + 1, not b)
let f2 x = (x = x)
let f3 x y = if x = y then 0 else 1
let f4 (a, b) = (b, a)
let f5 g h x = h x (g x)
let f6 (g, h, x) = h (x, g x)
