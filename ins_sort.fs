module ins_sort

(*
 * list_oddcount.fs
 * By Luis A. Flores (841102500)
 * 11/15/2015
 * This program inserts an element to a list and orders it ascendantly.
 *)

// Insert function
let rec insert value = function
    | [] -> [value]
    | hd::tl -> if value <= hd then value::hd::tl else hd::(insert value tl)

// Insertion sort (ascending)
let rec insertion_sort = function
    | [] -> []
    | hd::tl -> insert hd (insertion_sort tl) 