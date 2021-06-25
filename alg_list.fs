(*
 * alg_list.fsi
 * Luis A. Flores *841-10-2500)
 * Signature file for a generic list.
 * This file contains the public interface of the list.
 *)

// Indicates the namespace that contains the AlgebraicList module.
namespace FunctionalCollections

// Defines the EmptyList exception.
exception EmptyList of string

// Declares the AlgebraicList module.
module AlgebraicList =
    // Declares the alg_list algebraic data type.
    type 'a alg_list = Empty | Cell of 'a * 'a alg_list

    // Returns a new empty list.
    let empty = Empty

    // Adds an element at the front of a list.
    let addHead elem lst = Cell(elem, lst) 

    // Removes the first ocurrence of an element from a list.
    // Raises EmptyList exception if needed.
    let rec remove elem = function
        | Empty -> raise (EmptyList "List is empty")
        | Cell (hd, tl) -> if elem = hd then tl else remove elem tl
        

    // Returns the first element of the given list.
    // Raises EmptyList exception if needed.
    let head = function
        | Empty -> raise (EmptyList "List is empty")
        | Cell (hd, _) -> hd

    // Returns the list that remains after removing the head of the given list.
    // Raises EmptyList exception if needed.
    let tail = function
        | Empty -> raise (EmptyList "List is empty")
        | Cell (_, tl) -> tl

    // Determines the number of elements in the list.
    let length lst =
        let rec helper acc = function
            | Empty -> acc
            | Cell (_, tl) -> helper (acc + 1) tl
        helper 0 lst

    // Determines whether a list is empty.
    let isEmpty = function
        | Empty -> true
        | _ -> false

    // Returns true if the element is a member of a list.
    let rec isMember elem = function
        | Empty -> raise (EmptyList "List is empty")
        | Cell (hd, tl) -> if elem = hd then true else isMember elem tl

    // Returns a new list with the elements of the given list in reverse order.
    let rev lst =
        let rec helper acc = function
            | Empty -> acc
            | Cell (hd, tl) -> helper(hd::acc) tl
        helper [] lst

    // Concatenates a list with another list.
    let rec append lst1 lst2 =
        match lst1 with
        | Empty -> lst2
        | Cell(hd, tl) -> hd::(append tl lst2) 

    // Iterates through a list using a visit function.
    let rec iter visit = function
        | Empty -> ()
        | Cell (hd, tl) -> visit hd
                           iter visit tl
         
    // Creates a list by calling a generator on each index.
    let init len gen =
        let rec helper acc len gen =
            match len with 
            | 0 -> acc
            | _ -> helper (gen (len - 1)::acc) (len - 1) gen
        helper [] len gen

    // Applies a function to each element of a list, accumulating a result
    // based on an initial value.
    let rec fold fn acc = function
        | Empty -> acc
        | Cell (hd, tl) -> fold fn (fn acc hd) tl

    // Applies a function to each element of a list, creating a new list.
    let rec map fn = function
        | Empty -> Empty
        | Cell (hd, tl) -> (fn hd)::(map fn tl)
       

    // Creates a new list with those elements of the original for which a 
    // predicate is satisfied.
    let rec filter pred = function
        | Empty -> Empty
        | Cell (hd, tl) -> if pred hd then hd::(filter pred tl) else filter pred tl

    // USAR ADDHEAD!!
