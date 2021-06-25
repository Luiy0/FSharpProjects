(*
 * queue.fs
 * By Luis A. Flores (841-10-2500)
 * Source (implementation) file for a generic queue.
 * This file contains the private implementation of the queue as a generic list.
 *)

// Indicates the namespace that contains the Queue module.
namespace FunctionalCollections

// Defines the EmptyQueue exception.
exception EmptyQueue of string

// Declares the Queue module.
module Queue =
    // Declares the queue data type.
    type 'a queue = QueueList of 'a list * 'a list

    // Returns a new empty queue.
    let empty = QueueList ([], [])

    // Determines whether a queue is empty.
    let isEmpty = function
        | QueueList ([], []) -> true
        | _ -> false
    
    // Adds an element to the front of a queue.
    let add elem (QueueList (lst,lst2)) = QueueList (lst, elem::lst2)    

    // Removes the element at the front of a queue.
    // Raises EmptyQueue exception if needed.
    let rec remove = function
        | QueueList ([], []) -> raise (EmptyQueue "queue is empty")
        | QueueList (hd::tl, lst2) -> QueueList(tl,lst2)
        | QueueList([], lst2) -> remove (QueueList(List.rev lst2, []))         

    // Returns the element at the top of a queue.
    // Raises EmptyQueue exception if needed.
    let rec peek = function
        | QueueList ([], []) -> raise (EmptyQueue "queue is empty")
        | QueueList([], lst2) -> peek (QueueList(List.rev lst2, []))
        | QueueList(hd::_, _) -> hd    

    // Iterates through a queue using a visit function. 
    let rec iter visit = function
        | QueueList ([], []) -> ()
        | QueueList (lst, lst2) -> List.iter visit lst
                                   iter visit (QueueList(List.rev lst2, []))
    

