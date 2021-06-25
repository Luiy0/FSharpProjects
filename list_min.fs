module list_min

 (*
 * list_min.fs
 * By Luis A. Flores (841102500)
 * 11/13/2015
 * This program determines the minimum element on a list of integer numbers
 *)

// Using regular recursion
let rec minReg = function
    | [] -> failwith "Empty list\n"
    | [hd] -> hd
    | hd::tl -> if hd < minReg tl then hd else minReg tl

// Using Tail recursion
let minTail lst =
    let rec helper min = function
    | [] -> min
    | hd::tl -> helper (if hd < min then hd else min) tl
    helper (List.head lst) lst

// Serves as the entry-point of the program.
let start lst =
    match lst with
    | [] -> failwith "Empty list.\n"
    | _ -> printfn "\nUsing the List.min function:\n\tThe minimum element is: %A" (List.min lst)
           printfn "Using the List.fold function:\n\tThe minimum element is: %A" (List.fold (fun val1 val2 -> if val1 < val2 then val1 else val2 ) (List.head lst) lst)
           printfn "Using a tail recursive helper function:\n\tThe minimum element is: %A" (minTail lst)
           printfn "Using a regular recursive function:\n\tThe minimum element is: %A\n" (minReg lst)

