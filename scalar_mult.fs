module scalar_mult

(*
 * scalar_mult.fs
 * By Luis A. Flores (841102500)
 * 11/16/2015
 * This program determines the scalar product of an integer by a list of integers.
 *)

// Using regular recursion
let rec scalar scl = function
    | [] -> failwith "Empty List\n"
    | [hd] -> [hd*scl]
    | hd::tl -> (hd * scl)::(scalar scl tl)


// Using Tail recursion
let rec scalarTail scl lst =
    let rec helper scl lst acc =
        match lst with
        | [] -> failwith "Empty List\n"
        | [hd] -> scl*hd::acc
        | hd::tl -> helper scl tl ((scl*hd)::acc)
    helper scl (List.rev lst) []


let start scl lst =
    match lst with
    | [] -> failwith "Empty list.\n"
    | _ ->  printfn "\nUsing the List.map function:\n\tThe scalar multiplication is %A" (List.map (fun elem -> elem * scl) lst)
            printfn "Using a tail recursive helper function:\n\tThe scalar multiplication is %A" (scalarTail scl lst)
            printfn "Using a regular recursive function:\n\tThe scalar multiplication is %A\n" (scalar scl lst)

