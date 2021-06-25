module odd_count

(*
 * list_oddcount.fs
 * By Luis A. Flores (841102500)
 * 11/15/2015
 * This program determines the number of odd elements on a list.
 *)

 // Using regular recursion
let rec oddCount = function
    | [] -> 0
    | hd::tl -> if hd % 2 <> 0 then 1 + oddCount tl else oddCount tl

// Using Tail recursion
let oddCountTail lst =
    let rec helper accum1 = function
        | [] -> accum1
        | hd::tl -> helper (if hd % 2 <> 0 then accum1 + 1 else accum1) tl
    helper 0 lst

let start lst =
    printfn "\nUsing the List.filter and List.length functions:\n\tThe number of odd elements is: %A" (List.filter (fun elem -> elem % 2 <> 0) lst |> List.length)
    printfn "Using a tail-recursive helper function:\n\tThe number of odd elements is: %A" (oddCountTail lst)
    printfn "Using regular recursion:\n\tThe number of odd elements is: %A\n" (oddCount lst)