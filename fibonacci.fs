module fibonacci
(*
 * fibonacci.fs
 * By Luis A. Flores (841102500)
 * 11/11/2015
 * This program calculates the nth term of the Fibonacci sequence.
 *)

// Using if expression
let rec fibo1 nth =
     if nth < 0 then failwith "Negative Number\n"
     elif nth = 0 then 0
     elif nth = 1 then 1
     else fibo1 (nth - 1) + fibo1 (nth - 2)

// Using pattern matching expression
let rec fibo2 nth =
    match nth with
    | _ when nth < 0 -> failwith "Negative Number\n"
    | 0 -> 0
    | 1 -> 1
    | _ -> fibo2 (nth-1) + fibo2 (nth-2)

// Using a pattern matching function
let rec fibo3 = function
    | nth when nth < 0 -> failwith "Negative Number\n"
    | 0 -> 0
    | 1 -> 1
    | nth -> fibo3 (nth-1) + fibo3 (nth-2)

// Using tail recursion
let fibo4 nth =
    let rec helper accum1 accum2 = function
        | nth when nth < 0 -> failwith "Negative Number\n"
        | nth when nth = 0 -> accum1
        | nth -> helper accum2 (accum1 + accum2) (nth - 1)
    helper 0 1 nth

// Serves as the entry-point of the program.
let start term =
    printfn "\nUsing a tail recursive helper function:\n\tThe %d-th number in the Fibonacci sequence is %d" term (fibo4 term)
    printfn "Using a pattern matching function:\n\tThe %d-th number in the Fibonacci sequence is %d" term (fibo3 term)
    printfn "Using a pattern matching expression:\n\tThe %d-th number in the Fibonacci sequence is %d" term (fibo2 term)
    printfn "Using an if expression:\n\tThe %d-th number in the Fibonacci sequence is %d\n" term (fibo1 term)