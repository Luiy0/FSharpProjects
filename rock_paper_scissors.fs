(*
 * rock_paper_scissors.fs
 * By Luis A. Flores (841-10-2500)
 * Clasic Rock-Paper-Scissors game 
 *)

open System    // for .NET Console and Random
exception InvalidSelection of string

let rnd = Random () // For generating a random number

// Function that determines the winner, 
let winner sel gen =
    match (sel, gen) with
    | (0,1) -> Console.WriteLine("Your selection was 0=Rock Vs. 1=Paper -- You lost.")
    | (1,2) -> Console.WriteLine("Your selection was 1=Paper Vs. 2=Scissors -- You lost.")
    | (2,0) -> Console.WriteLine("Your selection was 2=Scissors Vs. 0=Rock -- You lost.")
    | (1,0) -> Console.WriteLine("Your selection was 1=Paper Vs. 0=Rock -- You won!")
    | (2,1) -> Console.WriteLine("Your selection was 2=Scissors Vs. 1=Paper -- You won!")
    | (0,2) -> Console.WriteLine("Your selection was 0=Rock Vs. 2=Scissors -- You won!")
    | (0,0) -> Console.WriteLine("Your selection was 0=Rock Vs. 0=Rock -- I'ts a tie.")  
    | (1,1) -> Console.WriteLine("Your selection was 1=Paper Vs. 1=Paper -- I'ts a tie.")
    | (2,2) -> Console.WriteLine("Your selection was 2=Scissors Vs. 2=Scissors -- I'ts a tie.")
    | (_,_) -> failwith ("Error!")   


let rec process_main acc =    
    Console.Write("Enter your selection (0=Rock, 1=Paper, 2=Scissors): ")
    let selection = Console.ReadLine() |> int

    let gen = rnd.Next(3) 
    match selection with
    | 0 | 1 | 2 -> winner selection gen    
    | _ -> Console.WriteLine("Invalid selection!\n")
           process_main acc
    
    Console.Write("\nDo you want to play again? (y/n): ")
    match Console.ReadLine() with
    | "y" -> process_main (acc + 1)           
    | "n" -> Console.WriteLine("Thank you!. {0}", acc)
             Console.ReadKey() |> ignore
             exit(0)
    | _ -> Console.WriteLine("Invalid answer! You are now forced to play again.\n")
           process_main (acc + 1)


let main () = 
    process_main 1   
                     
// Calls the main function.
main ()



