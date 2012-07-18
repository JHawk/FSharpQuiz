namespace FSharpQuiz
    
    open System

    /// <summary> I know this exists - I'm just having fun. </summary>
    [<AutoOpen>]
    module TexasChainsawTester =
        
        // TODO - make a fun FACT like attribute
        type CauseISaidSo () =
            inherit System.Attribute()
            member this.Text = ""
             
        let equal (actual: 'T) (expected: 'T) = 
            if actual <> expected then 
                failwith <| sprintf "FAILED : actual %A not equal to %A" actual expected

    module Seq = 
        let fib = Seq.unfold (fun (a,b) -> Some(a + b, (b, a + b))) (1,1)

    module List =
        /// <summary> Format a list of options into a nice human readable string. </summary>
        let join_or (l: 'x list): string =
            let last x y = x.ToString() + " or " + y.ToString()
            
            let rec join str = function 
                | []    -> str
                | [x]   -> last str x
                | x::xs -> join (str + ", " + x.ToString()) xs

            match l with 
                | []    -> ""
                | [x]   -> x.ToString()
                | [x;y] -> last x y
                | x::xs -> join (x.ToString()) xs

    [<AutoOpen>]
    module internal ListTests =
        let ``join_or : returns the first element if only one element`` = 
            let actual = List.join_or ["hi"]
            let expected = "hi"
            equal actual expected

        let ``join_or : returns an empty string if no elements`` = 
            let actual = List.join_or []
            let expected = ""
            equal actual expected
            
        let ``join_or : returns an or seperation for the last element`` = 
            let actual = List.join_or ["hi" ; "bye"]
            let expected = "hi or bye"
            equal actual expected

        let ``join_or : more than two elements`` = 
            let actual = List.join_or [1..5]
            let expected = "1, 2, 3, 4 or 5"
            equal actual expected
            
    module General =
        
        let rec fib n = if (n < 2) then n else fib(n - 1) + fib(n - 2)

        let _continue (msg: string) : bool = 
            let show_response b =
                Console.WriteLine(if b then "Yes" else "No")
                b

            Console.WriteLine(msg + " [Y/N]")
            let r = Console.ReadLine().ToUpper()
            show_response (r.Contains("Y") || r |> String.IsNullOrEmpty)

        type IQuiz =
            abstract member id: string
            abstract member description: string
            abstract member start: unit -> unit