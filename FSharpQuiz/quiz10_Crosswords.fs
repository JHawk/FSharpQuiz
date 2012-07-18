namespace FSharpQuiz.Crosswords
    
    open System
    open FSharpQuiz.General

    [<AutoOpen>]
    module internal CrosswordsUtils =

        type Square = Letter | Filled with
            member __.display = function Filled -> '#' | _ -> ' '
            static member ofChar i c = match c with 
                                           | 'X' -> Some Filled
                                           | '_' -> Some Letter
                                           |  _  -> None

        let row = Seq.toArray << Seq.choose id << Seq.mapi Square.ofChar

        let default_layout = "X _ _ _ _ X X
                              _ _ X _ _ _ _
                              _ _ _ _ X _ _
                              _ X _ _ X X X
                              _ _ _ X _ _ _
                              X _ _ _ _ _ X"

    type Crosswords () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = "10"
            member __.description = "10) Crosswords"
            member __.start () = 
                Console.WriteLine("Using the default crossword layout.")
                let layout = default_layout
                
                let rows = layout.Split('\n') |> Array.map row
                Console.WriteLine(sprintf "%A" rows)