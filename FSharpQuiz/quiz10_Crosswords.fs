namespace FSharpQuiz.Crosswords
    
    open System
    open FSharpQuiz.General

    [<AutoOpen>]
    module internal CrosswordsUtils =

        type Square = Letter | Filled with
                                        
            static member ofChar c = match c with 
                                         | 'X' -> Some Filled
                                         | '_' -> Some Letter
                                         |  _  -> None

        let row = Seq.toArray << Seq.choose id << Seq.map Square.ofChar

        let default_layout = "X _ _ _ _ X X
                              _ _ X _ _ _ _
                              _ _ _ _ X _ _
                              _ X _ _ X X X
                              _ _ _ X _ _ _
                              X _ _ _ _ _ X"

    type Crosswords () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = 10
            member __.description = "10) Crosswords"
            member __.start () = 
                Console.WriteLine("Using the default crossword layout.")
                let layout = default_layout
                
                let a = layout.Split('\n') |> Array.map row
                Console.WriteLine(sprintf "%A" a)