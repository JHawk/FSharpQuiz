namespace FSharpQuiz

    open System
    open FSharpQuiz.SolitaireCipher

    module Main =
        
        [<EntryPoint>]
        [<STAThread()>]
        let main (args: String []) =
            solitaire_cipher "Code in Ruby, live longer!"
            Console.ReadLine() |> ignore
            100000