namespace FSharpQuiz

    open System
    open FSharpQuiz.SolitaireCipher

    module Main =
        
        [<EntryPoint>]
        [<STAThread()>]
        let main (args: String []) =
            Console.WriteLine("What should I encrypt and then decrypt for you?")
            let input = Console.ReadLine()
            encrypt_decrypt input
            Console.ReadLine() |> ignore
            100000