namespace FSharpQuiz

    open System
    open FSharpQuiz.SolitaireCipher
    open FSharpQuiz.SecretSantas
    open FSharpQuiz.Crosswords
    open FSharpQuiz.LangtonsAnt

    module Main =
        let quizzes : FSharpQuiz.General.IQuiz list = 
            [ Cipher () ; SecretSantas () ; Crosswords () ; LangtonsAnt () ]
        
        let print_menu () = 
            Console.WriteLine("***")
            Console.WriteLine("Choose your quiz.")
            quizzes |> List.iter (fun q -> Console.WriteLine(q.description)) 
            Console.WriteLine("Q) Quit")
            Console.WriteLine("***")
            
        [<EntryPoint>]
        [<STAThread()>]
        let main (args: String []) =

            let options = quizzes |> List.map (fun q -> (q.id.ToLower(),q.start))
            let format_options = 
                options |> List.map (fun (id,_) -> id.ToString()) |> List.join_or

            let rec loop () =
                let missing s = Console.WriteLine (sprintf "Unexpected entry : %A\nTry %s" s format_options) ; loop()
                print_menu()
                let s = Console.ReadLine().ToLower()
                match s, options |> List.tryFind (fun (id, _) -> id = s)  with
                    | _, Some (_, start) -> start() ; loop ()
                    | s, _ when s.Contains("q") -> Console.WriteLine("Quitting")
                    | _ -> missing s
                    
            loop ()
            100000