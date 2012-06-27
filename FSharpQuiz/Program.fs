namespace FSharpQuiz

    open System
    open FSharpQuiz.SolitaireCipher
    open FSharpQuiz.SecretSantas

    module Main =
        let quizzes : FSharpQuiz.General.IQuiz list = 
            [Cipher () ; SecretSantas ()]
        
        let print_menu () = 
            Console.WriteLine("***")
            Console.WriteLine("Choose your quiz.")
            quizzes |> List.iter (fun q -> Console.WriteLine(q.description)) 
            Console.WriteLine("Q) Quit")
            Console.WriteLine("***")
            
        [<EntryPoint>]
        [<STAThread()>]
        let main (args: String []) =

            let options =  quizzes |> List.map (fun q -> (q.id,q.start))

            let rec loop () =
                let missing s = Console.WriteLine (sprintf "Unexpected entry : %A" s) ; loop()
                print_menu()
                let s = Console.ReadLine()
                match Int32.TryParse s with
                    | true,  i -> match options |> List.tryFind (fun (id, _) -> id = i) with 
                                      | Some (_, start) -> start() ; loop ()
                                      | _               -> missing(i.ToString())
                    | false, _ when s.ToUpper().Contains("Q") -> Console.WriteLine("Quitting")
                    | _,     _ -> missing s
            loop ()
            100000