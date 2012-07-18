namespace FSharpQuiz.SolitaireCipher
    open System

    type internal Deck = int array

    [<AutoOpen>]
    module internal Helpers =
        let caps    = [|'A'..'Z'|]
        let cap_map = (caps, [|1..26|]) ||> Array.zip |> Map.ofArray

        let split l s = 
            let a = s |> Seq.toArray
            [| for i in 0..l..((a |> Array.length) - l) -> a.[i..i + l - 1] |]

        let number_to_letter i = caps.[i - 1]
        let letter_to_number l = cap_map |> Map.find l
        let ensure_range i = if i > 26 then i - 26 elif i < 0 then i + 26 else i

    [<AutoOpen>]
    module internal DeckHelpers =
        let jokerA = -1
        let jokerB = -2

        let move_joker (card: int) (deck: Deck) : Deck =
            let idx  =  deck |> Array.findIndex (fun c -> c = card)
            if (deck |> Array.length) - 1 = idx then Array.concat [ [|deck.[0] ; card|] ; deck.[1..idx - 1] ]
            else Array.concat [ deck.[0..idx - 1] ; [|deck.[idx + 1] ; card|] ; deck.[idx + 2..] ]
    
        let triple_cut (deck: Deck) : Deck =
            let jokers = [(=) jokerA ; (=) jokerB] |> List.map (fun f -> deck |> Array.findIndex f) 
            let top, bottom = jokers |> List.min, jokers |> List.max
            Array.concat [  deck.[bottom + 1..]
                            deck.[top..bottom]
                            deck.[0..top - 1] ]

        let count_cut (deck: Deck) : Deck =
            let last_idx = (deck |> Array.length) - 1
            let bottom = deck.[last_idx]
            if bottom > 0 then
                Array.concat [  deck.[bottom..last_idx - 1]
                                deck.[0..bottom - 1]
                                [|bottom|] ]
            else deck

        let output_letter (deck: Deck) : char option =
            let top = deck.[0]
            let convert card =  
                if card > 0 then card |> ensure_range |> number_to_letter |> Some
                else None

            if top > 0 then convert deck.[top]
            else convert deck.[deck.Length - 1]

        let deck: Deck = [1..52] @ [-1] @ [-2] |> List.toArray
    
        let cut = move_joker jokerA >> 
                           move_joker jokerB >> 
                           move_joker jokerB >> 
                           triple_cut >> 
                           count_cut

        let generate_keystream len : char array =
            let rec keystream deck letters len  =
                match len, cut deck with 
                    | 0, _     -> letters
                    | _, _deck -> match output_letter _deck with
                                      | Some l -> keystream _deck (l :: letters) (len - 1) 
                                      | None   -> keystream _deck letters len
            keystream deck [] len |> List.rev |> List.toArray
            
    [<AutoOpen>]
    module internal DeckHelpersTests =
        let pp setup expected = 
            (setup, expected) ||> Array.zip |> 
            Array.iter (fun (g,e) -> Console.WriteLine("{0} - {1}", g, e))  
    
        let ``check move jokers`` =
            let expectedA = [1..52] @ [-2] @ [-1] |> List.toArray
            let setup = deck |> move_joker jokerA
        
            if setup <> expectedA then failwith "move joker A is broken"

            let expectedB = [1] @ [-2] @ [2..52] @ [-1] |> List.toArray

            let setup = setup |> move_joker jokerB
                              |> move_joker jokerB

            if setup = expectedB then setup
            else pp setup expectedB; failwith "move joker B is broken"

        let ``check triple_cut`` =
            let expected = [-2] @ [2..52] @ [-1] @ [1] |> List.toArray
            let deck = ``check move jokers``
            let setup = deck |> triple_cut
            if setup = expected then setup
            else pp setup expected; failwith (sprintf "triple_cut is broken")
    
        let ``check count_cut`` =
            let expected = [2..52] @ [-1 ; -2] @ [1] |> List.toArray
            let setup = ``check triple_cut``
                            |> count_cut
            if setup = expected then setup
            else pp setup expected; failwith (sprintf "count_cut is broken")

        let ``check output_letter`` =
            let expected = Some 'D'
            let setup = ``check count_cut`` |> output_letter
            if setup = expected then setup
            else Console.WriteLine("{0} - {1}", setup, expected) 
                 failwith (sprintf "output_letter is broken")

        let ``check generate_keystream`` =
            let expected = 
                [|  'D' ; 'W' ; 'J' ; 'X' ; 'H' ; 'Y' ; 'R' ; 'F' ; 'D' ; 'G' |]
            let setup = expected |> Array.length |> generate_keystream 
            if setup = expected then setup
            else  pp setup expected; failwith (sprintf "generate_keystream is broken")

    [<AutoOpen>]
    module internal Sanitize = 
        open System.Text.RegularExpressions

        let format (s: string) = (new Regex("[^A-Z]+")).Replace(s.ToUpper(), "")
    
        let pad l s = 
            Seq.toArray <| match (s |> String.length) % l with
                               | x when x < 1 -> s 
                               | x -> [0..x] |> List.fold (fun acc _ -> acc + "X") s 

        let sanitize' i = format >> pad i

        let sanitize = sanitize' 5
        
    [<AutoOpen>]
    module internal SanitizeTests =
    
        let ``check sanitize`` = 
            let setup = "Code in F Sharp, live longer!"
            let expected = [| 3; 15; 4; 5; 9 ; 14; 6; 19; 8; 1 ; 18; 16; 12; 9; 22 ; 5; 12; 15; 14; 7 ; 5; 18; 24; 24; 24 |]
        
            if setup |> sanitize |> Array.map letter_to_number = expected then setup
            else failwith "sanitize is broken"

    type Cipher () = 

        interface FSharpQuiz.General.IQuiz with
            member __.id = "1"
            member __.description = "1) Solitaire Cipher"
            member __.start () = 
                    Console.WriteLine("What should I encrypt and then decrypt for you?")
                    let input = Console.ReadLine()
                    let encrypted = __.encrypt input
                    Console.WriteLine(sprintf "Encrypted result : %A" encrypted)
                    let decrypted = __.decrypt encrypted
                    Console.WriteLine(sprintf "Decrypted result : %A" decrypted)

        with member __.encrypt input =
                        Console.WriteLine(sprintf "Encrypting : %A" input)
                        let letters = input |> sanitize
                        Console.WriteLine(sprintf "1) sanitize input : %A" letters)
                        let keystream = (letters |> Seq.length) |> generate_keystream
                        Console.WriteLine(sprintf "2) keystream : %A" keystream)
                        let input_numbers = letters |> Array.map letter_to_number
                        Console.WriteLine(sprintf "3) input numbers : %A" input_numbers)
                        let keystream_numbers = keystream |> Array.map letter_to_number
                        Console.WriteLine(sprintf "4) keystream numbers : %A" keystream_numbers)
                        let added = (input_numbers, keystream_numbers) ||> Array.zip |> Array.map (fun (i,k) -> i + k |> ensure_range)
                        Console.WriteLine(sprintf "5) add keystream to input : %A" added)
                        let encrypted = added |> Array.map number_to_letter
                        Console.WriteLine(sprintf "6) back to letters : %A" encrypted)
                        encrypted

             member __.decrypt encrypted =
                        Console.WriteLine(sprintf "Decrypting : %A" encrypted)
                        let keystream = (encrypted |> Seq.length) |> generate_keystream 
                        Console.WriteLine(sprintf "1) keystream : %A" keystream)
                        let encrypted = encrypted |> Array.map letter_to_number 
                        Console.WriteLine(sprintf "2) encrypted numbers : %A" encrypted)
                        let keystream_numbers = keystream |> Array.map letter_to_number
                        Console.WriteLine(sprintf "3) keystream numbers : %A" keystream_numbers)
                        let subtracted = (encrypted, keystream_numbers) ||> Array.zip |> Array.map (fun (e,k) -> e - k |> ensure_range)
                        Console.WriteLine(sprintf "4) subtract keystream from encrypted : %A" subtracted)
                        let decrypted = subtracted |> Array.map number_to_letter 
                                                   |> Array.map (fun c -> c.ToString())
                                                   |> Array.reduce (+)
                        Console.WriteLine(sprintf "5) decrypted : %A" decrypted)
                        decrypted