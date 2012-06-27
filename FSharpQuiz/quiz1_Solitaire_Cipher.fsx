
// The Solitaire Cipher (#1)
open System

type Deck = int array

[<AutoOpen>]
module Helpers =
    let internal caps    = [|'A'..'Z'|]
    let internal cap_map = (caps, [|1..26|]) ||> Array.zip |> Map.ofArray

    let split l s = 
        let a = s |> Seq.toArray
        [| for i in 0..l..((a |> Array.length) - l) -> a.[i..i + l - 1] |]
    let number_to_letter i = caps.[i - 1]
    let letter_to_number l = cap_map |> Map.find l

[<AutoOpen>]
module DeckHelpers =
    let internal jokerA = -1
    let internal jokerB = -2

    let internal move_joker (card: int) (deck: Deck) : Deck =
        let idx  =  deck |> Array.findIndex (fun c -> c = card)
        if (deck |> Array.length) - 1 = idx then Array.concat [ [|deck.[0] ; card|] ; deck.[1..idx - 1] ]
        else Array.concat [ deck.[0..idx - 1] ; [|deck.[idx + 1] ; card|] ; deck.[idx + 2..] ]
    
    let internal triple_cut (deck: Deck) : Deck =
        let jokers = [(=) jokerA ; (=) jokerB] |> List.map (fun f -> deck |> Array.findIndex f) 
        let top, bottom = jokers |> List.min, jokers |> List.max
        Array.concat [  deck.[bottom + 1..]
                        deck.[top..bottom]
                        deck.[0..top - 1] ]

    let internal count_cut (deck: Deck) : Deck =
        let last_idx = (deck |> Array.length) - 1
        let bottom = deck.[last_idx]
        if bottom > 0 then
            Array.concat [  deck.[bottom..last_idx - 1]
                            deck.[0..bottom - 1]
                            [|bottom|] ]
        else deck

    let internal output_letter (deck: Deck) : char option =
        let top = deck.[0]

        let convert card =  
            if card > 0 then 
                let card = if card > 26 then card - 26 else card
                Some <| number_to_letter card
            else None

        if top > 0 then convert deck.[top]
        else convert deck.[deck.Length - 1]

    let internal deck: Deck = [1..52] @ [-1] @ [-2] |> List.toArray
    
    let internal cut = move_joker jokerA >> move_joker jokerB >> move_joker jokerB >> triple_cut >> count_cut

    let generate_keystream len : char array =
        let rec keystream deck letters len  =
            match len, cut deck with 
                | 0, _     -> letters
                | _, _deck -> match output_letter _deck with
                                  | Some l -> keystream _deck (l :: letters) (len - 1) 
                                  | None   -> keystream _deck letters len
        keystream deck [] len |> List.toArray

    // TESTS 
    
    let internal pp setup expected = 
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

//    let ``check generate_keystream`` =
//        let expected = 
//            [|  'D' ; 'W' ; 'J' ; 'X' ; 'H' ; 'Y' ; 'R' ; 'F' ; 'D' ; 'G' |]
//        let setup = expected |> Array.length |> generate_keystream 
//
//        Console.WriteLine(setup)
//
//        if setup = expected then setup
//        else  pp setup expected; failwith (sprintf "generate_keystream is broken")

    
[<AutoOpen>]
module Sanitize = 
    open System.Text.RegularExpressions

    let internal format (s: string) = (new Regex("[^A-Z]+")).Replace(s.ToUpper(), "")
    
    let internal pad l s = 
        Seq.toArray <| match (s |> String.length) % l with
                           | x when x < 1 -> s 
                           | x -> [0..x] |> List.fold (fun acc _ -> acc + "X") s 

    let internal sanitize' i = format >> pad i

    let sanitize = sanitize' 5

    // TESTS
    
    let ``check sanitize`` = 
        let setup = "Code in F Sharp, live longer!"
        let expected = [| 3; 15; 4; 5; 9 ; 14; 6; 19; 8; 1 ; 18; 16; 12; 9; 22 ; 5; 12; 15; 14; 7 ; 5; 18; 24; 24; 24 |]
        
        if setup |> sanitize |> Array.map letter_to_number = expected then setup
        else failwith "sanitize is broken"


// Main 

let s = "Code in Ruby, live longer!"

let s' = s |> sanitize |> Array.map letter_to_number
let keystream = (s' |> Seq.length) * 5 |> generate_keystream |> Array.map letter_to_number |> split 5
        
Console.WriteLine(sprintf "keystream :: %A" keystream)
Console.WriteLine(sprintf "s :: %A" s)

let t = (keystream, s) ||> Seq.zip 
Console.WriteLine(sprintf "%A" t)
let t' = t |> Seq.map (fun t -> Array.map (fun (a,b) -> a + b))
Console.WriteLine()
Console.ReadLine() |> ignore
100000