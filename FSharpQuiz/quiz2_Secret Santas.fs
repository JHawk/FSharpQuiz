namespace FSharpQuiz.SecretSantas
    
    open System
    open FSharpQuiz.General

    [<AutoOpen>]
    module internal SecretSantaUtils =
    
        type FirstName       = string
        type LastName        = string
        type Email           = Email of string
        type SecretSanta     = (FirstName * LastName * Email)
        
        let format_name ((first,last,_): SecretSanta) = first + " " + last

        type SantaAssignment = Assignment of SecretSanta * SecretSanta
            with member this.giver     = match this with Assignment (giver, _) -> giver
                 member this.recipient = match this with Assignment (_, recipient) -> recipient
                 member this.body = 
                            let _giver     = format_name this.giver
                            let _recipient = format_name this.recipient
                            sprintf "Congrats & Merry Christmas %s, you're buying something for %s. Good luck secret Santa..." _giver _recipient
         
        let example_santas =  [ ("Luke"   , "Skywalker" , Email("<luke@theforce.net>"          ))
                                ("Leia"   , "Skywalker" , Email("<leia@therebellion.org>"      ))
                                ("Danny"  , "Skywalker" , Email("<danny@therebellion.org>"     ))
                                ("Toula"  , "Portokalos", Email("<toula@manhunter.org>"        ))
                                ("Gus"    , "Portokalos", Email("<gus@weareallfruit.net>"      ))
                                ("Bruce"  , "Wayne"     , Email("<bruce@imbatman.com>"         ))
                                ("Virgil" , "Brigman"   , Email("<virgil@rigworkersunion.org>" ))
                                ("Lindsey", "Brigman"   , Email("<lindsey@iseealiens.net>"     )) ]
        
        let collect_santas (): SecretSanta list =
            Console.WriteLine("Enter some santas then.")

            let rec collect_santa santas: SecretSanta list =
                Console.WriteLine("First Name?")
                let first = Console.ReadLine()
                Console.WriteLine("Last Name?")
                let last = Console.ReadLine()
                Console.WriteLine("Email?")
                let email = Console.ReadLine()
                let santas = (first, last, Email email) :: santas
                if _continue ("Done?") then santas
                else collect_santa <| santas

            collect_santa []

        let pp (sas: SantaAssignment list) = 
            sas |> List.iter (fun sa -> Console.WriteLine sa.body)

        let email santas_assignments = santas_assignments |> List.iter (fun sa -> failwith "not implemented")

        let too_few santas = Console.WriteLine (sprintf "too few Santas - only %A given - try again" <| List.length santas)

        let assign_santas (santas: SecretSanta list) (after_assignment: SantaAssignment list -> unit): unit = 
            let santas = santas |> Seq.distinct |> Seq.toList

            let santas = santas |> Seq.groupBy (fun (_,l,_) -> l) 
                                |> Seq.sortBy (fun (_,c) -> - Seq.length c)
            Console.WriteLine(sprintf "asdf : %A" <| Seq.toArray santas)
//
//            match santas with 
//                | []  -> too_few santas
//                | [_] -> too_few santas
//                | _   -> after_assignment []

    type SecretSantas () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = 2
            member __.description = "2) Secret Santas"
            member __.start () = 
                        let dry_run = _continue ("Dry Run? (Doesn't actually email anyone.)") 
                        let santas = if not dry_run then collect_santas()
                                     elif _continue("Use example set?") then example_santas
                                     else collect_santas()
                        assign_santas santas (if dry_run then pp else email)