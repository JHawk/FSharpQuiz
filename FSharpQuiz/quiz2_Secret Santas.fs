namespace FSharpQuiz.SecretSantas

    module SecretSantas' = 
        let choose_santas () = "santas" ; ()
        
    type SecretSantas () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = 2
            member __.description = "2) Secret Santas"
            member __.start () = SecretSantas'.choose_santas ()

