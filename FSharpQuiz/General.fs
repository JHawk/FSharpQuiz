namespace FSharpQuiz

    module General =
     
        type IQuiz =
            abstract member id: int
            abstract member description: string
            abstract member start: unit -> unit