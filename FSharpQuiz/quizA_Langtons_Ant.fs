namespace FSharpQuiz.LangtonsAnt

    open System
    open FSharpQuiz.General
    
    [<AutoOpen>]
    module internal LangtonsAntUtils =
        type Position = {x: int ; y: int}
            with member p.up    = {p with y = p.y + 1}
                 member p.right = {p with x = p.x + 1}
                 member p.down  = {p with y = p.y - 1}
                 member p.left  = {p with x = p.x - 1}

        type Direction = N | S | E | W
            with member this.right = match this with N -> E | E -> S | S -> W | W -> N           
                 member this.left  = match this with N -> W | E -> N | S -> E | W -> S           
                 member this.move (p: Position) = 
                            match this with N -> p.up | E -> p.right | S -> p.down | W -> p.left
                 static member All  = [| N ; S ; E ; W |]
                 static member Rand = Direction.All.[(new System.Random()).Next(0, Array.length Direction.All - 1)]

        let on_grid grid_size = function 
            | p when 0 > p.x || p.x > grid_size || 0 > p.y || p.y > grid_size -> false
            | _ -> true

        let render grid_size black_positions pos count = 
            [ for x in 0..grid_size do 
                for y in 0..grid_size do
                    let p = {x = x ; y = y}
                    yield if p = pos then "A"
                          elif black_positions |> Set.contains p then "#" 
                          else "."
                yield (count.ToString()) + " steps\n" ] |> List.reduce (+) |> System.Console.WriteLine

        let move grid_size watch =
            let on_grid' = on_grid grid_size
            let render' = render grid_size
            let black_positions = Set.empty 
            let pos = {x = grid_size/2 ; y = grid_size/2}

            let rec move' black_positions pos (dir: Direction) count =
                let black_positions', dir' =
                    if black_positions |> Set.contains pos then
                        Set.remove pos black_positions, dir.left
                    else 
                        Set.add pos black_positions, dir.right
                let pos' = dir'.move pos
                if not <| on_grid' pos' then render' black_positions' pos' count
                else 
                    if watch then render' black_positions' pos' count
                    move' black_positions' pos' dir' (count + 1L)
            move' black_positions pos Direction.Rand 0L

    type LangtonsAnt () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = "A"
            member __.description = "A) Langton's Ant"
            member __.start () = 
                Console.WriteLine("Enter the length of the sides of the grid.")
                Console.WriteLine("For best results 100.")
                let rec loop () = 
                    match Int32.TryParse <| Console.ReadLine() with
                        | true, i -> move i <| _continue ("Watch the ant?")
                        | _       -> Console.WriteLine("Come on! An int plz.")
                                     loop ()
                loop ()