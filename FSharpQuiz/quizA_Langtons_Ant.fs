namespace FSharpQuiz.LangtonsAnt

    open System
    
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

        let render grid_size black_positions' pos' = 
            [ for x in 0..grid_size do 
                for y in 0..grid_size do
                    let p = {x = x ; y = y}
                    yield if p = pos' then "A"
                          elif black_positions' |> Set.contains p then "#" 
                          else "."
                yield "\n" ] |> List.reduce (+) |> System.Console.WriteLine

        let move grid_size black_positions pos (dir: Direction) =
            let on_grid' = on_grid grid_size
            let render' = render grid_size

            let rec move' black_positions pos (dir: Direction) =
                let black_positions', dir' =
                    if black_positions |> Set.contains pos then
                        Set.remove pos black_positions, dir.left
                    else 
                        Set.add pos black_positions, dir.right
                let pos' = dir'.move pos
                if not <| on_grid' pos' then render' black_positions' pos'
                else move' black_positions' pos' dir'
            move' black_positions pos dir

        let start grid_size = move grid_size Set.empty {x = grid_size/2 ; y = grid_size/2} Direction.Rand

    type LangtonsAnt () = 
        interface FSharpQuiz.General.IQuiz with
            member __.id = "A"
            member __.description = "A) Langton's Ant"
            member __.start () = 
                Console.WriteLine("Enter the length of the sides of the grid.")
                let rec loop () = 
                    match Int32.TryParse <| Console.ReadLine() with
                        | true, i -> start i
                        | _       -> Console.WriteLine("Come on! An int plz.")
                                     loop ()
                loop ()