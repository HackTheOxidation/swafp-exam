module MineSweeper

open System


[<AutoOpen>]
module Result =
    type ResultBuilder() =
        member _.Return(x) =
            Ok x

        member _.Bind(x, f) =
            Result.bind f x

    let result = ResultBuilder()


type Square =
    | Unturned of bool
    | Turned of bool * int

type Row = Square list

type Coordinate = int * int

type State = {turnedFields: int; score: bool}

type Board = Row list

let printBoard board: unit =
    let printRow row: unit =
        let printSquare s =
            match s with
                | Unturned _ ->
                    printf " "
                | Turned (isBomb, adjacentBombs) ->
                       if isBomb then
                           printf "*"
                       elif adjacentBombs > 0 then 
                           printf "%i" adjacentBombs
                       else
                           printf "."
        row 
        |> List.iter printSquare

    let len = board |> List.item 0 |> List.length
    let frame = String.init len (fun _ -> "-")

    printfn "%s" frame

    List.iter (fun r ->
               printRow r
               printfn "") board |> ignore

    printfn "%s" frame

let placeBombs board bombCoords =
    board
    |> List.mapi (fun i r ->
                  r |> List.mapi (fun j s ->
                                  if Set.contains (j, i) bombCoords
                                  then (Unturned true) else s))

let generateBombCoords bombs (x, y) coords: Set<Coordinate> =
    let allCoords =
        List.init x (fun v -> v - 1)
        |> List.allPairs (List.init y (fun v -> v - 1))

    let compute v =
        let random = Random()
        let shift = random.Next(0, x+y)
        let hash = random.Next(1, min x y)
        (v + shift) % hash

    allCoords
    |> List.sortBy (fun (a, b) -> (compute a, compute b))
    |> List.take bombs
    |> Set.ofList

let createBoardWith generator placer x y bombs =
    let isBetween value = value > 0 && value <= 50

    let nBombsValid = bombs > 0 && x * y - (2 * x * y / 3) > bombs

    if isBetween x && isBetween y && nBombsValid then
        let board = List.init y (fun _ -> List.init x (fun _ -> Unturned false))

        generator bombs (x, y) Set.empty 
        |> placer board
        |> Some
    else
        None

// By Using HOF, we can combine functions and test separately
let createBoard x y bombs: Board option =
    createBoardWith generateBombCoords placeBombs x y bombs

let turnSquare coord board: Result<Board, Board> =
    let x, y = coord

    let square = board |> List.item y |> List.item x

    let countAdjacentBombs board: int =
        let isAdjacent xn yn = abs (xn - x) <= 1 && abs (yn - y) <= 1

        let isBomb j i =
            let elem = board |> List.item i |> List.item j
            match elem with
                | Unturned true -> true
                | Turned (true, _) -> true
                | _ -> false

        let isAdjacentBomb j i =
            if isAdjacent j i && isBomb j i then
                1
            else
                0

        board
        |> List.mapi (fun i r -> r |> List.mapi (fun j s -> isAdjacentBomb j i))
        |> List.fold (fun acc r -> acc + List.fold (+) 0 r) 0

    match square with
        | Unturned isBomb ->
            let newRow =
                board
                |> List.item y
                |> List.updateAt x (Turned (isBomb, countAdjacentBombs board))
            let newBoard = List.updateAt y newRow board

            if isBomb then
                Error newBoard
            else
                Ok newBoard
        | _ -> Ok board

let promptUser (): Result<Coordinate, string> =
    let someInt str =
        try
            str |> int |> Ok 
        with
            | _ -> Error (sprintf "Invalid input: %s" str)
            
    printf "Enter a Coordinate >> "

    let fromUser = System.Console.ReadLine();
    let input = fromUser.Split(',')

    match input with
        | [| x; y |] ->
            result {
                let! w = someInt x
                let! h = someInt y
                return (w, h)
            }
            // Same as
            // someInt x
            // |> Result.bind (fun w -> someInt y |> Result.bind (fun h -> Ok (w, h))) 
        | _ -> Error "Invalid input"

let twoTrackResult fOk fError m =
    match m with
        | Ok value -> fOk value
        | Error value -> fError value

let rec gameLoop board: State =
    printBoard board

    let input = promptUser ()

    let hasWon b =
        let isTurned s =
            match s with
                | Turned (false, _) | Unturned true -> true
                | _ -> false
        b
        |> List.forall(fun r ->
                       List.forall (fun s -> isTurned s) r)

    let countTurned board =
        let isTurned s =
            match s with
                | Turned (false, _) -> 1
                | _ -> 0
        board
        |> List.map (fun r -> r |> List.map isTurned )
        |> List.collect id
        |> List.fold (+) 0 

    let checkInput coord board =
        let x, y = coord
        x >= 0 && y >= 0 && y <= (List.length board) && x <= (List.length <| List.item 0 board)

    let showAndContinue msg =
        printfn "%s" msg
        gameLoop board

    let winOrKeepPlaying board =
        if hasWon board then
            {turnedFields = countTurned board; score = true}
        else
            gameLoop board

    let lose board =
        printBoard board
        {turnedFields = countTurned board; score = false}

    let tryTurnSquare coord =
        if checkInput coord board then
            turnSquare coord board
            |> twoTrackResult winOrKeepPlaying lose
        else
            showAndContinue <| sprintf "Invalid coordinates: %A" coord

    input
    |> twoTrackResult tryTurnSquare showAndContinue

[<EntryPoint>]
let main args =
    let board = createBoard 10 10 10
    match board with
        | Some b ->
            let state = gameLoop b
            printfn "Total number of squares turned: %i" state.turnedFields
            if state.score then
                printfn "Yay! You won the game"
            else
                printfn "KABOOM! You lost, kiddo"
        | None -> failwith "Failed to create board"
    0
