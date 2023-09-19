module SevenWonders

open System
open FParsec

type Resource =
    | Wood
    | Brick
    | Stone
    | Ore

type Card =
    | Single of Resource
    | Double of Resource * Resource
    | Quad of Resource * Resource * Resource * Resource

let cardOneOf resource card =
    match card with
        | Single r -> resource = r
        | Double (r1, r2) -> resource = r1 || resource = r2
        | Quad (r1, r2, r3, r4) -> [r1; r2; r3; r4] |> List.contains resource

let isCardSingle card =
    match card with
        | Single _ -> true
        | _ -> false

let isCardDouble card =
    match card with
        | Double _ -> true
        | _ -> false

let isCardQuad card =
    match card with
        | Quad _ -> true
        | _ -> false

type Production = Map<Resource, int>

let prodFromList (res: Resource list): Production =
    res
    |> List.countBy id
    |> Map.ofList

let pCardInput =
    let pResource =
        anyOf "WSBOwsbo" |>> (fun c ->
                              match Char.ToUpper c with
                                  | 'W' -> Wood
                                  | 'S' -> Stone
                                  | 'B' -> Brick
                                  | 'O' -> Ore
                                  | _ -> failwith "Error: Invalid resource")
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let pSep = str_ws "/"
    let pSepRes = pResource .>> pSep       

    let pCard =
        (attempt (tuple4 pSepRes pSepRes pSepRes pResource |>> fun res -> Quad res))
         <|> (attempt (tuple2 pSepRes pResource |>> fun res -> Double res))
         <|> (attempt (pResource |>> fun res -> Single res)) .>> str_ws ","

    let pRequest =
        pstring "Can you make " >>. many pResource .>> pstring "?"

    let pCards =
        pstring "Cards [" >>. many pCard .>> pstring "]. "
    
    pCards .>>. pRequest

// I have never played '7 Wonders', so I have no idea if this game logic is right...
let rec solve cards products =
    let singles = List.filter isCardSingle cards
    let doubles = List.filter isCardDouble cards
    let quads = List.filter isCardQuad cards

    let countCards p n cards =
        n - (List.length <| List.filter (cardOneOf p) cards)
        |> max 0

    products
    |> Map.map (fun k v -> countCards k v singles)
    |> Map.map (fun k v -> countCards k v doubles)
    |> Map.map (fun k v -> countCards k v quads)
    |> Map.forall (fun k v -> v = 0)
            
[<EntryPoint>]
let main args =
    let input = Console.ReadLine();

    match run pCardInput input with
        | Success (result, _, _) ->
            printfn "Received input: %A" result
            let cards, prodList = result
            let products = prodFromList prodList
            match solve cards products with 
                | true -> printfn "Yes, this is feasable" 
                | false -> printfn "No, this is not possible"
        | Failure (errorMsg, _, _) -> printfn "Failure: %A" errorMsg
    0
