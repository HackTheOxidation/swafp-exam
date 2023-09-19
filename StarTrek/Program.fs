open System
open System.IO
open System.Threading

open Akka.FSharp
open Akka.Actor


type Planet =
    {name: string; hasLife: bool; travelTime: int}

let createPlanet planetName =
    let random = Random ();
    let travelTime = random.Next(1, 5)
    let hasLife = random.Next(2) = 0
    {name = planetName; hasLife = hasLife; travelTime = travelTime}

let explorePlanet (planet: Planet): bool =
    System.Threading.Thread.Sleep(planet.travelTime * 1000)
    planet.hasLife

type StarfleetCommandMessages =
    | SeekOutNewLifeAndNewCivilizations of Planet list
    | CreateShip of string
    | PlanetExplored of Planet

type OutputMessages =
    | Print of string

type CommandMessages =
    | Continue
    | Stop

type StarshipBaseMessages =
    | CreateShip of string

type StarshipMessages =
    | ExplorePlanet of Planet

let outputActor msg =
    match msg with
        | Print message ->
            printfn "Output received message: %s" message

let starshipActor starFleetCommand (mailbox: Actor<StarshipMessages>) msg =
    match msg with
        | ExplorePlanet planet ->
            explorePlanet planet |> ignore
            starFleetCommand <! StarfleetCommandMessages.PlanetExplored planet

let starshipBaseActor (mailbox: Actor<StarshipBaseMessages>) msg =
    match msg with
        | CreateShip shipName ->
            let trimmedShipName = String.filter (fun c -> c <> ' ') shipName
            spawn mailbox.Context trimmedShipName (actorOf2 (starshipActor mailbox.Context.Parent)) |> ignore

let starFleetCommandActor output (mailbox: Actor<StarfleetCommandMessages>) =
    // Prestart - Method 2
    let starshipBase = spawn mailbox.Context "starshipbase" (actorOf2 starshipBaseActor)

    let rec loop () =
        actor {
            let! msg = mailbox.Receive ()
            match msg with
                | SeekOutNewLifeAndNewCivilizations planets ->
                   let starships = mailbox.Context.ActorSelection("starshipbase/*")
                   planets
                   |> List.iter (fun planet ->
                                 starships.Tell(StarshipMessages.ExplorePlanet (planet))
                                 output <! OutputMessages.Print (sprintf "Departing for exploration of %s" planet.name)) 

                | StarfleetCommandMessages.CreateShip shipName ->
                   starshipBase <! StarshipBaseMessages.CreateShip shipName
                   output <! OutputMessages.Print (sprintf "Creating ship: %s" shipName)

                | PlanetExplored planet ->
                   match planet.hasLife with
                       | true -> output <! OutputMessages.Print (sprintf "Explored lively planet: %s" planet.name)
                       | false -> output <! OutputMessages.Print (sprintf "Explored desolate planet: %s" planet.name)

            return! loop()
            }
    loop ()

type CommandResult =
    | StopSystem
    | UnknownCommand

let commandActor starFleetCommand (mailbox: Actor<CommandMessages>) msg =
    let getCommand (): Result<StarfleetCommandMessages, CommandResult> =
        printfn "Available commands:\n [1] - Explore Planet: <planet-name>\n [2] - Create Ship: <ship-name>\n [0] - Exit"
        printf "Awaiting your orders commander :> "

        let input = Console.ReadLine()

        match input.Split(':') with
            | [| "0" |] ->
                printfn "Shuting down..."
                Error StopSystem
            | [| "1"; planetName |] ->
                Ok (StarfleetCommandMessages.SeekOutNewLifeAndNewCivilizations [createPlanet planetName])
            | [| "2"; shipName |] ->
                Ok (StarfleetCommandMessages.CreateShip shipName)
            | _ ->
                Error UnknownCommand

    match msg with
        | Continue ->
            match getCommand () with
                | Ok msg ->
                    starFleetCommand <! msg
                    mailbox.Self <! Continue
                | Error UnknownCommand ->
                    printfn "Unknown command"
                    mailbox.Self <! Continue
                | Error StopSystem ->
                    mailbox.Self <! Stop
        | Stop ->
            mailbox.Context.System.Terminate() |> ignore

[<EntryPoint>]
let main args =
    let starFleetSystem = System.create "Starfleet" (Configuration.load())

    let output = spawn starFleetSystem "output" (actorOf outputActor)

    let taela = spawn starFleetSystem "taela-shanthi" (starFleetCommandActor output)

    let command = spawn starFleetSystem "command" (actorOf2 (commandActor taela))

    command <! Continue

    starFleetSystem.WhenTerminated.Wait ()

    0
