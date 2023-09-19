module Tests

open System
open Xunit
open MineSweeper

[<Fact>]
let ``test createBoardWith 2x2 with dummies is 2x2`` () =
    let boardOpt = createBoardWith (fun _ _ _ -> Set.empty) (fun b _ -> b) 2 2 1

    Assert.True(Option.isSome boardOpt)

    match boardOpt with
        | Some board ->
            Assert.Equal(2, List.length board)
            Assert.True(List.forall (fun r -> List.length r = 2) board)
        | None -> Assert.Fail("Board is invalid")

[<Fact>]
let ``test createBoard 10x10 100 is too many bombs`` () =
    let board = createBoard 10 10 100
    Assert.True(Option.isNone board)

[<Fact>]
let ``test createBoard 0x0 10 has invalid size`` () =
    let board = createBoard 0 0 10
    Assert.True(Option.isNone board)

[<Fact>]
let ``test createBoard 10x10 0 have too few bombs`` () =
    let board = createBoard 10 10 0
    Assert.True(Option.isNone board)

[<Fact>]
let ``test placeBombs (1, 1) on board 2x2 is valid`` () =
    let coord = (1, 1)
    let board = [[Unturned false; Unturned false]; [Unturned false; Unturned false]]
    let expected = [[Unturned false; Unturned false]; [Unturned false; Unturned true]]

    let actual = placeBombs board [coord]
    
    // Because of C# interop
    let areEqual a b =
        a = b

    Assert.True(areEqual expected actual)

[<Fact>]
let ``test turnSquare (1, 1) for board 2x2 is valid`` () =
    let coord = (1, 1)
    let board = [[Unturned false; Unturned false]; [Unturned false; Unturned false]]
    let expected = [[Unturned false; Unturned false]; [Unturned false; Turned (false, 0)]]

    let newBoard = turnSquare coord board

    Assert.True(Result.isOk newBoard)

    // Because of C# interop
    let areEqual a b =
        a = b

    match newBoard with
        | Ok b -> Assert.True(areEqual expected b)
        | Error b -> Assert.True(areEqual expected b)
