open System.Text.RegularExpressions
open Chess
open Pieces

[<AbstractClass>]
type Player(color : Color) =
    member this.color = color

    abstract member nextMove : string -> string

type Human(color : Color) =
    inherit Player(color)

    override this.nextMove(input : string) : string =
        let movePattern = "[a-h][1-8] [a-h][1-8]"
        let rec nM input =
            if Regex.IsMatch(input, movePattern) then
                input
            elif input = "quit" then
                exit 1
            else 
                printf "\ninvalid, try again: "
                let newInput = System.Console.ReadLine()
                nM newInput
        nM input
            
type Game(whiteP : Player, blackP : Player) =
    let board : Chess.Board = Chess.Board()

    do
        board.[0,0] <- Some (rook (White) :> chessPiece)
        board.[0,7] <- Some (rook (White) :> chessPiece)
        board.[0,4] <- Some (king (White) :> chessPiece)
        board.[7,0] <- Some (rook (Black) :> chessPiece)
        board.[7,7] <- Some (rook (Black) :> chessPiece)
        board.[7,4] <- Some (king (Black) :> chessPiece)    
    
    let moveToIndex (rankNFile : string) : Position =
        let rankIndex = (System.Char.GetNumericValue rankNFile.[1] |> int)-1
        match rankNFile.[0] with 
        | 'a' -> (rankIndex, 0)
        | 'b' -> (rankIndex, 1)
        | 'c' -> (rankIndex, 2)
        | 'd' -> (rankIndex, 3)
        | 'e' -> (rankIndex, 4)
        | 'f' -> (rankIndex, 5)
        | 'g' -> (rankIndex, 6)
        | 'h' -> (rankIndex, 7)
    
    let rec codeStringToMove(str : string) (player : Player) =
        let move = str.Split [|' '|]
        let fromRankNFile = moveToIndex move.[0]
        let toRankNFile = moveToIndex move.[1]
        printfn "fromRankNFile = %A     |       toRankNFile = %A" fromRankNFile toRankNFile 
        let fromSquare = board.[fst fromRankNFile, snd fromRankNFile]
        let toSquare = board.[fst toRankNFile, snd toRankNFile]
        if Option.isNone fromSquare then
            printf "\ninvalid move, try again: "
            let input = System.Console.ReadLine()
            codeStringToMove (player.nextMove input) player
        elif Option.isNone toSquare then
            let piece = fromSquare.Value
            if List.contains toRankNFile (fst (piece.availableMoves board)) && piece.color = player.color then
                board.move fromRankNFile toRankNFile
            else
                printf "\ninvalid move, try again: "
                let input = System.Console.ReadLine()
                codeStringToMove (player.nextMove input) player
        else
            let piece = fromSquare.Value
            let capturedPiece = toSquare.Value
            if (List.contains toRankNFile (fst (piece.availableMoves board)) || List.contains capturedPiece (snd (piece.availableMoves board))) && piece.color = player.color then
                board.move fromRankNFile toRankNFile
            else
                printf "\ninvalid move, try again: "
                let input = System.Console.ReadLine()
                codeStringToMove (player.nextMove input) player

    let rec turn(player : Player) =
        printf "%A player's move: " player.color
        let input = System.Console.ReadLine()
        codeStringToMove (player.nextMove input) player
        printfn "%A" board
        if player.color = White then
            turn blackP
        else 
            turn whiteP
    member this.run() =
        printfn "%A" board
        turn(whiteP)

let game = Game(Human(White), Human(Black))
game.run()