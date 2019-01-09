#load "chess.fs"
#load "pieces.fs"
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
            if Regex.IsMatch(input, movePattern) || input = "quit" then
                input
            else 
                printf "\nInvalid move, try again: "
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
    
    let rec codeStringToMove(str : string) =
        let move = str.Split [|' '|]
        let fromRankNFile = moveToIndex move.[0]
        let toRankNFile = moveToIndex move.[1]
        printfn "fromRankNFile = %A     |       toRankNFile = %A" fromRankNFile toRankNFile 
        (fromRankNFile, toRankNFile)

    let rec turn(player : Player) =
        printf "%A player's move: " player.color
        let input = System.Console.ReadLine()
        let move = codeStringToMove (player.nextMove input)
        let moveFrom = fst move
        let moveTo = snd move

        if Option.isNone board.[fst moveFrom, snd moveFrom] then 
            
        
        printfn "%A" board

    member this.whitePlayer = whiteP
    member this.blackPlayer = blackP

    member this.run() =
        printfn "%A" board
        turn(this.whitePlayer)


///testing purposes
let game = Game(Human(White), Human(Black))
game.run()