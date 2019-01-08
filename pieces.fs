module Pieces
open Chess
/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  // king has runs of 1 in 8 directions: (N, NE, E, SE, S, SW, W, NW)
  override this.candidateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]

/// A king's available moves is limited, since it cannot move into a losing position.
/// The function works by calculating opponents possible moves and filtering the list so only valid moves are left.  
  override this.availableMoves (board : Board) : (Position list * chessPiece list) =
    let kingRank = fst this.position.Value
    let kingFile = snd this.position.Value
    let tmpKing = this :> chessPiece
    board.[kingRank,kingFile] <- None //Remove king from board to calculate proper rook moves.
    let mutable invalidSquares = []
    let neighbouringPieces : chessPiece list = snd (board.getVacantNNeighbours this)
    if this.color = White then 
      for r = 0 to 7 do
        for f = 0 to 7 do
          let square = board.[r,f]
          if Option.isSome square && square.Value.color = Black then
            let attackingSquares = fst (board.getVacantNNeighbours (square.Value))
            invalidSquares <- invalidSquares @ attackingSquares
    else
      for r = 0 to 7 do
        for f = 0 to 7 do
          let square = board.[r,f]
          if Option.isSome square && square.Value.color = White then
            let attackingSquares = fst (board.getVacantNNeighbours square.Value)
            invalidSquares <- invalidSquares @ attackingSquares
    let naiveValidSquares = fst (board.getVacantNNeighbours this)
    let isSquareInvalid pos = List.contains pos invalidSquares
    let validSquares : Position list = List.filter (fun pos -> not (isSquareInvalid pos)) naiveValidSquares
    board.[kingRank,kingFile] <- Some tmpKing //Put king back on the board
    (validSquares, neighbouringPieces)
    
/// A rook is a chessPiece which moves horisontally and vertically
type rook(col : Color) =
  inherit chessPiece(col)
  // rook can move horisontally and vertically
  // Make a list of relative coordinate lists. We consider the
  // current position and try all combinations of relative moves
  // (1,0); (2,0) ... (7,0); (-1,0); (-2,0); ...; (0,-7).
  // Some will be out of board, but will be assumed removed as
  // illegal moves.
  // A list of functions for relative moves
  let indToRel = [
    fun elm -> (elm,0); // South by elm
    fun elm -> (-elm,0); // North by elm
    fun elm -> (0,elm); // West by elm
    fun elm -> (0,-elm) // East by elm
    ]
  // For each function in indToRel, we calculate List.map f [1..7].
  // swap converts (List.map fct indices) to (List.map indices fct).
  let swap f a b = f b a
  override this.candidateRelativeMoves =
    List.map (swap List.map [1..7]) indToRel (*//§\label{chessPieceSwapApp}§*)
  override this.nameOfType = "rook"
