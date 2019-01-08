module Pieces
open Chess
/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  // king has runs of 1 in 8 directions: (N, NE, E, SE, S, SW, W, NW)
  override this.candiateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]

  override this.availableMoves (board : Board) : (Position list * chessPiece list) =
    let mutable invalidSquares = []
    let neighbouringPieces = snd (board.getVacantNNeighbours this)
    if this.color = White then 
      for r = 0 to 7 do
        for f = 0 to 7 do
          if board._array.[r,f].isSome && board._array.[r,f].Value.color = Black then
            let attackingSquares = fst (board.getVacantNNeighbours board._array.[r.f].Value)
            invalidSquares <- invalidSquares @ attackingSquares
    else
      for r = 0 to 7 do
        for f = 0 to 7 do
          if board._array.[r,f].isSome && board._array.[r,f].Value.color = White then
            let attackingSquares = fst (board.getVacantNNeighbours board._array.[r.f].Value)
            invalidSquares <- invalidSquares @ attackingSquares
    let naiveValidSquares = fst (board.getVacantNNeighbours this)
    let validSquares = List.filter
    
    (*let mutable validSquares : (Position list) = []
    let r = fst this.position.Value
    let f = snd this.position.Value
    let possiblePositions = [|Some (r-1, f-1); Some (r, f-1); Some (r+1, f-1); Some (r-1, f); Some (r+1, f); Some (r-1, f+1); Some (r, f+1); Some (r+1, f+1)|]
    let possibleKings = List.init 8 (fun x -> king (this.color))
    for i = 0 to 7 do
      possibleKings.[i].position <- possiblePositions.[i]
      if List.exists (fun cp -> cp.toString = 'K') (snd (board.getVacantNNeighbours possibleKings.[i])) then 
        ()
      else 
        validSquares <- possiblePositions.[i] :: validSquares
    let neighbouringPieces = snd (board.getVacantNNeighbours this)
    List.zip validSquares neighbouringPieces*)

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
  override this.candiateRelativeMoves =
    List.map (swap List.map [1..7]) indToRel (*//§\label{chessPieceSwapApp}§*)
  override this.nameOfType = "rook"
