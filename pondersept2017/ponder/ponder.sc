object ponder {
  case class Piece(val piece: Char, val colour: Char)
  case class Board(val pieces: Map[(Int, Int), Piece])

  val blackRook = Piece('R', 'b')                 //> blackRook  : ponder.Piece = Piece(R,b)

  // All remaining white pieces are placed randomly on the first rank, with two restrictions:
  val allStartingPositions = List('R', 'R', 'N', 'N', 'B', 'B', 'K', 'Q').permutations.toList
    // The bishops must be placed on opposite-color squares. (Their indices must sum to an odd number.)
    .filter { board =>
      board.zipWithIndex
        .filter { case (piece, pos) => piece == 'B' }
        .map { case (piece, pos) => pos }.sum % 2 != 0
    }
    // The king must be placed on a square between the rooks.
    .filter { board => board.filter { piece => piece == 'K' || piece == 'R' } == List('R', 'K', 'R') }
                                                  //> allStartingPositions  : List[List[Char]] = List(List(R, N, N, B, B, K, R, Q)
                                                  //| , List(R, N, N, B, B, K, Q, R), List(R, N, N, B, B, Q, K, R), List(R, N, N, 
                                                  //| B, K, R, B, Q), List(R, N, N, B, K, Q, B, R), List(R, N, N, B, Q, K, B, R), 
                                                  //| List(R, N, N, K, R, B, B, Q), List(R, N, N, K, R, Q, B, B), List(R, N, N, K,
                                                  //|  B, R, Q, B), List(R, N, N, K, B, B, R, Q), List(R, N, N, K, B, B, Q, R), Li
                                                  //| st(R, N, N, K, B, Q, R, B), List(R, N, N, K, Q, R, B, B), List(R, N, N, K, Q
                                                  //| , B, B, R), List(R, N, N, Q, B, B, K, R), List(R, N, N, Q, B, K, R, B), List
                                                  //| (R, N, N, Q, K, R, B, B), List(R, N, N, Q, K, B, B, R), List(R, N, B, N, K, 
                                                  //| R, Q, B), List(R, N, B, N, K, B, R, Q), List(R, N, B, N, K, B, Q, R), List(R
                                                  //| , N, B, N, K, Q, R, B), List(R, N, B, N, Q, B, K, R), List(R, N, B, N, Q, K,
                                                  //|  R, B), List(R, N, B, B, N, K, R, Q), List(R, N, B, B, N, K, Q, R), List(R, 
                                                  //| N, B, B, N, Q, K, R), List(R, N, B, B, K, R, N, Q), List(R, N, B, B, K, R, Q
                                                  //| , N), List(R, N, B, B, K, N, R, Q), List(R, N, B, B, K, N, Q, R), List(R, N,
                                                  //|  B, B, K, Q, R, N), List(R, N, B, B, K, Q, N, R), List(R, N, B, B, Q, N, K, 
                                                  //| R), List(R, N, B, B, Q, K, R, N), List(R, N, B, B, Q, K, N, R), List(R, N, B
                                                  //| , K, R, N, Q, B), List(R, N, B, K, R, B, N, Q), List(R, N, B, K, R, B, Q, N)
                                                  //| , List(R, N, B, K, R, Q, N, B), List(R, N, B, K, N, R, Q, B), List(R, N, B, 
                                                  //| K, N, B, R, Q), List(R, N, B, K, N, B, Q, R), List(R, N, B, K, N, Q, R, B), 
                                                  //| List(R, N, B, K, Q, R, N, B), List(R, N, B, K, Q, N, R, B), List(R, N, B, K,
                                                  //|  Q, B, R, N), List(R, N, B, K, Q, B, N, R), List(R, N, B, Q, N, B, K, R), Li
                                                  //| st(R, N, B, Q, N, K, R, B), List(R, N, B, Q, K, R, N, B), List(R, N, B, Q, K
                                                  //| , N, R, B), List(R, N, B, Q, K, B, R, N), List(R, N, B, Q, K, B, N, R), List
                                                  //| (R, N, K, R, N, B, B, Q), List(R, N, K, R, N, Q, B, B), List(R, N, K, R, B, 
                                                  //| N, Q, B), List(R, N, K, R, B, B, N, Q), List(R, N, K, R, B, B, Q, N), List(R
                                                  //| , N, K, R, B, Q, N, B), List(R, N, K, R, Q, N, B, B), List(R, N, K, R, Q, B,
                                                  //|  B, N), List(R, N, K, N, R, B, B, Q), List(R, N, K, N, R, Q, B, B), List(R, 
                                                  //| N, K, N, B, R, Q, B), List(R, N, K, N, B, B, R, Q), List(R, N, K, N, B, B, Q
                                                  //| , R), List(R, N, K, N, B, Q, R, B), List(R, N, K, N, Q, R, B, B), List(R, N,
                                                  //|  K, N, Q, B, B, R), List(R, N, K, B, R, N, B, Q), List(R, N, K, B, R, Q, B, 
                                                  //| N), List(R, N, K, B, N, R, B, Q), List(R, N, K, B, N, Q, B, R), List(R, N, K
                                                  //| , B, B, R, N, Q), List(R, N, K, B, B, R, Q, N), List(R, N, K, B, B, N, R, Q)
                                                  //| , List(R, N, K, B, B, N, Q, R), List(R, N, K, B, B, Q, R, N), List(R, N, K, 
                                                  //| B, B, Q, N, R), List(R, N, K, B, Q, R, B, N), List(R, N, K, B, Q, N, B, R), 
                                                  //| List(R, N, K, Q, R, N, B, B), List(R, N, K, Q, R, B, B, N), List(R, N, K, Q,
                                                  //|  N, R, B, B), List(R, N, K, Q, N, B, B, R), List(R, N, K, Q, B, R, N, B), Li
                                                  //| st(R, N, K, Q, B, N, R, B), List(R, N, K, Q, B, B, R, N), List(R, N, K, Q, B
                                                  //| , B, N, R), List(R, N, Q, N, B, B, K, R), List(R, N, Q, N, B, K, R, B), List
                                                  //| (R, N, Q, N, K, R, B, B), List(R, N, Q, N, K, B, B, R), List(R, N, Q, B, N, 
                                                  //| K, B, R), List(R, N, Q, B, B, N, K, R), List(R, N, Q, B, B, K, R, N), List(R
                                                  //| , N, Q, B, B, K, N, R), List(R, N, Q, B, K, R, B, N), List(R, N, Q, B, K, N,
                                                  //|  B, R), List(R, N, Q, K, R, N, B, B), List(R, N, Q, K, R, B, B, N), List(R, 
                                                  //| N, Q, K, N, R, B, B), List(R, N, Q, K, N, B, B, R), List(R, N, Q, K, B, R, N
                                                  //| , B), List(R, N, Q, K, B, N, R, B), List(R, N, Q, K, B, B, R, N), List(R, N,
                                                  //|  Q, K, B, B, N, R), List(R, B, N, N, B, K, R, Q), List(R, B, N, N, B, K, Q, 
                                                  //| R), List(R, B, N, N, B, Q, K, R), List(R, B, N, N, K, R, B, Q), List(R, B, N
                                                  //| , N, K, Q, B, R), List(R, B, N, N, Q, K, B, R), List(R, B, N, K, R, N, B, Q)
                                                  //| , List(R, B, N, K, R, Q, B, N), List(R, B, N, K, N, R, B, Q), List(R, B, N, 
                                                  //| K, N, Q, B, R), List(R, B, N, K, B, R, N, Q), List(R, B, N, K, B, R, Q, N), 
                                                  //| List(R, B, N, K, B, N, R, Q), List(R, B, N, K, B, N, Q, R), List(R, B, N, K,
                                                  //|  B, Q, R, N), List(R, B, N, K, B, Q, N, R), List(R, B, N, K, Q, R, B, N), Li
                                                  //| st(R, B, N, K, Q, N, B, R), List(R, B, N, Q, N, K, B, R), List(R, B, N, Q, B
                                                  //| , N, K, R), List(R, B, N, Q, B, K, R, N), List(R, B, N, Q, B, K, N, R), List
                                                  //| (R, B, N, Q, K, R, B, N), List(R, B, N, Q, K, N, B, R), List(R, B, B, N, N, 
                                                  //| K, R, Q), List(R, B, B, N, N, K, Q, R), List(R, B, B, N, N, Q, K, R), List(R
                                                  //| , B, B, N, K, R, N, Q), List(R, B, B, N, K, R, Q, N), List(R, B, B, N, K, N,
                                                  //|  R, Q), List(R, B, B, N, K, N, Q, R), List(R, B, B, N, K, Q, R, N), List(R, 
                                                  //| B, B, N, K, Q, N, R), List(R, B, B, N, Q, N, K, R), List(R, B, B, N, Q, K, R
                                                  //| , N), List(R, B, B, N, Q, K, N, R), List(R, B, B, K, R, N, N, Q), List(R, B,
                                                  //|  B, K, R, N, Q, N), List(R, B, B, K, R, Q, N, N), List(R, B, B, K, N, R, N, 
                                                  //| Q), List(R, B, B, K, N, R, Q, N), List(R, B, B, K, N, N, R, Q), List(R, B, B
                                                  //| , K, N, N, Q, R), List(R, B, B, K, N, Q, R, N), List(R, B, B, K, N, Q, N, R)
                                                  //| , List(R, B, B, K, Q, R, N, N), List(R, B, B, K, Q, N, R, N), List(R, B, B, 
                                                  //| K, Q, N, N, R), List(R, B, B, Q, N, N, K, R), List(R, B, B, Q, N, K, R, N), 
                                                  //| List(R, B, B, Q, N, K, N, R), List(R, B, B, Q, K, R, N, N), List(R, B, B, Q,
                                                  //|  K, N, R, N), List(R, B, B, Q, K, N, N, R), List(R, B, K, R, N, N, B, Q), Li
                                                  //| st(R, B, K, R, N, Q, B, N), List(R, B, K, R, B, N, N, Q), List(R, B, K, R, B
                                                  //| , N, Q, N), List(R, B, K, R, B, Q, N, N), List(R, B, K, R, Q, N, B, N), List
                                                  //| (R, B, K, N, R, N, B, Q), List(R, B, K, N, R, Q, B, N), List(R, B, K, N, N, 
                                                  //| R, B, Q), List(R, B, K, N, N, Q, B, R), List(R, B, K, N, B, R, N, Q), List(R
                                                  //| , B, K, N, B, R, Q, N), List(R, B, K, N, B, N, R, Q), List(R, B, K, N, B, N,
                                                  //|  Q, R), List(R, B, K, N, B, Q, R, N), List(R, B, K, N, B, Q, N, R), List(R, 
                                                  //| B, K, N, Q, R, B, N), List(R, B, K, N, Q, N, B, R), List(R, B, K, Q, R, N, B
                                                  //| , N), List(R, B, K, Q, N, R, B, N), List(R, B, K, Q, N, N, B, R), List(R, B,
                                                  //|  K, Q, B, R, N, N), List(R, B, K, Q, B, N, R, N), List(R, B, K, Q, B, N, N, 
                                                  //| R), List(R, B, Q, N, N, K, B, R), List(R, B, Q, N, B, N, K, R), List(R, B, Q
                                                  //| , N, B, K, R, N), List(R, B, Q, N, B, K, N, R), List(R, B, Q, N, K, R, B, N)
                                                  //| , List(R, B, Q, N, K, N, B, R), List(R, B, Q, K, R, N, B, N), List(R, B, Q, 
                                                  //| K, N, R, B, N), List(R, B, Q, K, N, N, B, R), List(R, B, Q, K, B, R, N, N), 
                                                  //| List(R, B, Q, K, B, N, R, N), List(R, B, Q, K, B, N, N, R), List(R, K, R, N,
                                                  //|  N, B, B, Q), List(R, K, R, N, N, Q, B, B), List(R, K, R, N, B, N, Q, B), Li
                                                  //| st(R, K, R, N, B, B, N, Q), List(R, K, R, N, B, B, Q, N), List(R, K, R, N, B
                                                  //| , Q, N, B), List(R, K, R, N, Q, N, B, B), List(R, K, R, N, Q, B, B, N), List
                                                  //| (R, K, R, B, N, N, B, Q), List(R, K, R, B, N, Q, B, N), List(R, K, R, B, B, 
                                                  //| N, N, Q), List(R, K, R, B, B, N, Q, N), List(R, K, R, B, B, Q, N, N), List(R
                                                  //| , K, R, B, Q, N, B, N), List(R, K, R, Q, N, N, B, B), List(R, K, R, Q, N, B,
                                                  //|  B, N), List(R, K, R, Q, B, N, N, B), List(R, K, R, Q, B, B, N, N), List(R, 
                                                  //| K, N, R, N, B, B, Q), List(R, K, N, R, N, Q, B, B), List(R, K, N, R, B, N, Q
                                                  //| , B), List(R, K, N, R, B, B, N, Q), List(R, K, N, R, B, B, Q, N), List(R, K,
                                                  //|  N, R, B, Q, N, B), List(R, K, N, R, Q, N, B, B), List(R, K, N, R, Q, B, B, 
                                                  //| N), List(R, K, N, N, R, B, B, Q), List(R, K, N, N, R, Q, B, B), List(R, K, N
                                                  //| , N, B, R, Q, B), List(R, K, N, N, B, B, R, Q), List(R, K, N, N, B, B, Q, R)
                                                  //| , List(R, K, N, N, B, Q, R, B), List(R, K, N, N, Q, R, B, B), List(R, K, N, 
                                                  //| N, Q, B, B, R), List(R, K, N, B, R, N, B, Q), List(R, K, N, B, R, Q, B, N), 
                                                  //| List(R, K, N, B, N, R, B, Q), List(R, K, N, B, N, Q, B, R), List(R, K, N, B,
                                                  //|  B, R, N, Q), List(R, K, N, B, B, R, Q, N), List(R, K, N, B, B, N, R, Q), Li
                                                  //| st(R, K, N, B, B, N, Q, R), List(R, K, N, B, B, Q, R, N), List(R, K, N, B, B
                                                  //| , Q, N, R), List(R, K, N, B, Q, R, B, N), List(R, K, N, B, Q, N, B, R), List
                                                  //| (R, K, N, Q, R, N, B, B), List(R, K, N, Q, R, B, B, N), List(R, K, N, Q, N, 
                                                  //| R, B, B), List(R, K, N, Q, N, B, B, R), List(R, K, N, Q, B, R, N, B), List(R
                                                  //| , K, N, Q, B, N, R, B), List(R, K, N, Q, B, B, R, N), List(R, K, N, Q, B, B,
                                                  //|  N, R), List(R, K, B, R, N, N, Q, B), List(R, K, B, R, N, B, N, Q), List(R, 
                                                  //| K, B, R, N, B, Q, N), List(R, K, B, R, N, Q, N, B), List(R, K, B, R, Q, N, N
                                                  //| , B), List(R, K, B, R, Q, B, N, N), List(R, K, B, N, R, N, Q, B), List(R, K,
                                                  //|  B, N, R, B, N, Q), List(R, K, B, N, R, B, Q, N), List(R, K, B, N, R, Q, N, 
                                                  //| B), List(R, K, B, N, N, R, Q, B), List(R, K, B, N, N, B, R, Q), List(R, K, B
                                                  //| , N, N, B, Q, R), List(R, K, B, N, N, Q, R, B), List(R, K, B, N, Q, R, N, B)
                                                  //| , List(R, K, B, N, Q, N, R, B), List(R, K, B, N, Q, B, R, N), List(R, K, B, 
                                                  //| N, Q, B, N, R), List(R, K, B, B, R, N, N, Q), List(R, K, B, B, R, N, Q, N), 
                                                  //| List(R, K, B, B, R, Q, N, N), List(R, K, B, B, N, R, N, Q), List(R, K, B, B,
                                                  //|  N, R, Q, N), List(R, K, B, B, N, N, R, Q), List(R, K, B, B, N, N, Q, R), Li
                                                  //| st(R, K, B, B, N, Q, R, N), List(R, K, B, B, N, Q, N, R), List(R, K, B, B, Q
                                                  //| , R, N, N), List(R, K, B, B, Q, N, R, N), List(R, K, B, B, Q, N, N, R), List
                                                  //| (R, K, B, Q, R, N, N, B), List(R, K, B, Q, R, B, N, N), List(R, K, B, Q, N, 
                                                  //| R, N, B), List(R, K, B, Q, N, N, R, B), List(R, K, B, Q, N, B, R, N), List(R
                                                  //| , K, B, Q, N, B, N, R), List(R, K, Q, R, N, N, B, B), List(R, K, Q, R, N, B,
                                                  //|  B, N), List(R, K, Q, R, B, N, N, B), List(R, K, Q, R, B, B, N, N), List(R, 
                                                  //| K, Q, N, R, N, B, B), List(R, K, Q, N, R, B, B, N), List(R, K, Q, N, N, R, B
                                                  //| , B), List(R, K, Q, N, N, B, B, R), List(R, K, Q, N, B, R, N, B), List(R, K,
                                                  //|  Q, N, B, N, R, B), List(R, K, Q, N, B, B, R, N), List(R, K, Q, N, B, B, N, 
                                                  //| R), List(R, K, Q, B, R, N, B, N), List(R, K, Q, B, N, R, B, N), List(R, K, Q
                                                  //| , B, N, N, B, R), List(R, K, Q, B, B, R, N, N), List(R, K, Q, B, B, N, R, N)
                                                  //| , List(R, K, Q, B, B, N, N, R), List(R, Q, N, N, B, B, K, R), List(R, Q, N, 
                                                  //| N, B, K, R, B), List(R, Q, N, N, K, R, B, B), List(R, Q, N, N, K, B, B, R), 
                                                  //| List(R, Q, N, B, N, K, B, R), List(R, Q, N, B, B, N, K, R), List(R, Q, N, B,
                                                  //|  B, K, R, N), List(R, Q, N, B, B, K, N, R), List(R, Q, N, B, K, R, B, N), Li
                                                  //| st(R, Q, N, B, K, N, B, R), List(R, Q, N, K, R, N, B, B), List(R, Q, N, K, R
                                                  //| , B, B, N), List(R, Q, N, K, N, R, B, B), List(R, Q, N, K, N, B, B, R), List
                                                  //| (R, Q, N, K, B, R, N, B), List(R, Q, N, K, B, N, R, B), List(R, Q, N, K, B, 
                                                  //| B, R, N), List(R, Q, N, K, B, B, N, R), List(R, Q, B, N, N, B, K, R), List(R
                                                  //| , Q, B, N, N, K, R, B), List(R, Q, B, N, K, R, N, B), List(R, Q, B, N, K, N,
                                                  //|  R, B), List(R, Q, B, N, K, B, R, N), List(R, Q, B, N, K, B, N, R), List(R, 
                                                  //| Q, B, B, N, N, K, R), List(R, Q, B, B, N, K, R, N), List(R, Q, B, B, N, K, N
                                                  //| , R), List(R, Q, B, B, K, R, N, N), List(R, Q, B, B, K, N, R, N), List(R, Q,
                                                  //|  B, B, K, N, N, R), List(R, Q, B, K, R, N, N, B), List(R, Q, B, K, R, B, N, 
                                                  //| N), List(R, Q, B, K, N, R, N, B), List(R, Q, B, K, N, N, R, B), List(R, Q, B
                                                  //| , K, N, B, R, N), List(R, Q, B, K, N, B, N, R), List(R, Q, K, R, N, N, B, B)
                                                  //| , List(R, Q, K, R, N, B, B, N), List(R, Q, K, R, B, N, N, B), List(R, Q, K, 
                                                  //| R, B, B, N, N), List(R, Q, K, N, R, N, B, B), List(R, Q, K, N, R, B, B, N), 
                                                  //| List(R, Q, K, N, N, R, B, B), List(R, Q, K, N, N, B, B, R), List(R, Q, K, N,
                                                  //|  B, R, N, B), List(R, Q, K, N, B, N, R, B), List(R, Q, K, N, B, B, R, N), Li
                                                  //| st(R, Q, K, N, B, B, N, R), List(R, Q, K, B, R, N, B, N), List(R, Q, K, B, N
                                                  //| , R, B, N), List(R, Q, K, B, N, N, B, R), List(R, Q, K, B, B, R, N, N), List
                                                  //| (R, Q, K, B, B, N, R, N), List(R, Q, K, B, B, N, N, R), List(N, R, N, B, B, 
                                                  //| K, R, Q), List(N, R, N, B, B, K, Q, R), List(N, R, N, B, B, Q, K, R), List(N
                                                  //| , R, N, B, K, R, B, Q), List(N, R, N, B, K, Q, B, R), List(N, R, N, B, Q, K,
                                                  //|  B, R), List(N, R, N, K, R, B, B, Q), List(N, R, N, K, R, Q, B, B), List(N, 
                                                  //| R, N, K, B, R, Q, B), List(N, R, N, K, B, B, R, Q), List(N, R, N, K, B, B, Q
                                                  //| , R), List(N, R, N, K, B, Q, R, B), List(N, R, N, K, Q, R, B, B), List(N, R,
                                                  //|  N, K, Q, B, B, R), List(N, R, N, Q, B, B, K, R), List(N, R, N, Q, B, K, R, 
                                                  //| B), List(N, R, N, Q, K, R, B, B), List(N, R, N, Q, K, B, B, R), List(N, R, B
                                                  //| , N, K, R, Q, B), List(N, R, B, N, K, B, R, Q), List(N, R, B, N, K, B, Q, R)
                                                  //| , List(N, R, B, N, K, Q, R, B), List(N, R, B, N, Q, B, K, R), List(N, R, B, 
                                                  //| N, Q, K, R, B), List(N, R, B, B, N, K, R, Q), List(N, R, B, B, N, K, Q, R), 
                                                  //| List(N, R, B, B, N, Q, K, R), List(N, R, B, B, K, R, N, Q), List(N, R, B, B,
                                                  //|  K, R, Q, N), List(N, R, B, B, K, N, R, Q), List(N, R, B, B, K, N, Q, R), Li
                                                  //| st(N, R, B, B, K, Q, R, N), List(N, R, B, B, K, Q, N, R), List(N, R, B, B, Q
                                                  //| , N, K, R), List(N, R, B, B, Q, K, R, N), List(N, R, B, B, Q, K, N, R), List
                                                  //| (N, R, B, K, R, N, Q, B), List(N, R, B, K, R, B, N, Q), List(N, R, B, K, R, 
                                                  //| B, Q, N), List(N, R, B, K, R, Q, N, B), List(N, R, B, K, N, R, Q, B), List(N
                                                  //| , R, B, K, N, B, R, Q), List(N, R, B, K, N, B, Q, R), List(N, R, B, K, N, Q,
                                                  //|  R, B), List(N, R, B, K, Q, R, N, B), List(N, R, B, K, Q, N, R, B), List(N, 
                                                  //| R, B, K, Q, B, R, N), List(N, R, B, K, Q, B, N, R), List(N, R, B, Q, N, B, K
                                                  //| , R), List(N, R, B, Q, N, K, R, B), List(N, R, B, Q, K, R, N, B), List(N, R,
                                                  //|  B, Q, K, N, R, B), List(N, R, B, Q, K, B, R, N), List(N, R, B, Q, K, B, N, 
                                                  //| R), List(N, R, K, R, N, B, B, Q), List(N, R, K, R, N, Q, B, B), List(N, R, K
                                                  //| , R, B, N, Q, B), List(N, R, K, R, B, B, N, Q), List(N, R, K, R, B, B, Q, N)
                                                  //| , List(N, R, K, R, B, Q, N, B), List(N, R, K, R, Q, N, B, B), List(N, R, K, 
                                                  //| R, Q, B, B, N), List(N, R, K, N, R, B, B, Q), List(N, R, K, N, R, Q, B, B), 
                                                  //| List(N, R, K, N, B, R, Q, B), List(N, R, K, N, B, B, R, Q), List(N, R, K, N,
                                                  //|  B, B, Q, R), List(N, R, K, N, B, Q, R, B), List(N, R, K, N, Q, R, B, B), Li
                                                  //| st(N, R, K, N, Q, B, B, R), List(N, R, K, B, R, N, B, Q), List(N, R, K, B, R
                                                  //| , Q, B, N), List(N, R, K, B, N, R, B, Q), List(N, R, K, B, N, Q, B, R), List
                                                  //| (N, R, K, B, B, R, N, Q), List(N, R, K, B, B, R, Q, N), List(N, R, K, B, B, 
                                                  //| N, R, Q), List(N, R, K, B, B, N, Q, R), List(N, R, K, B, B, Q, R, N), List(N
                                                  //| , R, K, B, B, Q, N, R), List(N, R, K, B, Q, R, B, N), List(N, R, K, B, Q, N,
                                                  //|  B, R), List(N, R, K, Q, R, N, B, B), List(N, R, K, Q, R, B, B, N), List(N, 
                                                  //| R, K, Q, N, R, B, B), List(N, R, K, Q, N, B, B, R), List(N, R, K, Q, B, R, N
                                                  //| , B), List(N, R, K, Q, B, N, R, B), List(N, R, K, Q, B, B, R, N), List(N, R,
                                                  //|  K, Q, B, B, N, R), List(N, R, Q, N, B, B, K, R), List(N, R, Q, N, B, K, R, 
                                                  //| B), List(N, R, Q, N, K, R, B, B), List(N, R, Q, N, K, B, B, R), List(N, R, Q
                                                  //| , B, N, K, B, R), List(N, R, Q, B, B, N, K, R), List(N, R, Q, B, B, K, R, N)
                                                  //| , List(N, R, Q, B, B, K, N, R), List(N, R, Q, B, K, R, B, N), List(N, R, Q, 
                                                  //| B, K, N, B, R), List(N, R, Q, K, R, N, B, B), List(N, R, Q, K, R, B, B, N), 
                                                  //| List(N, R, Q, K, N, R, B, B), List(N, R, Q, K, N, B, B, R), List(N, R, Q, K,
                                                  //|  B, R, N, B), List(N, R, Q, K, B, N, R, B), List(N, R, Q, K, B, B, R, N), Li
                                                  //| st(N, R, Q, K, B, B, N, R), List(N, N, R, B, B, K, R, Q), List(N, N, R, B, B
                                                  //| , K, Q, R), List(N, N, R, B, B, Q, K, R), List(N, N, R, B, K, R, B, Q), List
                                                  //| (N, N, R, B, K, Q, B, R), List(N, N, R, B, Q, K, B, R), List(N, N, R, K, R, 
                                                  //| B, B, Q), List(N, N, R, K, R, Q, B, B), List(N, N, R, K, B, R, Q, B), List(N
                                                  //| , N, R, K, B, B, R, Q), List(N, N, R, K, B, B, Q, R), List(N, N, R, K, B, Q,
                                                  //|  R, B), List(N, N, R, K, Q, R, B, B), List(N, N, R, K, Q, B, B, R), List(N, 
                                                  //| N, R, Q, B, B, K, R), List(N, N, R, Q, B, K, R, B), List(N, N, R, Q, K, R, B
                                                  //| , B), List(N, N, R, Q, K, B, B, R), List(N, N, B, R, K, R, Q, B), List(N, N,
                                                  //|  B, R, K, B, R, Q), List(N, N, B, R, K, B, Q, R), List(N, N, B, R, K, Q, R, 
                                                  //| B), List(N, N, B, R, Q, B, K, R), List(N, N, B, R, Q, K, R, B), List(N, N, B
                                                  //| , B, R, K, R, Q), List(N, N, B, B, R, K, Q, R), List(N, N, B, B, R, Q, K, R)
                                                  //| , List(N, N, B, B, Q, R, K, R), List(N, N, B, Q, R, B, K, R), List(N, N, B, 
                                                  //| Q, R, K, R, B), List(N, N, Q, R, B, B, K, R), List(N, N, Q, R, B, K, R, B), 
                                                  //| List(N, N, Q, R, K, R, B, B), List(N, N, Q, R, K, B, B, R), List(N, N, Q, B,
                                                  //|  R, K, B, R), List(N, N, Q, B, B, R, K, R), List(N, B, R, N, B, K, R, Q), Li
                                                  //| st(N, B, R, N, B, K, Q, R), List(N, B, R, N, B, Q, K, R), List(N, B, R, N, K
                                                  //| , R, B, Q), List(N, B, R, N, K, Q, B, R), List(N, B, R, N, Q, K, B, R), List
                                                  //| (N, B, R, K, R, N, B, Q), List(N, B, R, K, R, Q, B, N), List(N, B, R, K, N, 
                                                  //| R, B, Q), List(N, B, R, K, N, Q, B, R), List(N, B, R, K, B, R, N, Q), List(N
                                                  //| , B, R, K, B, R, Q, N), List(N, B, R, K, B, N, R, Q), List(N, B, R, K, B, N,
                                                  //|  Q, R), List(N, B, R, K, B, Q, R, N), List(N, B, R, K, B, Q, N, R), List(N, 
                                                  //| B, R, K, Q, R, B, N), List(N, B, R, K, Q, N, B, R), List(N, B, R, Q, N, K, B
                                                  //| , R), List(N, B, R, Q, B, N, K, R), List(N, B, R, Q, B, K, R, N), List(N, B,
                                                  //|  R, Q, B, K, N, R), List(N, B, R, Q, K, R, B, N), List(N, B, R, Q, K, N, B, 
                                                  //| R), List(N, B, N, R, B, K, R, Q), List(N, B, N, R, B, K, Q, R), List(N, B, N
                                                  //| , R, B, Q, K, R), List(N, B, N, R, K, R, B, Q), List(N, B, N, R, K, Q, B, R)
                                                  //| , List(N, B, N, R, Q, K, B, R), List(N, B, N, Q, R, K, B, R), List(N, B, N, 
                                                  //| Q, B, R, K, R), List(N, B, B, R, N, K, R, Q), List(N, B, B, R, N, K, Q, R), 
                                                  //| List(N, B, B, R, N, Q, K, R), List(N, B, B, R, K, R, N, Q), List(N, B, B, R,
                                                  //|  K, R, Q, N), List(N, B, B, R, K, N, R, Q), List(N, B, B, R, K, N, Q, R), Li
                                                  //| st(N, B, B, R, K, Q, R, N), List(N, B, B, R, K, Q, N, R), List(N, B, B, R, Q
                                                  //| , N, K, R), List(N, B, B, R, Q, K, R, N), List(N, B, B, R, Q, K, N, R), List
                                                  //| (N, B, B, N, R, K, R, Q), List(N, B, B, N, R, K, Q, R), List(N, B, B, N, R, 
                                                  //| Q, K, R), List(N, B, B, N, Q, R, K, R), List(N, B, B, Q, R, N, K, R), List(N
                                                  //| , B, B, Q, R, K, R, N), List(N, B, B, Q, R, K, N, R), List(N, B, B, Q, N, R,
                                                  //|  K, R), List(N, B, Q, R, N, K, B, R), List(N, B, Q, R, B, N, K, R), List(N, 
                                                  //| B, Q, R, B, K, R, N), List(N, B, Q, R, B, K, N, R), List(N, B, Q, R, K, R, B
                                                  //| , N), List(N, B, Q, R, K, N, B, R), List(N, B, Q, N, R, K, B, R), List(N, B,
                                                  //|  Q, N, B, R, K, R), List(N, Q, R, N, B, B, K, R), List(N, Q, R, N, B, K, R, 
                                                  //| B), List(N, Q, R, N, K, R, B, B), List(N, Q, R, N, K, B, B, R), List(N, Q, R
                                                  //| , B, N, K, B, R), List(N, Q, R, B, B, N, K, R), List(N, Q, R, B, B, K, R, N)
                                                  //| , List(N, Q, R, B, B, K, N, R), List(N, Q, R, B, K, R, B, N), List(N, Q, R, 
                                                  //| B, K, N, B, R), List(N, Q, R, K, R, N, B, B), List(N, Q, R, K, R, B, B, N), 
                                                  //| List(N, Q, R, K, N, R, B, B), List(N, Q, R, K, N, B, B, R), List(N, Q, R, K,
                                                  //|  B, R, N, B), List(N, Q, R, K, B, N, R, B), List(N, Q, R, K, B, B, R, N), Li
                                                  //| st(N, Q, R, K, B, B, N, R), List(N, Q, N, R, B, B, K, R), List(N, Q, N, R, B
                                                  //| , K, R, B), List(N, Q, N, R, K, R, B, B), List(N, Q, N, R, K, B, B, R), List
                                                  //| (N, Q, N, B, R, K, B, R), List(N, Q, N, B, B, R, K, R), List(N, Q, B, R, N, 
                                                  //| B, K, R), List(N, Q, B, R, N, K, R, B), List(N, Q, B, R, K, R, N, B), List(N
                                                  //| , Q, B, R, K, N, R, B), List(N, Q, B, R, K, B, R, N), List(N, Q, B, R, K, B,
                                                  //|  N, R), List(N, Q, B, N, R, B, K, R), List(N, Q, B, N, R, K, R, B), List(N, 
                                                  //| Q, B, B, R, N, K, R), List(N, Q, B, B, R, K, R, N), List(N, Q, B, B, R, K, N
                                                  //| , R), List(N, Q, B, B, N, R, K, R), List(B, R, N, N, K, R, Q, B), List(B, R,
                                                  //|  N, N, K, B, R, Q), List(B, R, N, N, K, B, Q, R), List(B, R, N, N, K, Q, R, 
                                                  //| B), List(B, R, N, N, Q, B, K, R), List(B, R, N, N, Q, K, R, B), List(B, R, N
                                                  //| , B, N, K, R, Q), List(B, R, N, B, N, K, Q, R), List(B, R, N, B, N, Q, K, R)
                                                  //| , List(B, R, N, B, K, R, N, Q), List(B, R, N, B, K, R, Q, N), List(B, R, N, 
                                                  //| B, K, N, R, Q), List(B, R, N, B, K, N, Q, R), List(B, R, N, B, K, Q, R, N), 
                                                  //| List(B, R, N, B, K, Q, N, R), List(B, R, N, B, Q, N, K, R), List(B, R, N, B,
                                                  //|  Q, K, R, N), List(B, R, N, B, Q, K, N, R), List(B, R, N, K, R, N, Q, B), Li
                                                  //| st(B, R, N, K, R, B, N, Q), List(B, R, N, K, R, B, Q, N), List(B, R, N, K, R
                                                  //| , Q, N, B), List(B, R, N, K, N, R, Q, B), List(B, R, N, K, N, B, R, Q), List
                                                  //| (B, R, N, K, N, B, Q, R), List(B, R, N, K, N, Q, R, B), List(B, R, N, K, Q, 
                                                  //| R, N, B), List(B, R, N, K, Q, N, R, B), List(B, R, N, K, Q, B, R, N), List(B
                                                  //| , R, N, K, Q, B, N, R), List(B, R, N, Q, N, B, K, R), List(B, R, N, Q, N, K,
                                                  //|  R, B), List(B, R, N, Q, K, R, N, B), List(B, R, N, Q, K, N, R, B), List(B, 
                                                  //| R, N, Q, K, B, R, N), List(B, R, N, Q, K, B, N, R), List(B, R, K, R, N, N, Q
                                                  //| , B), List(B, R, K, R, N, B, N, Q), List(B, R, K, R, N, B, Q, N), List(B, R,
                                                  //|  K, R, N, Q, N, B), List(B, R, K, R, Q, N, N, B), List(B, R, K, R, Q, B, N, 
                                                  //| N), List(B, R, K, N, R, N, Q, B), List(B, R, K, N, R, B, N, Q), List(B, R, K
                                                  //| , N, R, B, Q, N), List(B, R, K, N, R, Q, N, B), List(B, R, K, N, N, R, Q, B)
                                                  //| , List(B, R, K, N, N, B, R, Q), List(B, R, K, N, N, B, Q, R), List(B, R, K, 
                                                  //| N, N, Q, R, B), List(B, R, K, N, Q, R, N, B), List(B, R, K, N, Q, N, R, B), 
                                                  //| List(B, R, K, N, Q, B, R, N), List(B, R, K, N, Q, B, N, R), List(B, R, K, B,
                                                  //|  R, N, N, Q), List(B, R, K, B, R, N, Q, N), List(B, R, K, B, R, Q, N, N), Li
                                                  //| st(B, R, K, B, N, R, N, Q), List(B, R, K, B, N, R, Q, N), List(B, R, K, B, N
                                                  //| , N, R, Q), List(B, R, K, B, N, N, Q, R), List(B, R, K, B, N, Q, R, N), List
                                                  //| (B, R, K, B, N, Q, N, R), List(B, R, K, B, Q, R, N, N), List(B, R, K, B, Q, 
                                                  //| N, R, N), List(B, R, K, B, Q, N, N, R), List(B, R, K, Q, R, N, N, B), List(B
                                                  //| , R, K, Q, R, B, N, N), List(B, R, K, Q, N, R, N, B), List(B, R, K, Q, N, N,
                                                  //|  R, B), List(B, R, K, Q, N, B, R, N), List(B, R, K, Q, N, B, N, R), List(B, 
                                                  //| R, Q, N, N, B, K, R), List(B, R, Q, N, N, K, R, B), List(B, R, Q, N, K, R, N
                                                  //| , B), List(B, R, Q, N, K, N, R, B), List(B, R, Q, N, K, B, R, N), List(B, R,
                                                  //|  Q, N, K, B, N, R), List(B, R, Q, B, N, N, K, R), List(B, R, Q, B, N, K, R, 
                                                  //| N), List(B, R, Q, B, N, K, N, R), List(B, R, Q, B, K, R, N, N), List(B, R, Q
                                                  //| , B, K, N, R, N), List(B, R, Q, B, K, N, N, R), List(B, R, Q, K, R, N, N, B)
                                                  //| , List(B, R, Q, K, R, B, N, N), List(B, R, Q, K, N, R, N, B), List(B, R, Q, 
                                                  //| K, N, N, R, B), List(B, R, Q, K, N, B, R, N), List(B, R, Q, K, N, B, N, R), 
                                                  //| List(B, N, R, N, K, R, Q, B), List(B, N, R, N, K, B, R, Q), List(B, N, R, N,
                                                  //|  K, B, Q, R), List(B, N, R, N, K, Q, R, B), List(B, N, R, N, Q, B, K, R), Li
                                                  //| st(B, N, R, N, Q, K, R, B), List(B, N, R, B, N, K, R, Q), List(B, N, R, B, N
                                                  //| , K, Q, R), List(B, N, R, B, N, Q, K, R), List(B, N, R, B, K, R, N, Q), List
                                                  //| (B, N, R, B, K, R, Q, N), List(B, N, R, B, K, N, R, Q), List(B, N, R, B, K, 
                                                  //| N, Q, R), List(B, N, R, B, K, Q, R, N), List(B, N, R, B, K, Q, N, R), List(B
                                                  //| , N, R, B, Q, N, K, R), List(B, N, R, B, Q, K, R, N), List(B, N, R, B, Q, K,
                                                  //|  N, R), List(B, N, R, K, R, N, Q, B), List(B, N, R, K, R, B, N, Q), List(B, 
                                                  //| N, R, K, R, B, Q, N), List(B, N, R, K, R, Q, N, B), List(B, N, R, K, N, R, Q
                                                  //| , B), List(B, N, R, K, N, B, R, Q), List(B, N, R, K, N, B, Q, R), List(B, N,
                                                  //|  R, K, N, Q, R, B), List(B, N, R, K, Q, R, N, B), List(B, N, R, K, Q, N, R, 
                                                  //| B), List(B, N, R, K, Q, B, R, N), List(B, N, R, K, Q, B, N, R), List(B, N, R
                                                  //| , Q, N, B, K, R), List(B, N, R, Q, N, K, R, B), List(B, N, R, Q, K, R, N, B)
                                                  //| , List(B, N, R, Q, K, N, R, B), List(B, N, R, Q, K, B, R, N), List(B, N, R, 
                                                  //| Q, K, B, N, R), List(B, N, N, R, K, R, Q, B), List(B, N, N, R, K, B, R, Q), 
                                                  //| List(B, N, N, R, K, B, Q, R), List(B, N, N, R, K, Q, R, B), List(B, N, N, R,
                                                  //|  Q, B, K, R), List(B, N, N, R, Q, K, R, B), List(B, N, N, B, R, K, R, Q), Li
                                                  //| st(B, N, N, B, R, K, Q, R), List(B, N, N, B, R, Q, K, R), List(B, N, N, B, Q
                                                  //| , R, K, R), List(B, N, N, Q, R, B, K, R), List(B, N, N, Q, R, K, R, B), List
                                                  //| (B, N, Q, R, N, B, K, R), List(B, N, Q, R, N, K, R, B), List(B, N, Q, R, K, 
                                                  //| R, N, B), List(B, N, Q, R, K, N, R, B), List(B, N, Q, R, K, B, R, N), List(B
                                                  //| , N, Q, R, K, B, N, R), List(B, N, Q, N, R, B, K, R), List(B, N, Q, N, R, K,
                                                  //|  R, B), List(B, N, Q, B, R, N, K, R), List(B, N, Q, B, R, K, R, N), List(B, 
                                                  //| N, Q, B, R, K, N, R), List(B, N, Q, B, N, R, K, R), List(B, B, R, N, N, K, R
                                                  //| , Q), List(B, B, R, N, N, K, Q, R), List(B, B, R, N, N, Q, K, R), List(B, B,
                                                  //|  R, N, K, R, N, Q), List(B, B, R, N, K, R, Q, N), List(B, B, R, N, K, N, R, 
                                                  //| Q), List(B, B, R, N, K, N, Q, R), List(B, B, R, N, K, Q, R, N), List(B, B, R
                                                  //| , N, K, Q, N, R), List(B, B, R, N, Q, N, K, R), List(B, B, R, N, Q, K, R, N)
                                                  //| , List(B, B, R, N, Q, K, N, R), List(B, B, R, K, R, N, N, Q), List(B, B, R, 
                                                  //| K, R, N, Q, N), List(B, B, R, K, R, Q, N, N), List(B, B, R, K, N, R, N, Q), 
                                                  //| List(B, B, R, K, N, R, Q, N), List(B, B, R, K, N, N, R, Q), List(B, B, R, K,
                                                  //|  N, N, Q, R), List(B, B, R, K, N, Q, R, N), List(B, B, R, K, N, Q, N, R), Li
                                                  //| st(B, B, R, K, Q, R, N, N), List(B, B, R, K, Q, N, R, N), List(B, B, R, K, Q
                                                  //| , N, N, R), List(B, B, R, Q, N, N, K, R), List(B, B, R, Q, N, K, R, N), List
                                                  //| (B, B, R, Q, N, K, N, R), List(B, B, R, Q, K, R, N, N), List(B, B, R, Q, K, 
                                                  //| N, R, N), List(B, B, R, Q, K, N, N, R), List(B, B, N, R, N, K, R, Q), List(B
                                                  //| , B, N, R, N, K, Q, R), List(B, B, N, R, N, Q, K, R), List(B, B, N, R, K, R,
                                                  //|  N, Q), List(B, B, N, R, K, R, Q, N), List(B, B, N, R, K, N, R, Q), List(B, 
                                                  //| B, N, R, K, N, Q, R), List(B, B, N, R, K, Q, R, N), List(B, B, N, R, K, Q, N
                                                  //| , R), List(B, B, N, R, Q, N, K, R), List(B, B, N, R, Q, K, R, N), List(B, B,
                                                  //|  N, R, Q, K, N, R), List(B, B, N, N, R, K, R, Q), List(B, B, N, N, R, K, Q, 
                                                  //| R), List(B, B, N, N, R, Q, K, R), List(B, B, N, N, Q, R, K, R), List(B, B, N
                                                  //| , Q, R, N, K, R), List(B, B, N, Q, R, K, R, N), List(B, B, N, Q, R, K, N, R)
                                                  //| , List(B, B, N, Q, N, R, K, R), List(B, B, Q, R, N, N, K, R), List(B, B, Q, 
                                                  //| R, N, K, R, N), List(B, B, Q, R, N, K, N, R), List(B, B, Q, R, K, R, N, N), 
                                                  //| List(B, B, Q, R, K, N, R, N), List(B, B, Q, R, K, N, N, R), List(B, B, Q, N,
                                                  //|  R, N, K, R), List(B, B, Q, N, R, K, R, N), List(B, B, Q, N, R, K, N, R), Li
                                                  //| st(B, B, Q, N, N, R, K, R), List(B, Q, R, N, N, B, K, R), List(B, Q, R, N, N
                                                  //| , K, R, B), List(B, Q, R, N, K, R, N, B), List(B, Q, R, N, K, N, R, B), List
                                                  //| (B, Q, R, N, K, B, R, N), List(B, Q, R, N, K, B, N, R), List(B, Q, R, B, N, 
                                                  //| N, K, R), List(B, Q, R, B, N, K, R, N), List(B, Q, R, B, N, K, N, R), List(B
                                                  //| , Q, R, B, K, R, N, N), List(B, Q, R, B, K, N, R, N), List(B, Q, R, B, K, N,
                                                  //|  N, R), List(B, Q, R, K, R, N, N, B), List(B, Q, R, K, R, B, N, N), List(B, 
                                                  //| Q, R, K, N, R, N, B), List(B, Q, R, K, N, N, R, B), List(B, Q, R, K, N, B, R
                                                  //| , N), List(B, Q, R, K, N, B, N, R), List(B, Q, N, R, N, B, K, R), List(B, Q,
                                                  //|  N, R, N, K, R, B), List(B, Q, N, R, K, R, N, B), List(B, Q, N, R, K, N, R, 
                                                  //| B), List(B, Q, N, R, K, B, R, N), List(B, Q, N, R, K, B, N, R), List(B, Q, N
                                                  //| , N, R, B, K, R), List(B, Q, N, N, R, K, R, B), List(B, Q, N, B, R, N, K, R)
                                                  //| , List(B, Q, N, B, R, K, R, N), List(B, Q, N, B, R, K, N, R), List(B, Q, N, 
                                                  //| B, N, R, K, R), List(Q, R, N, N, B, B, K, R), List(Q, R, N, N, B, K, R, B), 
                                                  //| List(Q, R, N, N, K, R, B, B), List(Q, R, N, N, K, B, B, R), List(Q, R, N, B,
                                                  //|  N, K, B, R), List(Q, R, N, B, B, N, K, R), List(Q, R, N, B, B, K, R, N), Li
                                                  //| st(Q, R, N, B, B, K, N, R), List(Q, R, N, B, K, R, B, N), List(Q, R, N, B, K
                                                  //| , N, B, R), List(Q, R, N, K, R, N, B, B), List(Q, R, N, K, R, B, B, N), List
                                                  //| (Q, R, N, K, N, R, B, B), List(Q, R, N, K, N, B, B, R), List(Q, R, N, K, B, 
                                                  //| R, N, B), List(Q, R, N, K, B, N, R, B), List(Q, R, N, K, B, B, R, N), List(Q
                                                  //| , R, N, K, B, B, N, R), List(Q, R, B, N, N, B, K, R), List(Q, R, B, N, N, K,
                                                  //|  R, B), List(Q, R, B, N, K, R, N, B), List(Q, R, B, N, K, N, R, B), List(Q, 
                                                  //| R, B, N, K, B, R, N), List(Q, R, B, N, K, B, N, R), List(Q, R, B, B, N, N, K
                                                  //| , R), List(Q, R, B, B, N, K, R, N), List(Q, R, B, B, N, K, N, R), List(Q, R,
                                                  //|  B, B, K, R, N, N), List(Q, R, B, B, K, N, R, N), List(Q, R, B, B, K, N, N, 
                                                  //| R), List(Q, R, B, K, R, N, N, B), List(Q, R, B, K, R, B, N, N), List(Q, R, B
                                                  //| , K, N, R, N, B), List(Q, R, B, K, N, N, R, B), List(Q, R, B, K, N, B, R, N)
                                                  //| , List(Q, R, B, K, N, B, N, R), List(Q, R, K, R, N, N, B, B), List(Q, R, K, 
                                                  //| R, N, B, B, N), List(Q, R, K, R, B, N, N, B), List(Q, R, K, R, B, B, N, N), 
                                                  //| List(Q, R, K, N, R, N, B, B), List(Q, R, K, N, R, B, B, N), List(Q, R, K, N,
                                                  //|  N, R, B, B), List(Q, R, K, N, N, B, B, R), List(Q, R, K, N, B, R, N, B), Li
                                                  //| st(Q, R, K, N, B, N, R, B), List(Q, R, K, N, B, B, R, N), List(Q, R, K, N, B
                                                  //| , B, N, R), List(Q, R, K, B, R, N, B, N), List(Q, R, K, B, N, R, B, N), List
                                                  //| (Q, R, K, B, N, N, B, R), List(Q, R, K, B, B, R, N, N), List(Q, R, K, B, B, 
                                                  //| N, R, N), List(Q, R, K, B, B, N, N, R), List(Q, N, R, N, B, B, K, R), List(Q
                                                  //| , N, R, N, B, K, R, B), List(Q, N, R, N, K, R, B, B), List(Q, N, R, N, K, B,
                                                  //|  B, R), List(Q, N, R, B, N, K, B, R), List(Q, N, R, B, B, N, K, R), List(Q, 
                                                  //| N, R, B, B, K, R, N), List(Q, N, R, B, B, K, N, R), List(Q, N, R, B, K, R, B
                                                  //| , N), List(Q, N, R, B, K, N, B, R), List(Q, N, R, K, R, N, B, B), List(Q, N,
                                                  //|  R, K, R, B, B, N), List(Q, N, R, K, N, R, B, B), List(Q, N, R, K, N, B, B, 
                                                  //| R), List(Q, N, R, K, B, R, N, B), List(Q, N, R, K, B, N, R, B), List(Q, N, R
                                                  //| , K, B, B, R, N), List(Q, N, R, K, B, B, N, R), List(Q, N, N, R, B, B, K, R)
                                                  //| , List(Q, N, N, R, B, K, R, B), List(Q, N, N, R, K, R, B, B), List(Q, N, N, 
                                                  //| R, K, B, B, R), List(Q, N, N, B, R, K, B, R), List(Q, N, N, B, B, R, K, R), 
                                                  //| List(Q, N, B, R, N, B, K, R), List(Q, N, B, R, N, K, R, B), List(Q, N, B, R,
                                                  //|  K, R, N, B), List(Q, N, B, R, K, N, R, B), List(Q, N, B, R, K, B, R, N), Li
                                                  //| st(Q, N, B, R, K, B, N, R), List(Q, N, B, N, R, B, K, R), List(Q, N, B, N, R
                                                  //| , K, R, B), List(Q, N, B, B, R, N, K, R), List(Q, N, B, B, R, K, R, N), List
                                                  //| (Q, N, B, B, R, K, N, R), List(Q, N, B, B, N, R, K, R), List(Q, B, R, N, N, 
                                                  //| K, B, R), List(Q, B, R, N, B, N, K, R), List(Q, B, R, N, B, K, R, N), List(Q
                                                  //| , B, R, N, B, K, N, R), List(Q, B, R, N, K, R, B, N), List(Q, B, R, N, K, N,
                                                  //|  B, R), List(Q, B, R, K, R, N, B, N), List(Q, B, R, K, N, R, B, N), List(Q, 
                                                  //| B, R, K, N, N, B, R), List(Q, B, R, K, B, R, N, N), List(Q, B, R, K, B, N, R
                                                  //| , N), List(Q, B, R, K, B, N, N, R), List(Q, B, N, R, N, K, B, R), List(Q, B,
                                                  //|  N, R, B, N, K, R), List(Q, B, N, R, B, K, R, N), List(Q, B, N, R, B, K, N, 
                                                  //| R), List(Q, B, N, R, K, R, B, N), List(Q, B, N, R, K, N, B, R), List(Q, B, N
                                                  //| , N, R, K, B, R), List(Q, B, N, N, B, R, K, R), List(Q, B, B, R, N, N, K, R)
                                                  //| , List(Q, B, B, R, N, K, R, N), List(Q, B, B, R, N, K, N, R), List(Q, B, B, 
                                                  //| R, K, R, N, N), List(Q, B, B, R, K, N, R, N), List(Q, B, B, R, K, N, N, R), 
                                                  //| List(Q, B, B, N, R, N, K, R), List(Q, B, B, N, R, K, R, N), List(Q, B, B, N,
                                                  //|  R, K, N, R), List(Q, B, B, N, N, R, K, R))
  allStartingPositions.size                       //> res0: Int = 960

  // Create the board, which is just a map with entries like this (5,1)->Piece('P','w')
  def createBoardFromString(listChar: List[Char]): Board = {
    val pieces = (0 to 7).zip(listChar)
        .flatMap { case (pos, piece) => Map((pos, 0) -> Piece(piece, 'w'), (pos, 7) -> Piece(piece, 'b')) }
    val pawns = (0 to 7) flatMap { pos => Map((pos, 1) -> Piece('P', 'w'), (pos, 6) -> Piece('P', 'b')) }
    Board(pieces.toMap ++ pawns.toMap)
  }                                               //> createBoardFromString: (listChar: List[Char])ponder.Board

 // All pieces except pawns use this, since pawns moves are different depending on whether they are attacking or not.
 // Even the King can use this function since we filter out moves where the king is put in check when we call allAvailableMoves
 def canMoveHere(colour: Char, board: Board): ((Int, Int)) => Boolean = {
  case (newx, newy) => (
   ((0 to 7) contains newx) &&
   ((0 to 7) contains newy) &&
   (board.pieces.get(newx, newy) match {
     case Some(result) if (result.colour == colour) => false
     case _ => true
    }))
  }                                               //> canMoveHere: (colour: Char, board: ponder.Board)((Int, Int)) => Boolean
 
  // Spans of the long distance movers, i.e. Queen, Bishops, Rooks.
  def longDistanceMoves(xDirection: Int, yDirection: Int, piece: ((Int, Int), Piece), board: Board) = {
   val (clearPath, firstObstacle) = (1 to 7).map { x =>
    val oldx = piece._1._1
    val oldy = piece._1._2
    val newx = oldx + xDirection * x
    val newy = oldy + yDirection * x
    (newx, newy)
   }.span {
    case (newx, newy) => (
     ((0 to 7) contains newx) &&
     ((0 to 7) contains newy) &&
     (board.pieces.get(newx, newy) match {
      case Some(result) => false
      case _            => true
     })
    )
   }

	 // If we hit an obstacle, capture it if we can
   createBoardListFromMoves(piece, board)(
   if (!firstObstacle.isEmpty)
    board.pieces.get(firstObstacle.head._1, firstObstacle.head._2) match {
     case Some(result) if (result.colour != piece._2.colour) => clearPath.toList :+ firstObstacle.head
     case _ => clearPath.toList
    }
    else clearPath.toList
   )
  }                                               //> longDistanceMoves: (xDirection: Int, yDirection: Int, piece: ((Int, Int), p
                                                  //| onder.Piece), board: ponder.Board)List[ponder.Board]
 
 
 def createBoardListFromMoves(piece: ((Int, Int), Piece), board: Board) : List[(Int, Int)] => List[Board] = {
  _.map {
   case (newx, newy) =>
    val oldx = piece._1._1
    val oldy = piece._1._2
    Board(board.pieces - ((oldx, oldy)) + ((newx, newy) -> piece._2))
  }.toList
 }                                                //> createBoardListFromMoves: (piece: ((Int, Int), ponder.Piece), board: ponder
                                                  //| .Board)List[(Int, Int)] => List[ponder.Board]
                                                                                                    
 def kingMoves(piece: ((Int, Int), Piece), board: Board) = {
  val normalMoves = createBoardListFromMoves(piece, board)(
   for {
    x <- List(-1, 0, 1)
    y <- List(-1, 0, 1)
    if (!(x == 0 && y == 0))
    oldx = piece._1._1
    oldy = piece._1._2
    newx = oldx + x
    newy = oldy + y
    if (canMoveHere(piece._2.colour, board)(newx,newy))
   } yield (newx, newy))
    
  //  add castling Queenside only since Kingside Castling won't allow for checkmate
  // White King is on (3,0) d1, and White's Rook is on (2,0), c1, and you can't castle to get out of check.
  if ((piece._2.colour == 'w') && piece._1._1 == 3 && piece._1._2 == 0 && (board.pieces.get(2, 0) match {
    case Some(result) if (result.colour == 'w' && result.piece == 'R' && !isInCheck(board, 'w')) => true
    case _ => false
   }))
   Board(board.pieces + ((2, 0) -> Piece('K', 'w')) + ((3, 0) -> Piece('R', 'w'))) :: normalMoves
  else normalMoves
 }                                                //> kingMoves: (piece: ((Int, Int), ponder.Piece), board: ponder.Board)List[pon
                                                  //| der.Board]
 
  def knightMoves(piece: ((Int, Int), Piece), board: Board) = {
  createBoardListFromMoves(piece, board) (
   List((1,   2),
        (1,  -2),
        (-1,  2),
        (-1, -2),
        (2,   1),
        (2,  -1),
        (-2,  1),
        (-2, -1))
      .map { x =>
        val oldx = piece._1._1
        val oldy = piece._1._2
        val newx = oldx + x._1
        val newy = oldy + x._2
        (newx, newy)
      }.filter { canMoveHere(piece._2.colour, board) })
  }                                               //> knightMoves: (piece: ((Int, Int), ponder.Piece), board: ponder.Board)List[p
                                                  //| onder.Board]

  def pawnMoves(colour: Int, piece: ((Int, Int), Piece), board: Board) = {
    val movesToEmptySquare = (
     if (colour == 1) if (piece._1._2 == 1) List((0, colour * 1), (0, colour * 2)) else List((0, colour * 1))
    else if (piece._1._2 == 6) List((0, colour * 1), (0, colour * 2)) else List((0, colour * 1)))
      .map { x =>
        val oldx = piece._1._1
        val oldy = piece._1._2
        val newx = oldx + x._1
        val newy = oldy + x._2
        (newx, newy)
      }.filter {
        case (newx, newy) => (
        ((0 to 7) contains newx) &&
        ((0 to 7) contains newy) &&
        // Square must be empty
        (board.pieces.get(newx, newy) match {
          case Some(result) => false
          case _            => true
        }))
      }

    val movesToAttack = List((1, colour * 1), (-1, colour * 1))
      .map { x =>
        val oldx = piece._1._1
        val oldy = piece._1._2
        val newx = oldx + x._1
        val newy = oldy + x._2
        (newx, newy)
      }.filter {
        case (newx, newy) => (
        ((0 to 7) contains newx) &&
        ((0 to 7) contains newy) &&
        // Square must contain an opponent's piece
        (board.pieces.get(newx, newy) match {
          case Some(result) if (result.colour != piece._2.colour) => true
          case _ => false
        }))
      }
    
    createBoardListFromMoves(piece, board)(movesToEmptySquare ++ movesToAttack)
  }                                               //> pawnMoves: (colour: Int, piece: ((Int, Int), ponder.Piece), board: ponder.B
                                                  //| oard)List[ponder.Board]



  def moves(piece: ((Int, Int), Piece), board: Board, moveNumber: Int): List[Board] = (piece._2.piece, piece._2.colour) match {
    case ('K', _) => kingMoves(piece, board)
    case ('B', _) => longDistanceMoves(1, 1, piece, board) ++ longDistanceMoves(1, -1, piece, board) ++ longDistanceMoves(-1, 1, piece, board) ++ longDistanceMoves(-1, -1, piece, board)
    case ('R', _) => longDistanceMoves(1, 0, piece, board) ++ longDistanceMoves(-1, 0, piece, board) ++ longDistanceMoves(0,  1, piece, board) ++ longDistanceMoves(0,  -1, piece, board)
    case ('Q', _) => longDistanceMoves(1, 1, piece, board) ++ longDistanceMoves(1, -1, piece, board) ++ longDistanceMoves(-1, 1, piece, board) ++ longDistanceMoves(-1, -1, piece, board) ++
                     longDistanceMoves(1, 0, piece, board) ++ longDistanceMoves(-1, 0, piece, board) ++ longDistanceMoves(0,  1, piece, board) ++ longDistanceMoves(0,  -1, piece, board)

    case ('N', _) => knightMoves(piece, board)
    case ('P', _) => pawnMoves(if (piece._2.colour == 'w') 1 else -1, piece, board)
  }                                               //> moves: (piece: ((Int, Int), ponder.Piece), board: ponder.Board, moveNumber:
                                                  //|  Int)List[ponder.Board]

  // Create a list of all the boards that are possible from all the moves from this colour.
  def movesAvailable(board: Board, colour: Char, moveNumber: Int) = {
   for {
    piece <- board.pieces.filter(_._2.colour == colour)
    // For efficiency, there's no point in Black's second move being a Pawn move.
    if ( !(moveNumber == 2 && piece._2 == Piece('P', 'b')))
    move <- moves(piece, board, moveNumber)
   } yield move
  }                                               //> movesAvailable: (board: ponder.Board, colour: Char, moveNumber: Int)scala.c
                                                  //| ollection.immutable.Iterable[ponder.Board]

  // White is in check when there exists a move for black such that it will capture white's king
  // We check for the existance of a black move that literally kills white's king (it is removed from the board).
  // Note that movesAvailable doesn't worry about whether a move puts themselves in check. In fact, it leads to an infinite
  // loop if you try to do that. I.e. Are you in check? I'll see if you have a move that can take my king. Of your available moves,
  // does it put you in check? We'll see if *that* move allows me to take *your* king... etc.
  // This is also not how you play the game. You're allowed to check the opponent with a pinned piece.
  def isInCheck(board: Board, colour: Char): Boolean = {
    movesAvailable(board, if (colour == 'b') 'w' else 'b', 0)
    .exists { hypotheticalBoard => !hypotheticalBoard.pieces.values.exists(_ == Piece('K', colour)) }
  }                                               //> isInCheck: (board: ponder.Board, colour: Char)Boolean

  // White is checkmated when it is in check and all moves it can make are also in check
  def isCheckmated(board: Board, colour: Char): Boolean = {
    isInCheck(board, colour) && movesAvailable(board, colour, 0).forall { isInCheck(_, colour) }
  }                                               //> isCheckmated: (board: ponder.Board, colour: Char)Boolean
 
 // Note: Here's the only place where we ensure that no moves puts the moving player into check.
 // This covers the rule that the player must get out of check, as well as the rule that the player can't move into check.
 // A slightly more general solution would be to make this function recursive, have it yield a list of boards, prepending the current board.
 val solutionViaBruteForce = allStartingPositions.par.flatMap { startingString =>
   val startingBoard = createBoardFromString(startingString)
   (for {
       bo1 <- movesAvailable(startingBoard, 'w', 1).filter(!isInCheck(_, 'w'))
       bo2 <- movesAvailable(bo1, 'b', 1).filter(!isInCheck(_, 'b'))
       bo3 <- movesAvailable(bo2, 'w', 2).filter(!isInCheck(_, 'w'))
       bo4 <- movesAvailable(bo3, 'b', 2).filter(!isInCheck(_, 'b'))
     } yield (bo4, bo3, bo2, bo1, startingString))
     .find { case (bo4, bo3, bo2, bo1, startingString) => isCheckmated(bo4, 'w') }
  }                                               //> solutionViaBruteForce  : scala.collection.parallel.immutable.ParSeq[(ponder
                                                  //| .Board, ponder.Board, ponder.Board, ponder.Board, List[Char])] = ParVector(
                                                  //| (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (
                                                  //| 0,2) -> Piece(P,w), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piec
                                                  //| e(R,w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1
                                                  //| ) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R
                                                  //| ,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -
                                                  //| > Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b)
                                                  //| , (5,7) -> Piece(N,b), (2,2) -> Piece(P,w), (5,5) -> Piece(P,b), (2,7) -> P
                                                  //| iece(B,b), (3,7) -> Piece(K,b), (1,7) -> Piece(N,b), (1,2) -> Piece(Q,b), (
                                                  //| 6,0) -> Piece(Q,w), (1,0) -> Piece(N,w), (0,6) -> Piece(P,b), (7,0) -> Piec
                                                  //| e(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece
                                                  //| (N,w), (6,7) -> Piece(Q,b), (0,2) -> Piece(P,w), (0,0) -> Piece(R,w), (5,1)
                                                  //|  -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,
                                                  //| b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) ->
                                                  //|  Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w),
                                                  //|  (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Pi
                                                  //| ece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,2) -> Piece(P,w), (5
                                                  //| ,5) -> Piece(P,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (1,7) -> Piece
                                                  //| (N,b), (6,0) -> Piece(Q,w), (1,0) -> Piece(N,w), (0,6) -> Piece(P,b), (7,0)
                                                  //|  -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) 
                                                  //| -> Piece(N,w), (6,7) -> Piece(Q,b), (0,2) -> Piece(P,w), (0,0) -> Piece(R,w
                                                  //| ), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(B,b), (4,7) -> 
                                                  //| Piece(R,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), 
                                                  //| (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Pie
                                                  //| ce(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,
                                                  //| 6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (5,5) -> Piece(
                                                  //| P,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (1,7) -> Piece(N,b), (2,1) 
                                                  //| -> Piece(P,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(N,w), (0,6) -> Piece(P,b
                                                  //| ), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b)
                                                  //| , (5,0) -> Piece(N,w), (6,7) -> Piece(Q,b), (0,2) -> Piece(P,w), (0,0) -> P
                                                  //| iece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(B,b), (
                                                  //| 4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piec
                                                  //| e(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0
                                                  //| ) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P
                                                  //| ,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7) -
                                                  //| > Piece(B,b), (3,7) -> Piece(K,b), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w)
                                                  //| , (6,0) -> Piece(Q,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> P
                                                  //| iece(P,b), (7,0) -> Piece(B,w))),List(R, N, B, K, R, N, Q, B)), (Board(Map(
                                                  //| (7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Pie
                                                  //| ce(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (4,
                                                  //| 7) -> Piece(R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w), (4,1) -> Piece(
                                                  //| P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) 
                                                  //| -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b
                                                  //| ), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> 
                                                  //| Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(Q,b), (1,7) -> Piece(N,b), 
                                                  //| (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6) -> Pie
                                                  //| ce(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1) -> Piec
                                                  //| e(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0
                                                  //| ) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(Q
                                                  //| ,b), (4,7) -> Piece(R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w), (4,1) -
                                                  //| > Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (1,6) -> Piece(P,b)
                                                  //| , (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> P
                                                  //| iece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (
                                                  //| 3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7) -> Piec
                                                  //| e(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6
                                                  //| ) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1)
                                                  //|  -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,
                                                  //| b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) ->
                                                  //|  Piece(Q,b), (4,7) -> Piece(R,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w),
                                                  //|  (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Pi
                                                  //| ece(P,b), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4
                                                  //| ,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece
                                                  //| (B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7)
                                                  //|  -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,
                                                  //| w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(M
                                                  //| ap((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> 
                                                  //| Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), 
                                                  //| (7,7) -> Piece(Q,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (6,1) -> Pie
                                                  //| ce(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,
                                                  //| 0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(
                                                  //| P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) 
                                                  //| -> Piece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w
                                                  //| ), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> 
                                                  //| Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w)))
                                                  //| ,List(R, N, B, K, R, B, N, Q)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Pi
                                                  //| ece(P,b), (5,0) -> Piece(B,w), (0,2) -> Piece(P,w), (0,0) -> Piece(R,w), (5
                                                  //| ,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece
                                                  //| (R,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1)
                                                  //|  -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,
                                                  //| w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) ->
                                                  //|  Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,2) -> Piece(P,w),
                                                  //|  (5,5) -> Piece(P,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (1,7) -> Pi
                                                  //| ece(N,b), (1,2) -> Piece(Q,b), (6,0) -> Piece(Q,w), (1,0) -> Piece(N,w), (0
                                                  //| ,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,
                                                  //| 6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(Q,b), (0,2) -> Piece(
                                                  //| P,w), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) 
                                                  //| -> Piece(N,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w
                                                  //| ), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> 
                                                  //| Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), 
                                                  //| (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Pie
                                                  //| ce(B,b), (2,2) -> Piece(P,w), (5,5) -> Piece(P,b), (2,7) -> Piece(B,b), (3,
                                                  //| 7) -> Piece(K,b), (1,7) -> Piece(N,b), (6,0) -> Piece(Q,w), (1,0) -> Piece(
                                                  //| N,w), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P
                                                  //| ,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(Q,b), (0,2) -
                                                  //| > Piece(P,w), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w)
                                                  //| , (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1) -> P
                                                  //| iece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (
                                                  //| 2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piec
                                                  //| e(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7
                                                  //| ) -> Piece(B,b), (5,5) -> Piece(P,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K
                                                  //| ,b), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(Q,w), (1,0) -
                                                  //| > Piece(N,w), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) ->
                                                  //|  Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(Q,b),
                                                  //|  (0,2) -> Piece(P,w), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Pi
                                                  //| ece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3
                                                  //| ,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece
                                                  //| (R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6)
                                                  //|  -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,
                                                  //| b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (1,7) ->
                                                  //|  Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(N,w),
                                                  //|  (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),List(R, N,
                                                  //|  B, K, R, B, Q, N)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (
                                                  //| 5,0) -> Piece(R,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piec
                                                  //| e(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1
                                                  //| ) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R
                                                  //| ,b), (2,0) -> Piece(B,w), (0,3) -> Piece(Q,b), (3,0) -> Piece(K,w), (1,6) -
                                                  //| > Piece(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b)
                                                  //| , (5,7) -> Piece(R,b), (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> P
                                                  //| iece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (
                                                  //| 6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piec
                                                  //| e(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece
                                                  //| (P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1)
                                                  //|  -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (4,7) -> Piece(Q,
                                                  //| b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) ->
                                                  //|  Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w),
                                                  //|  (1,6) -> Piece(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Pi
                                                  //| ece(P,b), (5,7) -> Piece(R,b), (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2
                                                  //| ,7) -> Piece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece
                                                  //| (N,b), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6)
                                                  //|  -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) 
                                                  //| -> Piece(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w
                                                  //| ), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (4,7) -> 
                                                  //| Piece(Q,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), 
                                                  //| (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Pie
                                                  //| ce(K,w), (1,6) -> Piece(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,
                                                  //| 6) -> Piece(P,b), (5,7) -> Piece(R,b), (1,3) -> Piece(P,w), (2,7) -> Piece(
                                                  //| B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) 
                                                  //| -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b
                                                  //| ), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w)
                                                  //| , (7,6) -> Piece(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(N,b), (0,0) -> P
                                                  //| iece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (
                                                  //| 4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piec
                                                  //| e(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0
                                                  //| ) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P
                                                  //| ,b), (2,6) -> Piece(P,b), (5,7) -> Piece(R,b), (1,3) -> Piece(P,w), (2,7) -
                                                  //| > Piece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b)
                                                  //| , (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,6) -> P
                                                  //| iece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, N, B, K, Q, R
                                                  //| , N, B)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Pie
                                                  //| ce(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,
                                                  //| 0) -> Piece(Q,w), (7,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(
                                                  //| P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) 
                                                  //| -> Piece(B,w), (0,3) -> Piece(Q,b), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b
                                                  //| ), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> 
                                                  //| Piece(N,b), (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piece(B,b), 
                                                  //| (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0) -> Pie
                                                  //| ce(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,
                                                  //| 0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0
                                                  //| ) -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P
                                                  //| ,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (4,7) -> Piece(Q,b), (6,6) -
                                                  //| > Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w)
                                                  //| , (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> P
                                                  //| iece(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (
                                                  //| 5,7) -> Piece(N,b), (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piec
                                                  //| e(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0
                                                  //| ) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P
                                                  //| ,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,
                                                  //| b), (5,0) -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) ->
                                                  //|  Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (4,7) -> Piece(Q,b),
                                                  //|  (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Pi
                                                  //| ece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1
                                                  //| ,6) -> Piece(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece
                                                  //| (P,b), (5,7) -> Piece(N,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7)
                                                  //|  -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,
                                                  //| w), (6,0) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) ->
                                                  //|  Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> 
                                                  //| Piece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), 
                                                  //| (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(B,b), (4,7) -> Pie
                                                  //| ce(Q,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,
                                                  //| 1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(
                                                  //| K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) 
                                                  //| -> Piece(P,b), (5,7) -> Piece(N,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b
                                                  //| ), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> 
                                                  //| Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), 
                                                  //| (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, N, B, K, Q, N, R, B)), (
                                                  //| Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6
                                                  //| ,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece
                                                  //| (Q,w), (7,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1)
                                                  //|  -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,
                                                  //| w), (0,3) -> Piece(Q,b), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,5) ->
                                                  //|  Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b),
                                                  //|  (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Pi
                                                  //| ece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0) -> Piece(R,w), (1
                                                  //| ,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece
                                                  //| (N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(
                                                  //| B,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) 
                                                  //| -> Piece(Q,w), (7,7) -> Piece(N,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b
                                                  //| ), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> 
                                                  //| Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), 
                                                  //| (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Pie
                                                  //| ce(B,b), (1,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piece(B,b), (3,
                                                  //| 7) -> Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0) -> Piece(
                                                  //| R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) 
                                                  //| -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -
                                                  //| > Piece(B,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w)
                                                  //| , (4,0) -> Piece(Q,w), (7,7) -> Piece(N,b), (4,7) -> Piece(Q,b), (6,6) -> P
                                                  //| iece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (
                                                  //| 0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piec
                                                  //| e(P,b), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7
                                                  //| ) -> Piece(B,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(K
                                                  //| ,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -
                                                  //| > Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b)
                                                  //| , (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b),
                                                  //|  (5,0) -> Piece(B,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Pi
                                                  //| ece(P,w), (4,0) -> Piece(Q,w), (7,7) -> Piece(N,b), (4,7) -> Piece(Q,b), (6
                                                  //| ,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece
                                                  //| (P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6)
                                                  //|  -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,
                                                  //| b), (5,7) -> Piece(B,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) ->
                                                  //|  Piece(K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w),
                                                  //|  (6,0) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Pi
                                                  //| ece(P,b), (7,0) -> Piece(N,w))),List(R, N, B, K, Q, B, R, N)), (Board(Map((
                                                  //| 7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piec
                                                  //| e(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7
                                                  //| ) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P
                                                  //| ,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (0,3) -
                                                  //| > Piece(Q,b), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,5) -> Piece(P,b)
                                                  //| , (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (1,3) -> P
                                                  //| iece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (
                                                  //| 0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0) -> Piece(N,w), (1,0) -> Piec
                                                  //| e(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Boa
                                                  //| rd(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7)
                                                  //|  -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,
                                                  //| w), (7,7) -> Piece(R,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), (3,1) ->
                                                  //|  Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b),
                                                  //|  (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,5) -> Pi
                                                  //| ece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (1
                                                  //| ,3) -> Piece(P,w), (2,2) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece
                                                  //| (K,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (6,0) -> Piece(N,w), (1,0)
                                                  //|  -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,
                                                  //| w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w
                                                  //| ), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> 
                                                  //| Piece(Q,w), (7,7) -> Piece(R,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), 
                                                  //| (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Pie
                                                  //| ce(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,
                                                  //| 5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(
                                                  //| B,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (0,1) 
                                                  //| -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w
                                                  //| ), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> 
                                                  //| Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> P
                                                  //| iece(B,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (
                                                  //| 4,0) -> Piece(Q,w), (7,7) -> Piece(R,b), (4,7) -> Piece(Q,b), (6,6) -> Piec
                                                  //| e(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7
                                                  //| ) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P
                                                  //| ,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -
                                                  //| > Piece(B,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b)
                                                  //| , (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> P
                                                  //| iece(N,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (
                                                  //| 7,0) -> Piece(R,w))),List(R, N, B, K, Q, B, N, R)), (Board(Map((7,1) -> Pie
                                                  //| ce(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,
                                                  //| 0) -> Piece(R,w), (5,2) -> Piece(P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(
                                                  //| R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (4,1) 
                                                  //| -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(Q,w
                                                  //| ), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> 
                                                  //| Piece(P,w), (7,3) -> Piece(Q,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), 
                                                  //| (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Pie
                                                  //| ce(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N,w), (5,
                                                  //| 6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1
                                                  //| ) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N
                                                  //| ,b), (0,0) -> Piece(R,w), (5,2) -> Piece(P,w), (4,0) -> Piece(K,w), (7,7) -
                                                  //| > Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w)
                                                  //| , (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> P
                                                  //| iece(Q,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (
                                                  //| 6,3) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piec
                                                  //| e(B,b), (2,7) -> Piece(B,b), (3,7) -> Piece(Q,b), (0,1) -> Piece(P,w), (1,7
                                                  //| ) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(N
                                                  //| ,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Board(
                                                  //| Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) ->
                                                  //|  Piece(N,b), (0,0) -> Piece(R,w), (5,2) -> Piece(P,w), (4,0) -> Piece(K,w),
                                                  //|  (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (3,1) -> Pi
                                                  //| ece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2
                                                  //| ,0) -> Piece(B,w), (3,0) -> Piece(Q,w), (1,6) -> Piece(P,b), (3,6) -> Piece
                                                  //| (P,b), (1,1) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7)
                                                  //|  -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> Piece(Q,b), (0,1) -> Piece(P,
                                                  //| w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) ->
                                                  //|  Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))
                                                  //| ),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), 
                                                  //| (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,2) -> Piece(P,w), (4,0) -> Pie
                                                  //| ce(K,w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (3,
                                                  //| 1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(
                                                  //| R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(Q,w), (1,6) -> Piece(P,b), (3,6) 
                                                  //| -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b
                                                  //| ), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> Piece(Q,b), (0,1) -> 
                                                  //| Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), 
                                                  //| (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Pie
                                                  //| ce(R,w))),List(R, N, B, Q, K, B, N, R)), (Board(Map((7,1) -> Piece(P,w), (7
                                                  //| ,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(K,b), (0,0) -> Piece
                                                  //| (R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(R,b), (4,7)
                                                  //|  -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(K,
                                                  //| w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) ->
                                                  //|  Piece(N,w), (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(Q,b),
                                                  //|  (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Pi
                                                  //| ece(B,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2
                                                  //| ,1) -> Piece(P,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece
                                                  //| (P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(
                                                  //| P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(K,b), (0,0) -> Piece(R,w), (5,1) 
                                                  //| -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(R,b), (4,7) -> Piece(B,b
                                                  //| ), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(K,w), (4,1) -> 
                                                  //| Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(N,w), 
                                                  //| (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(P,w), (3,5) -> Pie
                                                  //| ce(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,
                                                  //| 7) -> Piece(Q,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(
                                                  //| N,b), (2,1) -> Piece(P,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) 
                                                  //| -> Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -
                                                  //| > Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(K,b), (0,0) -> Piece(R,w)
                                                  //| , (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(R,b), (4,7) -> P
                                                  //| iece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (4,1) -> Piece(P,w), (
                                                  //| 0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(N,w), (1,6) -> Piec
                                                  //| e(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(P,w), (3,5) -> Piece(P,b), (4,6
                                                  //| ) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(Q
                                                  //| ,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -
                                                  //| > Piece(P,w), (6,0) -> Piece(K,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b)
                                                  //| , (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w),
                                                  //|  (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(K,b), (0,0) -> Pi
                                                  //| ece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(R,b), (4
                                                  //| ,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (4,1) -> Piece
                                                  //| (P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(N,w), (1,6)
                                                  //|  -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(P,
                                                  //| w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) ->
                                                  //|  Piece(Q,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(N,b),
                                                  //|  (2,1) -> Piece(P,w), (6,0) -> Piece(K,w), (1,0) -> Piece(N,w), (5,6) -> Pi
                                                  //| ece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),List(R, N, Q, N, B, B,
                                                  //|  K, R)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piec
                                                  //| e(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7
                                                  //| ) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P
                                                  //| ,w), (6,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -
                                                  //| > Piece(K,w), (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(Q,b)
                                                  //| , (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> P
                                                  //| iece(N,b), (4,2) -> Piece(P,w), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (
                                                  //| 5,3) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0) -> Piec
                                                  //| e(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0
                                                  //| ) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0)
                                                  //|  -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,
                                                  //| w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) ->
                                                  //|  Piece(P,w), (6,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w),
                                                  //|  (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (3,5) -> Pi
                                                  //| ece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2
                                                  //| ,7) -> Piece(Q,b), (4,2) -> Piece(P,w), (3,7) -> Piece(K,b), (0,1) -> Piece
                                                  //| (P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w), (6,0)
                                                  //|  -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,
                                                  //| b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b
                                                  //| ), (5,0) -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> 
                                                  //| Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), 
                                                  //| (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Pie
                                                  //| ce(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (1,
                                                  //| 1) -> Piece(P,w), (3,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(
                                                  //| P,b), (5,7) -> Piece(N,b), (2,7) -> Piece(Q,b), (3,7) -> Piece(K,b), (0,1) 
                                                  //| -> Piece(P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> Piece(P,w
                                                  //| ), (6,0) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> 
                                                  //| Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> P
                                                  //| iece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (
                                                  //| 4,0) -> Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piec
                                                  //| e(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7
                                                  //| ) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P
                                                  //| ,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -
                                                  //| > Piece(P,b), (5,7) -> Piece(N,b), (2,7) -> Piece(Q,b), (3,7) -> Piece(K,b)
                                                  //| , (0,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(N,b), (2,1) -> P
                                                  //| iece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(N,w), (5,6) -> Piece(P,b), (
                                                  //| 0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, N, Q, K, B, N, R, B)), (B
                                                  //| oard(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,
                                                  //| 7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(
                                                  //| R,w), (4,7) -> Piece(R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w), (4,1) 
                                                  //| -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (1,6) -> Piece(P,b
                                                  //| ), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> 
                                                  //| Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7) -> Piece(B,b), 
                                                  //| (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(Q,b), (1,7) -> Pie
                                                  //| ce(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(B,w), (5,
                                                  //| 6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1
                                                  //| ) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece(N
                                                  //| ,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -
                                                  //| > Piece(Q,b), (4,7) -> Piece(R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w)
                                                  //| , (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (1,6) -> P
                                                  //| iece(P,b), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (
                                                  //| 4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7) -> Piec
                                                  //| e(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7
                                                  //| ) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(B
                                                  //| ,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(
                                                  //| Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) ->
                                                  //|  Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w),
                                                  //|  (7,7) -> Piece(Q,b), (4,7) -> Piece(R,b), (6,1) -> Piece(P,w), (4,1) -> Pi
                                                  //| ece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1
                                                  //| ,6) -> Piece(P,b), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece
                                                  //| (P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7)
                                                  //|  -> Piece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,
                                                  //| w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) ->
                                                  //|  Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))
                                                  //| ),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), 
                                                  //| (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Pie
                                                  //| ce(R,w), (7,7) -> Piece(Q,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (6,
                                                  //| 1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(
                                                  //| B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) 
                                                  //| -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b
                                                  //| ), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> 
                                                  //| Piece(P,w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), 
                                                  //| (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Pie
                                                  //| ce(Q,w))),List(R, B, B, K, R, N, N, Q)), (Board(Map((7,6) -> Piece(P,b), (5
                                                  //| ,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,2) -> Piece(Q,b), (0,0) -> Piece
                                                  //| (R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7)
                                                  //|  -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,
                                                  //| w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) ->
                                                  //|  Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b),
                                                  //|  (2,6) -> Piece(P,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Pi
                                                  //| ece(P,w), (1,7) -> Piece(B,b), (1,2) -> Piece(P,w), (2,1) -> Piece(P,w), (6
                                                  //| ,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(B,w), (5,6) -> Piece
                                                  //| (P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,6) -> Piece(
                                                  //| P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w), (5,1) 
                                                  //| -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> Piece(N,b
                                                  //| ), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> 
                                                  //| Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(R,w), 
                                                  //| (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Pie
                                                  //| ce(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,
                                                  //| 1) -> Piece(P,w), (1,7) -> Piece(B,b), (1,2) -> Piece(P,w), (2,1) -> Piece(
                                                  //| P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(B,w), (5,6) 
                                                  //| -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,6) -
                                                  //| > Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w)
                                                  //| , (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> P
                                                  //| iece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (
                                                  //| 4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piec
                                                  //| e(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,5
                                                  //| ) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(K
                                                  //| ,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (2,1) -
                                                  //| > Piece(P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(B,w)
                                                  //| , (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map
                                                  //| ((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Pi
                                                  //| ece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4
                                                  //| ,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece
                                                  //| (P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0)
                                                  //|  -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,
                                                  //| w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) ->
                                                  //|  Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b),
                                                  //|  (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Pi
                                                  //| ece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),L
                                                  //| ist(R, B, K, R, N, Q, B, N)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piec
                                                  //| e(P,b), (5,0) -> Piece(N,w), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0
                                                  //| ) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P
                                                  //| ,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -
                                                  //| > Piece(R,b), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b)
                                                  //| , (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> P
                                                  //| iece(N,b), (5,5) -> Piece(P,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (
                                                  //| 0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (2,3) -> Piece(Q,b), (2,1) -> Piec
                                                  //| e(K,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(B,w), (0,6) -> Piece(P,b), (7,0
                                                  //| ) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0)
                                                  //|  -> Piece(N,w), (6,7) -> Piece(Q,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,
                                                  //| w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) ->
                                                  //|  Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w),
                                                  //|  (0,7) -> Piece(R,b), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Pi
                                                  //| ece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5
                                                  //| ,7) -> Piece(N,b), (5,5) -> Piece(P,b), (2,7) -> Piece(K,b), (3,7) -> Piece
                                                  //| (R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (2,3) -> Piece(P,w), (2,1)
                                                  //|  -> Piece(K,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(B,w), (0,6) -> Piece(P,
                                                  //| b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b
                                                  //| ), (5,0) -> Piece(N,w), (6,7) -> Piece(Q,b), (0,0) -> Piece(R,w), (5,1) -> 
                                                  //| Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), 
                                                  //| (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Pie
                                                  //| ce(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(R,w), (1,
                                                  //| 6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(
                                                  //| P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (5,5) -> Piece(P,b), (2,7) 
                                                  //| -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b
                                                  //| ), (2,3) -> Piece(P,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(B,w), (0,6) -> 
                                                  //| Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> P
                                                  //| iece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece(Q,b), (0,0) -> Piece(R,w), (
                                                  //| 5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piec
                                                  //| e(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1
                                                  //| ) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(R
                                                  //| ,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -
                                                  //| > Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7) -> Piece(K,b)
                                                  //| , (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (2,3) -> P
                                                  //| iece(P,w), (6,0) -> Piece(Q,w), (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (
                                                  //| 0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),List(R, B, K, R, B, N, Q, N)), (B
                                                  //| oard(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,
                                                  //| 2) -> Piece(Q,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(
                                                  //| B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) 
                                                  //| -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b
                                                  //| ), (2,0) -> Piece(K,w), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> 
                                                  //| Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (2,7) -> Piece(K,b), 
                                                  //| (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (1,2) -> Pie
                                                  //| ce(P,w), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (7,2) -> Piece(P,w), (1,
                                                  //| 0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(
                                                  //| N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N
                                                  //| ,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -
                                                  //| > Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w)
                                                  //| , (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> P
                                                  //| iece(K,w), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (
                                                  //| 4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piec
                                                  //| e(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(B,b), (1,2
                                                  //| ) -> Piece(P,w), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (7,2) -> Piece(P
                                                  //| ,w), (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -
                                                  //| > Piece(N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) ->
                                                  //|  Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w),
                                                  //|  (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Pi
                                                  //| ece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2
                                                  //| ,0) -> Piece(K,w), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece
                                                  //| (P,b), (1,1) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7)
                                                  //|  -> Piece(Q,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,
                                                  //| w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (7,2) ->
                                                  //|  Piece(P,w), (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b),
                                                  //|  (7,0) -> Piece(N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), 
                                                  //| (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Pie
                                                  //| ce(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,
                                                  //| 1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(
                                                  //| R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) 
                                                  //| -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b
                                                  //| ), (5,7) -> Piece(Q,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> 
                                                  //| Piece(P,w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), 
                                                  //| (7,2) -> Piece(P,w), (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Pie
                                                  //| ce(P,b), (7,0) -> Piece(N,w))),List(R, B, K, R, B, Q, N, N)), (Board(Map((7
                                                  //| ,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece
                                                  //| (R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7)
                                                  //|  -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,
                                                  //| w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(K,w), (1,6) ->
                                                  //|  Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(Q,b), (3,5) -> Piece(P,b),
                                                  //|  (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (4,2) -> Pi
                                                  //| ece(P,w), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1
                                                  //| ,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece
                                                  //| (B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Boar
                                                  //| d(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) 
                                                  //| -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b
                                                  //| ), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> 
                                                  //| Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(Q,w), (3,0) -> Piece(K,w), 
                                                  //| (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (3,5) -> Piece(P,b), (4,6) -> Pie
                                                  //| ce(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (2,7) -> Piece(Q,b), (4,
                                                  //| 2) -> Piece(P,w), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (5,3) -> Piece(
                                                  //| P,w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) 
                                                  //| -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w
                                                  //| ))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w)
                                                  //| , (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7) -> P
                                                  //| iece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (
                                                  //| 6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piec
                                                  //| e(Q,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (1,1) -> Piece(P,w), (3,5
                                                  //| ) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N
                                                  //| ,b), (2,7) -> Piece(Q,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (5,3) -
                                                  //| > Piece(P,w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w)
                                                  //| , (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> P
                                                  //| iece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Pi
                                                  //| ece(N,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7
                                                  //| ,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece
                                                  //| (P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0)
                                                  //|  -> Piece(Q,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,
                                                  //| b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) ->
                                                  //|  Piece(N,b), (2,7) -> Piece(Q,b), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w),
                                                  //|  (5,3) -> Piece(P,w), (1,7) -> Piece(B,b), (2,1) -> Piece(P,w), (6,0) -> Pi
                                                  //| ece(R,w), (1,0) -> Piece(B,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7
                                                  //| ,0) -> Piece(N,w))),List(R, B, Q, K, B, N, R, N)), (Board(Map((7,1) -> Piec
                                                  //| e(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0
                                                  //| ) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B
                                                  //| ,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -
                                                  //| > Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w)
                                                  //| , (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> P
                                                  //| iece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Piece(R,b), (4,2) -> Piece(N,w), (
                                                  //| 3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piec
                                                  //| e(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6
                                                  //| ) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6)
                                                  //|  -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,
                                                  //| w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4,7) ->
                                                  //|  Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w),
                                                  //|  (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (1,6) -> Pi
                                                  //| ece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5
                                                  //| ,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(R,b), (4,2) -> Piece
                                                  //| (N,w), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1)
                                                  //|  -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,
                                                  //| b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w
                                                  //| ), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> 
                                                  //| Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), 
                                                  //| (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Pie
                                                  //| ce(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (1,
                                                  //| 6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,5) -> Piece(
                                                  //| P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(R,b), (4,2) 
                                                  //| -> Piece(N,w), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b
                                                  //| ), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K,w), (5,6) -> 
                                                  //| Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> P
                                                  //| iece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (
                                                  //| 0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piec
                                                  //| e(B,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1
                                                  //| ) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R
                                                  //| ,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -
                                                  //| > Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(R,b)
                                                  //| , (4,2) -> Piece(N,w), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> P
                                                  //| iece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K,w), (
                                                  //| 5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, K, R
                                                  //| , N, N, Q, B, B)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,
                                                  //| 0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(
                                                  //| P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) 
                                                  //| -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w
                                                  //| ), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> 
                                                  //| Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), 
                                                  //| (2,7) -> Piece(R,b), (4,2) -> Piece(N,w), (3,7) -> Piece(N,b), (0,1) -> Pie
                                                  //| ce(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,
                                                  //| 0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(
                                                  //| B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q
                                                  //| ,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -
                                                  //| > Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b)
                                                  //| , (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> P
                                                  //| iece(R,b), (2,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (
                                                  //| 4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piec
                                                  //| e(P,w), (2,7) -> Piece(R,b), (4,2) -> Piece(N,w), (3,7) -> Piece(N,b), (0,1
                                                  //| ) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N
                                                  //| ,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -
                                                  //| > Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) ->
                                                  //|  Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w),
                                                  //|  (4,0) -> Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Pi
                                                  //| ece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0
                                                  //| ,7) -> Piece(R,b), (2,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece
                                                  //| (P,b), (1,1) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7)
                                                  //|  -> Piece(Q,b), (2,7) -> Piece(R,b), (4,2) -> Piece(N,w), (3,7) -> Piece(N,
                                                  //| b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) ->
                                                  //|  Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b),
                                                  //|  (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), 
                                                  //| (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Pie
                                                  //| ce(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,
                                                  //| 6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(
                                                  //| P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) 
                                                  //| -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b
                                                  //| ), (5,7) -> Piece(Q,b), (2,7) -> Piece(R,b), (4,2) -> Piece(N,w), (3,7) -> 
                                                  //| Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), 
                                                  //| (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Pie
                                                  //| ce(P,b), (7,0) -> Piece(B,w))),List(R, K, R, N, B, Q, N, B)), (Board(Map((7
                                                  //| ,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece
                                                  //| (R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7)
                                                  //|  -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,
                                                  //| w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (3,0) ->
                                                  //|  Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b),
                                                  //|  (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Piece(R,b), (3,7) -> Pi
                                                  //| ece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6
                                                  //| ,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(K,w), (5,6) -> Piece
                                                  //| (P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,6) -> Piece(
                                                  //| P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w), (5,1) 
                                                  //| -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> Piece(N,b
                                                  //| ), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> 
                                                  //| Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), 
                                                  //| (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Pie
                                                  //| ce(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(R,b), (3,
                                                  //| 7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(
                                                  //| P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(K,w), (5,6) 
                                                  //| -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,6) -
                                                  //| > Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w)
                                                  //| , (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> P
                                                  //| iece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (
                                                  //| 4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (3,0) -> Piec
                                                  //| e(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,5
                                                  //| ) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(R
                                                  //| ,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -
                                                  //| > Piece(P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Piece(K,w)
                                                  //| , (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map
                                                  //| ((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Pi
                                                  //| ece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4
                                                  //| ,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece
                                                  //| (P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w), (3,0)
                                                  //|  -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,
                                                  //| w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (2,7) ->
                                                  //|  Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b),
                                                  //|  (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (7,2) -> Piece(P,w), (1,0) -> Pi
                                                  //| ece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),L
                                                  //| ist(R, K, R, B, N, Q, B, N)), (Board(Map((7,6) -> Piece(P,b), (5,0) -> Piec
                                                  //| e(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0
                                                  //| ) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P
                                                  //| ,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -
                                                  //| > Piece(R,b), (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b)
                                                  //| , (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> P
                                                  //| iece(Q,b), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (
                                                  //| 1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (7,2) -> Piec
                                                  //| e(P,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0
                                                  //| ) -> Piece(N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7)
                                                  //|  -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,
                                                  //| w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) ->
                                                  //|  Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b),
                                                  //|  (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Pi
                                                  //| ece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1
                                                  //| ,3) -> Piece(P,w), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece
                                                  //| (P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (7,2)
                                                  //|  -> Piece(P,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,
                                                  //| b), (7,0) -> Piece(N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> Piece(Q,w
                                                  //| ), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> 
                                                  //| Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), 
                                                  //| (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Pie
                                                  //| ce(R,b), (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,
                                                  //| 6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(
                                                  //| P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) 
                                                  //| -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w
                                                  //| ), (7,2) -> Piece(P,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> 
                                                  //| Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,6) -> Piece(P,b), (5,0) -> P
                                                  //| iece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (
                                                  //| 4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (6,6) -> Piec
                                                  //| e(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7
                                                  //| ) -> Piece(R,b), (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P
                                                  //| ,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -
                                                  //| > Piece(P,b), (5,7) -> Piece(Q,b), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b)
                                                  //| , (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> P
                                                  //| iece(N,w), (7,2) -> Piece(P,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (
                                                  //| 0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),List(R, K, R, B, B, Q, N, N)), (B
                                                  //| oard(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,
                                                  //| 7) -> Piece(B,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(
                                                  //| Q,w), (7,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) 
                                                  //| -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(R,w
                                                  //| ), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,5) -> Piece(P,b), (4,6) -> 
                                                  //| Piece(P,b), (1,4) -> Piece(Q,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), 
                                                  //| (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Pie
                                                  //| ce(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K,w), (5,
                                                  //| 6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1
                                                  //| ) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) -> Piece(B
                                                  //| ,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w), (7,7) -
                                                  //| > Piece(N,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w)
                                                  //| , (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> P
                                                  //| iece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,5) -> Piece(P,b), (
                                                  //| 4,6) -> Piece(P,b), (1,4) -> Piece(P,w), (2,6) -> Piece(P,b), (5,7) -> Piec
                                                  //| e(N,b), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7
                                                  //| ) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(K
                                                  //| ,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(
                                                  //| Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7) ->
                                                  //|  Piece(B,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(Q,w),
                                                  //|  (7,7) -> Piece(N,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), (3,1) -> Pi
                                                  //| ece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2
                                                  //| ,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,5) -> Piece
                                                  //| (P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (1,3)
                                                  //|  -> Piece(P,w), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,
                                                  //| w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) ->
                                                  //|  Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))
                                                  //| ),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), 
                                                  //| (6,7) -> Piece(B,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Pie
                                                  //| ce(Q,w), (7,7) -> Piece(N,b), (4,7) -> Piece(Q,b), (6,6) -> Piece(P,b), (3,
                                                  //| 1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(
                                                  //| R,b), (2,0) -> Piece(R,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) 
                                                  //| -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b
                                                  //| ), (1,3) -> Piece(P,w), (2,7) -> Piece(R,b), (3,7) -> Piece(B,b), (0,1) -> 
                                                  //| Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), 
                                                  //| (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Pie
                                                  //| ce(N,w))),List(R, K, R, B, Q, N, B, N)), (Board(Map((7,1) -> Piece(P,w), (7
                                                  //| ,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece
                                                  //| (R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4,7)
                                                  //|  -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,
                                                  //| w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) ->
                                                  //|  Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w),
                                                  //|  (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Pi
                                                  //| ece(B,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2
                                                  //| ,1) -> Piece(P,w), (6,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece
                                                  //| (P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(
                                                  //| P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) 
                                                  //| -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4,7) -> Piece(N,b
                                                  //| ), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> 
                                                  //| Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(R,w), 
                                                  //| (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> Pie
                                                  //| ce(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,
                                                  //| 7) -> Piece(B,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(
                                                  //| K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) 
                                                  //| -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -
                                                  //| > Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w)
                                                  //| , (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4,7) -> P
                                                  //| iece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (
                                                  //| 4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piec
                                                  //| e(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6
                                                  //| ) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B
                                                  //| ,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -
                                                  //| > Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b)
                                                  //| , (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w),
                                                  //|  (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Pi
                                                  //| ece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4
                                                  //| ,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece
                                                  //| (P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0)
                                                  //|  -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,
                                                  //| b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) ->
                                                  //|  Piece(B,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b),
                                                  //|  (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Pi
                                                  //| ece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, K, B, R, N, Q,
                                                  //|  N, B)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piec
                                                  //| e(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0
                                                  //| ) -> Piece(R,w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P
                                                  //| ,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -
                                                  //| > Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b)
                                                  //| , (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> Piece(P,b), (2,6) -> P
                                                  //| iece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Piece(B,b), (3,7) -> Piece(N,b), (
                                                  //| 0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piec
                                                  //| e(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Boa
                                                  //| rd(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7)
                                                  //|  -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,
                                                  //| w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (3,1) ->
                                                  //|  Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b),
                                                  //|  (2,0) -> Piece(B,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Pi
                                                  //| ece(P,b), (1,1) -> Piece(K,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5
                                                  //| ,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece
                                                  //| (N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0)
                                                  //|  -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,
                                                  //| w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w
                                                  //| ), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> 
                                                  //| Piece(R,w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), 
                                                  //| (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Pie
                                                  //| ce(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,
                                                  //| 6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(
                                                  //| Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(N,b), (0,1) 
                                                  //| -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w
                                                  //| ), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> 
                                                  //| Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> P
                                                  //| iece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (
                                                  //| 4,0) -> Piece(R,w), (7,7) -> Piece(B,b), (4,7) -> Piece(R,b), (6,6) -> Piec
                                                  //| e(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7
                                                  //| ) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P
                                                  //| ,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -
                                                  //| > Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(N,b)
                                                  //| , (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> P
                                                  //| iece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (
                                                  //| 7,0) -> Piece(B,w))),List(R, K, B, N, R, Q, N, B)), (Board(Map((7,1) -> Pie
                                                  //| ce(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b), (0,
                                                  //| 0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(
                                                  //| B,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) 
                                                  //| -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w
                                                  //| ), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> 
                                                  //| Piece(K,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), 
                                                  //| (2,7) -> Piece(B,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Pie
                                                  //| ce(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,
                                                  //| 6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,w), (7,6
                                                  //| ) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R
                                                  //| ,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b), (4,7) -
                                                  //| > Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w)
                                                  //| , (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> P
                                                  //| iece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (
                                                  //| 4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piec
                                                  //| e(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7
                                                  //| ) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (5,6) -> Piece(P
                                                  //| ,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> Piece(P,
                                                  //| w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b), (0,0) ->
                                                  //|  Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(B,b),
                                                  //|  (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Pi
                                                  //| ece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3
                                                  //| ,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece
                                                  //| (P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7)
                                                  //|  -> Piece(B,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,
                                                  //| b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(K,w), (5,6) ->
                                                  //|  Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,1) -> 
                                                  //| Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b), 
                                                  //| (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Pie
                                                  //| ce(B,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,
                                                  //| 1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(
                                                  //| B,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) 
                                                  //| -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w
                                                  //| ), (2,7) -> Piece(B,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (1,7) -> 
                                                  //| Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(K,w), 
                                                  //| (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),List(R, K, 
                                                  //| B, N, N, Q, R, B)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5
                                                  //| ,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece
                                                  //| (P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6,6)
                                                  //|  -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,
                                                  //| w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6) ->
                                                  //|  Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> Piece(P,b),
                                                  //|  (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Piece(B,b), (3,7) -> Pi
                                                  //| ece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6
                                                  //| ,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece
                                                  //| (N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(
                                                  //| Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) 
                                                  //| -> Piece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b
                                                  //| ), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> 
                                                  //| Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), 
                                                  //| (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> Piece(P,b), (2,6) -> Pie
                                                  //| ce(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,
                                                  //| 7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(
                                                  //| P,w), (6,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) 
                                                  //| -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -
                                                  //| > Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w)
                                                  //| , (4,0) -> Piece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6,6) -> P
                                                  //| iece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (
                                                  //| 0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6) -> Piec
                                                  //| e(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7
                                                  //| ) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(B
                                                  //| ,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -
                                                  //| > Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b)
                                                  //| , (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b),
                                                  //|  (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Pi
                                                  //| ece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(N,b), (4,7) -> Piece(R,b), (6
                                                  //| ,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece
                                                  //| (P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6)
                                                  //|  -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,
                                                  //| b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) ->
                                                  //|  Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w),
                                                  //|  (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) -> Pi
                                                  //| ece(P,b), (7,0) -> Piece(N,w))),List(R, K, B, B, R, Q, N, N)), (Board(Map((
                                                  //| 7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piec
                                                  //| e(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7
                                                  //| ) -> Piece(N,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P
                                                  //| ,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -
                                                  //| > Piece(B,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b)
                                                  //| , (1,1) -> Piece(K,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> P
                                                  //| iece(Q,b), (2,7) -> Piece(B,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (
                                                  //| 1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (5,6) -> Piec
                                                  //| e(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece
                                                  //| (P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b), (0,0)
                                                  //|  -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,
                                                  //| b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) ->
                                                  //|  Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w),
                                                  //|  (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Pi
                                                  //| ece(K,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1
                                                  //| ,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(B,b), (0,1) -> Piece
                                                  //| (P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (5,6)
                                                  //|  -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) 
                                                  //| -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(R,b
                                                  //| ), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> 
                                                  //| Piece(N,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), 
                                                  //| (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Pie
                                                  //| ce(B,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,
                                                  //| 5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(
                                                  //| P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) 
                                                  //| -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(K,w
                                                  //| ), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Ma
                                                  //| p((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> P
                                                  //| iece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (
                                                  //| 7,7) -> Piece(N,b), (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (3,1) -> Piec
                                                  //| e(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0
                                                  //| ) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P
                                                  //| ,b), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -
                                                  //| > Piece(P,w), (2,7) -> Piece(B,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w)
                                                  //| , (1,7) -> Piece(K,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> P
                                                  //| iece(K,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),
                                                  //| List(R, K, B, B, N, Q, R, N)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Pie
                                                  //| ce(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,
                                                  //| 1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(R,b), (4,7) -> Piece(
                                                  //| N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) 
                                                  //| -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w
                                                  //| ), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> 
                                                  //| Piece(P,b), (2,6) -> Piece(P,b), (1,3) -> Piece(Q,b), (2,7) -> Piece(B,b), 
                                                  //| (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Pie
                                                  //| ce(P,w), (6,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,
                                                  //| 0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0
                                                  //| ) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) -> Piece(P
                                                  //| ,w), (4,0) -> Piece(N,w), (7,7) -> Piece(R,b), (4,7) -> Piece(N,b), (6,6) -
                                                  //| > Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w)
                                                  //| , (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1,6) -> P
                                                  //| iece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(K,w), (4,5) -> Piece(P,b), (
                                                  //| 2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piec
                                                  //| e(B,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1
                                                  //| ) -> Piece(P,w), (6,0) -> Piece(N,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P
                                                  //| ,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,
                                                  //| b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), (5,1) ->
                                                  //|  Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(R,b), (4,7) -> Piece(N,b),
                                                  //|  (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Pi
                                                  //| ece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(B,w), (1
                                                  //| ,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,5) -> Piece(P,b), (2,6) -> Piece
                                                  //| (P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b), (3,7)
                                                  //|  -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> Piece(P,
                                                  //| w), (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), (0,6) ->
                                                  //|  Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> 
                                                  //| Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piece(N,b), (0,0) -> Piece(R,w), 
                                                  //| (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(R,b), (4,7) -> Pie
                                                  //| ce(N,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,
                                                  //| 1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(B,w), (3,0) -> Piece(
                                                  //| B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) 
                                                  //| -> Piece(P,b), (5,7) -> Piece(Q,b), (1,3) -> Piece(P,w), (2,7) -> Piece(B,b
                                                  //| ), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(K,b), (2,1) -> 
                                                  //| Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(K,w), (5,6) -> Piece(P,b), 
                                                  //| (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),List(R, K, B, B, N, Q, N, R)), (
                                                  //| Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (2,5) -> Piece(P,b), (6
                                                  //| ,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(K,w), (4,0) -> Piece
                                                  //| (B,w), (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1)
                                                  //|  -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,
                                                  //| b), (2,0) -> Piece(N,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) ->
                                                  //|  Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (5,7) -> Piece(K,b),
                                                  //|  (2,7) -> Piece(N,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (5,3) -> Pi
                                                  //| ece(Q,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(Q,w), (5
                                                  //| ,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board(Map((7,
                                                  //| 1) -> Piece(P,w), (7,6) -> Piece(P,b), (2,5) -> Piece(P,b), (6,7) -> Piece(
                                                  //| R,b), (0,0) -> Piece(R,w), (5,1) -> Piece(K,w), (4,0) -> Piece(B,w), (7,7) 
                                                  //| -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w
                                                  //| ), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> 
                                                  //| Piece(N,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), 
                                                  //| (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (5,7) -> Piece(K,b), (2,7) -> Pie
                                                  //| ce(N,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1,
                                                  //| 7) -> Piece(Q,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(
                                                  //| Q,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Board
                                                  //| (Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (2,5) -> Piece(P,b), (5,0) -
                                                  //| > Piece(K,w), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w)
                                                  //| , (7,7) -> Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> P
                                                  //| iece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (
                                                  //| 2,0) -> Piece(N,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piec
                                                  //| e(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (5,7) -> Piece(K,b), (2,7
                                                  //| ) -> Piece(N,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (5,3) -> Piece(P
                                                  //| ,w), (1,7) -> Piece(Q,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -
                                                  //| > Piece(Q,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w)
                                                  //| )),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(K,w),
                                                  //|  (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7) -> Pi
                                                  //| ece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6
                                                  //| ,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece
                                                  //| (N,w), (3,0) -> Piece(N,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1)
                                                  //|  -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(K,
                                                  //| b), (2,7) -> Piece(N,b), (3,7) -> Piece(N,b), (0,1) -> Piece(P,w), (5,3) ->
                                                  //|  Piece(P,w), (1,7) -> Piece(Q,b), (2,1) -> Piece(P,w), (6,0) -> Piece(R,w),
                                                  //|  (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Pi
                                                  //| ece(B,w))),List(R, Q, N, N, B, K, R, B)), (Board(Map((7,1) -> Piece(P,w), (
                                                  //| 7,6) -> Piece(P,b), (2,5) -> Piece(P,b), (6,7) -> Piece(R,b), (0,0) -> Piec
                                                  //| e(R,w), (5,1) -> Piece(K,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7
                                                  //| ) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P
                                                  //| ,w), (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(N,w), (3,0) -
                                                  //| > Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w)
                                                  //| , (4,6) -> Piece(P,b), (5,7) -> Piece(K,b), (2,7) -> Piece(N,b), (3,7) -> P
                                                  //| iece(B,b), (0,1) -> Piece(P,w), (5,3) -> Piece(Q,b), (2,1) -> Piece(P,w), (
                                                  //| 6,0) -> Piece(R,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6) -> Piec
                                                  //| e(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece
                                                  //| (P,b), (2,5) -> Piece(P,b), (6,7) -> Piece(R,b), (0,0) -> Piece(R,w), (5,1)
                                                  //|  -> Piece(K,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,
                                                  //| b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) ->
                                                  //|  Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(N,w), (3,0) -> Piece(B,w),
                                                  //|  (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Pi
                                                  //| ece(P,b), (5,7) -> Piece(K,b), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0
                                                  //| ,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(Q,b), (2,1) -> Piece
                                                  //| (P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6)
                                                  //|  -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) 
                                                  //| -> Piece(P,b), (2,5) -> Piece(P,b), (5,0) -> Piece(K,w), (6,7) -> Piece(R,b
                                                  //| ), (0,0) -> Piece(R,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> 
                                                  //| Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), 
                                                  //| (4,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(N,w), (3,0) -> Pie
                                                  //| ce(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,
                                                  //| 6) -> Piece(P,b), (5,7) -> Piece(K,b), (2,7) -> Piece(N,b), (3,7) -> Piece(
                                                  //| B,b), (0,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(Q,b), (2,1) 
                                                  //| -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b
                                                  //| ), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w)
                                                  //| , (7,6) -> Piece(P,b), (5,0) -> Piece(K,w), (6,7) -> Piece(R,b), (0,0) -> P
                                                  //| iece(R,w), (4,0) -> Piece(B,w), (7,7) -> Piece(N,b), (4,7) -> Piece(B,b), (
                                                  //| 6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (4,1) -> Piec
                                                  //| e(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(N,w), (3,0) -> Piece(B,w), (1,6
                                                  //| ) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P
                                                  //| ,b), (2,6) -> Piece(P,b), (5,7) -> Piece(K,b), (2,7) -> Piece(N,b), (3,7) -
                                                  //| > Piece(B,b), (0,1) -> Piece(P,w), (5,3) -> Piece(P,w), (1,7) -> Piece(Q,b)
                                                  //| , (2,1) -> Piece(P,w), (6,0) -> Piece(R,w), (1,0) -> Piece(Q,w), (5,6) -> P
                                                  //| iece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),List(R, Q, N, B, B, K
                                                  //| , R, N)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (2,5) -> Pie
                                                  //| ce(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w), (5,
                                                  //| 1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> Piece(
                                                  //| N,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P,w), (0,7) -> Piece(R,b), (2,0) 
                                                  //| -> Piece(K,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b
                                                  //| ), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (5,7) -> Piece(R,b), (3,2) -> 
                                                  //| Piece(P,w), (2,7) -> Piece(K,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), 
                                                  //| (5,3) -> Piece(Q,b), (2,1) -> Piece(P,w), (4,3) -> Piece(P,w), (6,0) -> Pie
                                                  //| ce(B,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,
                                                  //| 0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (2,5
                                                  //| ) -> Piece(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R
                                                  //| ,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -
                                                  //| > Piece(N,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P,w), (0,7) -> Piece(R,b)
                                                  //| , (2,0) -> Piece(K,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> P
                                                  //| iece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (5,7) -> Piece(R,b), (
                                                  //| 3,2) -> Piece(P,w), (2,7) -> Piece(K,b), (3,7) -> Piece(B,b), (0,1) -> Piec
                                                  //| e(P,w), (1,7) -> Piece(Q,b), (2,1) -> Piece(P,w), (4,3) -> Piece(P,w), (6,0
                                                  //| ) -> Piece(B,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P
                                                  //| ,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,
                                                  //| b), (2,5) -> Piece(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(B,b), (0,0) ->
                                                  //|  Piece(R,w), (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b),
                                                  //|  (4,7) -> Piece(N,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P,w), (4,1) -> Pi
                                                  //| ece(P,w), (0,7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(B,w), (1
                                                  //| ,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece
                                                  //| (P,b), (5,7) -> Piece(R,b), (3,2) -> Piece(P,w), (2,7) -> Piece(K,b), (3,7)
                                                  //|  -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(Q,b), (2,1) -> Piece(P,
                                                  //| w), (6,0) -> Piece(B,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), (0,6) ->
                                                  //|  Piece(P,b), (7,0) -> Piece(N,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> 
                                                  //| Piece(P,b), (5,0) -> Piece(R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(R,w), 
                                                  //| (5,1) -> Piece(P,w), (4,0) -> Piece(N,w), (7,7) -> Piece(N,b), (4,7) -> Pie
                                                  //| ce(N,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,
                                                  //| 7) -> Piece(R,b), (2,0) -> Piece(K,w), (3,0) -> Piece(B,w), (1,6) -> Piece(
                                                  //| P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) 
                                                  //| -> Piece(P,b), (5,7) -> Piece(R,b), (3,2) -> Piece(P,w), (2,7) -> Piece(K,b
                                                  //| ), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(Q,b), (2,1) -> 
                                                  //| Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piece(Q,w), (5,6) -> Piece(P,b), 
                                                  //| (0,6) -> Piece(P,b), (7,0) -> Piece(N,w))),List(R, Q, K, B, N, R, B, N)), (
                                                  //| Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(R,w), (6
                                                  //| ,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece
                                                  //| (K,w), (4,7) -> Piece(K,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7)
                                                  //|  -> Piece(N,b), (2,0) -> Piece(N,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,
                                                  //| b), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), (4,6) -> Piece(P,b), (2,6) ->
                                                  //|  Piece(P,b), (5,7) -> Piece(R,b), (3,2) -> Piece(P,w), (2,2) -> Piece(Q,b),
                                                  //|  (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Pi
                                                  //| ece(R,b), (1,2) -> Piece(P,w), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w), (1
                                                  //| ,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece
                                                  //| (Q,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(
                                                  //| R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) 
                                                  //| -> Piece(K,w), (7,7) -> Piece(Q,b), (4,7) -> Piece(K,b), (6,1) -> Piece(P,w
                                                  //| ), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(N,w), (3,0) -> 
                                                  //| Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (6,5) -> Piece(P,b), 
                                                  //| (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(R,b), (3,2) -> Pie
                                                  //| ce(P,w), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (1,
                                                  //| 7) -> Piece(R,b), (1,2) -> Piece(P,w), (2,1) -> Piece(P,w), (6,0) -> Piece(
                                                  //| B,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) 
                                                  //| -> Piece(Q,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -
                                                  //| > Piece(R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w)
                                                  //| , (4,0) -> Piece(K,w), (7,7) -> Piece(Q,b), (4,7) -> Piece(K,b), (6,1) -> P
                                                  //| iece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(N,w), (
                                                  //| 3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (6,5) -> Piec
                                                  //| e(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7
                                                  //| ) -> Piece(R,b), (3,2) -> Piece(P,w), (2,7) -> Piece(N,b), (3,7) -> Piece(B
                                                  //| ,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(P,w), (6,0) -
                                                  //| > Piece(B,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b)
                                                  //| , (7,0) -> Piece(Q,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b),
                                                  //|  (5,0) -> Piece(R,w), (6,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Pi
                                                  //| ece(P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(Q,b), (4,7) -> Piece(K,b), (6
                                                  //| ,6) -> Piece(P,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece
                                                  //| (N,b), (2,0) -> Piece(N,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6)
                                                  //|  -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,
                                                  //| b), (5,7) -> Piece(R,b), (3,2) -> Piece(P,w), (2,7) -> Piece(N,b), (3,7) ->
                                                  //|  Piece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(P,w),
                                                  //|  (6,0) -> Piece(B,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Pi
                                                  //| ece(P,b), (7,0) -> Piece(Q,w))),List(N, R, N, B, K, R, B, Q)), (Board(Map((
                                                  //| 7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7) -> Piec
                                                  //| e(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(K,w), (7,7
                                                  //| ) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P
                                                  //| ,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(N,w), (3,0) -
                                                  //| > Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w)
                                                  //| , (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (3,2) -> Piece(P,w), (1,3) -> P
                                                  //| iece(Q,b), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0,1) -> Piece(P,w), (
                                                  //| 1,7) -> Piece(R,b), (2,3) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0) -> Piec
                                                  //| e(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Boa
                                                  //| rd(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w), (6,7)
                                                  //|  -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(K,
                                                  //| w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), (6,1) ->
                                                  //|  Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(N,w),
                                                  //|  (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Pi
                                                  //| ece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(Q,b), (3
                                                  //| ,2) -> Piece(P,w), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0,1) -> Piece
                                                  //| (P,w), (1,7) -> Piece(R,b), (2,3) -> Piece(P,w), (6,0) -> Piece(B,w), (1,0)
                                                  //|  -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,
                                                  //| w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(Q,w
                                                  //| ), (6,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> 
                                                  //| Piece(K,w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piece(P,b), 
                                                  //| (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Pie
                                                  //| ce(N,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,
                                                  //| 1) -> Piece(P,w), (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(
                                                  //| Q,b), (3,2) -> Piece(P,w), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b), (0,1) 
                                                  //| -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(P,w), (6,0) -> Piece(B,w
                                                  //| ), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> 
                                                  //| Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> P
                                                  //| iece(Q,w), (6,7) -> Piece(B,b), (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (
                                                  //| 4,0) -> Piece(K,w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6) -> Piec
                                                  //| e(P,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0
                                                  //| ) -> Piece(N,w), (3,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P
                                                  //| ,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -
                                                  //| > Piece(Q,b), (3,2) -> Piece(P,w), (2,7) -> Piece(N,b), (3,7) -> Piece(B,b)
                                                  //| , (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(P,w), (6,0) -> P
                                                  //| iece(B,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (
                                                  //| 7,0) -> Piece(R,w))),List(N, R, N, B, K, Q, B, R)), (Board(Map((7,1) -> Pie
                                                  //| ce(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,
                                                  //| 0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (4,7) -> Piece(
                                                  //| R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) 
                                                  //| -> Piece(N,b), (2,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b
                                                  //| ), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (2,6) -> 
                                                  //| Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> Piece(K,b), 
                                                  //| (0,1) -> Piece(P,w), (3,3) -> Piece(Q,b), (1,7) -> Piece(R,b), (2,1) -> Pie
                                                  //| ce(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,
                                                  //| 6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1) -> Piece(P,w), (7,6
                                                  //| ) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) -> Piece(N
                                                  //| ,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(Q,b), (4,7) -
                                                  //| > Piece(R,b), (3,1) -> Piece(K,w), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w)
                                                  //| , (0,7) -> Piece(N,b), (2,0) -> Piece(B,w), (1,6) -> Piece(P,b), (3,6) -> P
                                                  //| iece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece(P,b), (
                                                  //| 2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7) -> Piec
                                                  //| e(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1
                                                  //| ) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P
                                                  //| ,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1) -> Piece(P,
                                                  //| w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) ->
                                                  //|  Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Piece(Q,b),
                                                  //|  (4,7) -> Piece(R,b), (6,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Pi
                                                  //| ece(N,b), (2,0) -> Piece(B,w), (3,0) -> Piece(K,w), (1,6) -> Piece(P,b), (3
                                                  //| ,6) -> Piece(P,b), (6,5) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) -> Piece
                                                  //| (P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,7)
                                                  //|  -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7) -> Piece(R,
                                                  //| b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) ->
                                                  //|  Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),Board(Map((7,1) -> 
                                                  //| Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), 
                                                  //| (0,0) -> Piece(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(R,w), (7,7) -> Pie
                                                  //| ce(Q,b), (4,7) -> Piece(R,b), (6,6) -> Piece(P,b), (6,1) -> Piece(P,w), (4,
                                                  //| 1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(B,w), (3,0) -> Piece(
                                                  //| K,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,6) 
                                                  //| -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b
                                                  //| ), (3,7) -> Piece(K,b), (0,1) -> Piece(P,w), (3,3) -> Piece(P,w), (1,7) -> 
                                                  //| Piece(R,b), (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), 
                                                  //| (5,6) -> Piece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(Q,w))),List(N, R, 
                                                  //| B, K, R, B, N, Q)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5
                                                  //| ,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) -> Piece(N,w), (5,2) -> Piece
                                                  //| (P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b), (6,6)
                                                  //|  -> Piece(P,b), (3,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,
                                                  //| b), (2,0) -> Piece(B,w), (3,0) -> Piece(Q,w), (1,6) -> Piece(P,b), (3,6) ->
                                                  //|  Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(P,w), (7,3) -> Piece(Q,b),
                                                  //|  (4,5) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Pi
                                                  //| ece(B,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(P,w), (6
                                                  //| ,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) -> Piece
                                                  //| (P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(
                                                  //| P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) -> Piece(N,w), (5,2) 
                                                  //| -> Piece(P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(R,b), (4,7) -> Piece(K,b
                                                  //| ), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> 
                                                  //| Piece(N,b), (2,0) -> Piece(B,w), (3,0) -> Piece(Q,w), (1,6) -> Piece(P,b), 
                                                  //| (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (6,3) -> Piece(P,w), (4,5) -> Pie
                                                  //| ce(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B,b), (3,
                                                  //| 7) -> Piece(Q,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -> Piece(
                                                  //| P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b), (0,6) 
                                                  //| -> Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w), (7,6) -
                                                  //| > Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) -> Piece(N,w)
                                                  //| , (5,2) -> Piece(P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(R,b), (4,7) -> P
                                                  //| iece(K,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P,w), (
                                                  //| 4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(B,w), (3,0) -> Piec
                                                  //| e(Q,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w), (4,5
                                                  //| ) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) -> Piece(B
                                                  //| ,b), (3,7) -> Piece(Q,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b), (2,1) -
                                                  //| > Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Piece(P,b)
                                                  //| , (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),Board(Map((7,1) -> Piece(P,w),
                                                  //|  (7,6) -> Piece(P,b), (5,0) -> Piece(B,w), (6,7) -> Piece(N,b), (0,0) -> Pi
                                                  //| ece(N,w), (5,2) -> Piece(P,w), (4,0) -> Piece(K,w), (7,7) -> Piece(R,b), (4
                                                  //| ,7) -> Piece(K,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece
                                                  //| (P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(B,w), (3,0)
                                                  //|  -> Piece(Q,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,
                                                  //| w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(B,b), (2,7) ->
                                                  //|  Piece(B,b), (3,7) -> Piece(Q,b), (0,1) -> Piece(P,w), (1,7) -> Piece(R,b),
                                                  //|  (2,1) -> Piece(P,w), (6,0) -> Piece(N,w), (1,0) -> Piece(R,w), (5,6) -> Pi
                                                  //| ece(P,b), (0,6) -> Piece(P,b), (7,0) -> Piece(R,w))),List(N, R, B, Q, K, B,
                                                  //|  N, R)), (Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piec
                                                  //| e(N,w), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(B,b), (4,7
                                                  //| ) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) -> Piece(P
                                                  //| ,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(K,w), (3,0) -
                                                  //| > Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Piece(P,w)
                                                  //| , (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (5,5) -> P
                                                  //| iece(P,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece(P,w), (
                                                  //| 1,7) -> Piece(R,b), (2,3) -> Piece(Q,b), (1,2) -> Piece(N,w), (6,0) -> Piec
                                                  //| e(Q,w), (1,0) -> Piece(R,w), (0,6) -> Piece(P,b), (7,0) -> Piece(B,w))),Boa
                                                  //| rd(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w), (6,7)
                                                  //|  -> Piece(Q,b), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> Piece(B,
                                                  //| b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), (6,1) ->
                                                  //|  Piece(P,w), (4,1) -> Piece(P,w), (0,7) -> Piece(N,b), (2,0) -> Piece(K,w),
                                                  //|  (3,0) -> Piece(R,w), (1,6) -> Piece(P,b), (3,6) -> Piece(P,b), (1,1) -> Pi
                                                  //| ece(P,w), (4,6) -> Piece(P,b), (2,6) -> Piece(P,b), (5,7) -> Piece(N,b), (5
                                                  //| ,5) -> Piece(P,b), (2,7) -> Piece(K,b), (3,7) -> Piece(R,b), (0,1) -> Piece
                                                  //| (P,w), (1,7) -> Piece(R,b), (2,3) -> Piece(P,w), (1,2) -> Piece(N,w), (6,0)
                                                  //|  -> Piece(Q,w), (1,0) -> Piece(R,w), (0,6) -> Piece(P,b), (7,0) -> Piece(B,
                                                  //| w))),Board(Map((7,1) -> Piece(P,w), (7,6) -> Piece(P,b), (5,0) -> Piece(N,w
                                                  //| ), (6,7) -> Piece(Q,b), (5,1) -> Piece(P,w), (4,0) -> Piece(B,w), (7,7) -> 
                                                  //| Piece(B,b), (4,7) -> Piece(B,b), (6,6) -> Piece(P,b), (3,1) -> Piece(P,w), 
                                                  //| (6,1) -> Piece(P,w), (4,1) -> Piece(P,w),
                                                  //| Output exceeds cutoff limit.
 def printBoard(board: Board) = {
  (7 to 0 by -1) foreach { y =>
   (0 to 7) foreach { x =>
    print(board.pieces.get(x, y) match {
     case Some(result) => if (result.colour == 'w') result.piece.toLower else result.piece.toUpper
     case _ => " "
    })
   }
   println()
  }
 }                                                //> printBoard: (board: ponder.Board)Unit
  
 
 def printAllSolutions =
 solutionViaBruteForce.zipWithIndex foreach {
  case ((bo4, bo3, bo2, bo1, startingString),i) =>
   println("Board 1 ", i)
   printBoard(bo1)
   println()
   println("Board 2 ", i)
   printBoard(bo2)
   println()
   println("Board 3 ", i)
   printBoard(bo3)
   println()
   println("BOARD 4 ", i)
   printBoard(bo4)
   println()
   println()
   case _ => println("Should not happen")
  }                                               //> printAllSolutions: => Unit
 printAllSolutions                                //> (Board 1 ,0)
                                                  //| (Board 1 ,23)
                                                  //| (Board 1 ,46)
                                                  //| (Board 1 ,69)
                                                  //| RBKRBKBBRNQNNQNR
                                                  //| 
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|   N B R K R Q B
                                                  //| pRpN BpKpRpNpQpB
                                                  //| brkbnnqr
                                                  //| 
                                                  //| (Board 2 ,69)
                                                  //| BRKBNNQR
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| brkbnnqr
                                                  //| 
                                                  //| (Board 3 ,69)
                                                  //| BRKBNNQR
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| br bnnqr
                                                  //| 
                                                  //| (BOARD 4 ,69)
                                                  //| BRKBNN R
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|       PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbrqnn
                                                  //| 
                                                  //| (Board 2 ,23)
                                                  //| RKBBRQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbrqnn
                                                  //| 
                                                  //| (Board 3 ,23)
                                                  //| RKBBRQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bbrqnn
                                                  //| 
                                                  //| (BOARD 4 ,23)
                                                  //| RKBBR NN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bbrqnn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,24)
                                                  //| RKBBNQRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbnqrn
                                                  //| 
                                                  //| (Board 2 ,24)
                                                  //| RKBBNQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbnqrn
                                                  //| 
                                                  //| (Board 3 ,24)
                                                  //| RKBBNQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bbnqrn
                                                  //| 
                                                  //| (BOARD 4 ,24)
                                                  //| RKBBN RN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bbnqrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,25)
                                                  //| RKBBNQNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbnqnr
                                                  //| 
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| p       
                                                  //|  ppppppp
                                                  //| rnbkrnqb
                                                  //| 
                                                  //| (Board 2 ,0)
                                                  //| RNBKRNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| p       
                                                  //|  ppppppp
                                                  //| rnbkrnqb
                                                  //| 
                                                  //| (Board 3 ,0)
                                                  //| RNBKRNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| p p     
                                                  //|  p ppppp
                                                  //| rnbkrnqb
                                                  //| 
                                                  //| (BOARD 4 ,0)
                                                  //| RNBKRN B
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| pQp     
                                                  //|  p ppppp
                                                  //| rnbkrnqb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,1)
                                                  //| RNBKRBNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| rnbkrbnq
                                                  //| 
                                                  //| (Board 2 ,1)
                                                  //| RNBKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| rnbkrbnq
                                                  //| 
                                                  //| (Board 3 ,1)
                                                  //| RNBKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| rnb rbnq
                                                  //| 
                                                  //| (BOARD 4 ,1)
                                                  //| RNBKRBN 
                                                  //| PPPPPP P
                                                  //|      N
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrrqbn
                                                  //| 
                                                  //| (Board 2 ,46)
                                                  //| NBRKRQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrrqbn
                                                  //| 
                                                  //| (Board 3 ,46)
                                                  //| NBRKRQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //| p pppppp
                                                  //| nbkrrqbn
                                                  //| 
                                                  //| (BOARD 4 ,46)
                                                  //| NBRKR BN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| Qp      
                                                  //| p pppppp
                                                  //| nbkrrqbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,47)
                                                  //| NBRKNQBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrnqbr
                                                  //| 
                                                  //| (Board 2 ,47)
                                                  //| NBRKNQBR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrnqbr
                                                  //| 
                                                  //| (Board 3 ,47)
                                                  //| NBRKNQBR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //| p pppppp
                                                  //| nbkrnqbr
                                                  //| 
                                                  //| (BOARD 4 ,47)
                                                  //| NBRKN BR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| Qp      
                                                  //| p pppppp
                                                  //| nbkrnqbr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,48)
                                                  //| NBRKBRNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrkbrnq
                                                  //| 
                                                  //| (Board 2 ,48)
                                                  //| NBRKBRNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrkbrnq
                                                  //| 
                                                  //| (Board 3 ,48)
                                                  //| NBRKBRNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nbr brnq
                                                  //| 
                                                  //| (BOARD 4 ,48)
                                                  //| NBRKBRN 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nbr brnq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,49)
                                                  //| NBRKBNRQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrkbnrq
                                                  //| 
                                                  //| (Board 2 ,49)
                                                  //| NBRKBNRQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrkbnrq
                                                  //| 
                                                  //| (Board 3 ,49)
                                                  //| NBRKBNRQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nbr bnrq
                                                  //| 
                                                  //| (BOARD 4 ,49)
                                                  //| NBRKBNR 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| rnb rbnq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,2)
                                                  //| RNBKRBQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| p       
                                                  //|  ppppppp
                                                  //| rnbkrbqn
                                                  //| 
                                                  //| (Board 2 ,2)
                                                  //| RNBKRBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| p       
                                                  //|  ppppppp
                                                  //| rnbkrbqn
                                                  //| 
                                                  //| (Board 3 ,2)
                                                  //| RNBKRBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| p p     
                                                  //|  p ppppp
                                                  //| rnbkrbqn
                                                  //| 
                                                  //| (BOARD 4 ,2)
                                                  //| RNBKRB N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //| pQp     
                                                  //|  p ppppp
                                                  //| rnbkrbqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,3)
                                                  //| RNBKQRNB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqrnb
                                                  //| 
                                                  //| (Board 2 ,3)
                                                  //| RNBKQRNB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqrnb
                                                  //| 
                                                  //| (Board 3 ,3)
                                                  //| RNBKQRNB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqrnb
                                                  //| 
                                                  //| (BOARD 4 ,3)
                                                  //| RNBK RNB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //| Qp      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqrnb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,4)
                                                  //| RNBKQNRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqnrb
                                                  //| 
                                                  //| (Board 2 ,4)
                                                  //| RNBKQNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqnrb
                                                  //| 
                                                  //| (Board 3 ,4)
                                                  //| RNBKQNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqnrb
                                                  //| 
                                                  //| (BOARD 4 ,4)
                                                  //| RNBK NRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //| Qp      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqnrb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,5)
                                                  //| RNBKQBRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqbrn
                                                  //| 
                                                  //| (Board 2 ,5)
                                                  //| RNBKQBRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|   
                                                  //| ppkppppp
                                                  //| br bnnqr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,70)
                                                  //| BRQNNBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| brqnnbkr
                                                  //| 
                                                  //| (Board 2 ,70)
                                                  //| BRQNNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| brqnnbkr
                                                  //| 
                                                  //| (Board 3 ,70)
                                                  //| BRQNNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| brqnnb r
                                                  //| 
                                                  //| (BOARD 4 ,70)
                                                  //| BR NNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| brqnnb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,71)
                                                  //| BNRKRBNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| bn
                                                  //| (Board 2 ,25)
                                                  //| RKBBNQNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbbnqnr
                                                  //| 
                                                  //| (Board 3 ,25)
                                                  //| RKBBNQNR
                                                  //| PPPP PPP
                                                  //|    
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nbr bnrq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,50)
                                                  //| NBRKBQRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrbqrn
                                                  //| 
                                                  //| (Board 2 ,50)
                                                  //| NBRKBQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrbqrn
                                                  //| 
                                                  //| (Board 3 ,50)
                                                  //| NBRKBQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //| p pppppp
                                                  //| nbkrbqrn
                                                  //| 
                                                  //| (BOARD 4 ,50)
                                                  //| NBRKB RN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| Qp      
                                                  //| p pppppp
                                                  //| nbkrbqrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,51)
                                                  //| NBRKBQNR
                                                  //| PPPPPPPP
                                                  //|           
                                                  //|                 
                                                  //| rkrbnq
                                                  //| 
                                                  //| (Board 2 ,71)
                                                  //| BNRKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| bnrkrbnq
                                                  //| 
                                                  //| (Board 3 ,71)
                                                  //| BNRKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| bnr rbnq
                                                  //| 
                                                  //| (BOARD 4 ,71)
                                                  //| BNRKRBN 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| bnr rbnq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,72)
                                                  //| BNRQKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| bnrqkbnr
                                                  //| 
                                                  //| (Board 2 ,72)
                                                  //| BNRQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| bnrqkbnr
                                                  //| 
                                                  //| (Board 3 ,72)
                                                  //| BNRQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       p 
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| bnrqkbnr
                                                  //| 
                                                  //| (BOARD 4 ,72)
                                                  //| BNR KBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       pQ
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| bnrqkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,73)
                                                  //| BNQRNBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| bnqrnbkr
                                                  //| 
                                                  //| (Board 2 ,73)
                                                  //| BNQRNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| bnqrnbkr
                                                  //| 
                                                  //| (Board 3 ,73)
                                                  //| BNQRNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| bnqrnb r
                                                  //| 
                                                  //| (BOARD 4 ,73)
                                                  //| BN RNBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| bnqrnb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,74)
                                                  //| BNQNRBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| bnqnrbkr
                                                  //| 
                                                  //| (Board 2 ,74)
                                                  //| BNQNRBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| bnqnrbkr
                                                  //| 
                                                  //| (Board 3 ,74)
                                                  //| BNQNRBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| bnqnrb r
                                                  //| 
                                                  //| (BOARD 4 ,74)
                                                  //| BN NRBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| bnqnrb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,75)
                                                  //| BNQBNRKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| bnqb rkr
                                                  //| 
                                                  //| (Board 2 ,75)
                                                  //| BNQBNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| bnqb rkr
                                                  //| 
                                                  //| (Board 3 ,75)
                                                  //| BNQBNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|      n  
                                                  //| pppppp p
                                                  //| bnqb rkr
                                                  //| 
                                                  //| (BOARD 4 ,75)
                                                  //| BN BNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|      n  
                                                  //| pppppp p
                                                  //| bnqb rkr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,76)
                                                  //| BBRKRNNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|       n 
                                                  //| pppppppp
                                                  //| bbrkr nq
                                                  //| 
                                                  //| (Board 2 ,76)
                                                  //| BBRKRNNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|         
                                                  //|       n 
                                                  //| pppppppp
                                                  //| bbrkr nq
                                                  //| 
                                                  //| (Board 3 ,76)
                                                  //| BBRKRNNQ
                                                  //| 
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrbqnr
                                                  //| 
                                                  //| (Board 2 ,51)
                                                  //| NBRKBQNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //| pppppppp
                                                  //| nbkrbqnr
                                                  //| 
                                                  //| (Board 3 ,51)
                                                  //| NBRKBQNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //| p pppppp
                                                  //| nbkrbqnr
                                                  //| 
                                                  //| (BOARD 4 ,51)
                                                  //| NBRKB NR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| Qp      
                                                  //| p pppppp
                                                  //| nbkrbqnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,52)
                                                  //| NBNQBRKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nbnqbrkr
                                                  //| 
                                                  //| (Board 2 ,52)
                                                  //| NBNQBRKR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nbnqbrkr
                                                  //| 
                                                  //| (Board 3 ,52)
                                                  //| NBNQBRKR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|       p 
                                                  //|         
                                                  //|         
                                                  //| pppppp p
                                                  //| nbnqbrkr
                                                  //| 
                                                  //| (BOARD 4 ,52)
                                                  //| NBN BRKR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|       Q 
                                                  //|         
                                                  //|         
                                                  //| pppppp p
                                                  //| nbnqbrkr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,53)
                                                  //| NBQNRKBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  bqnrkbr
                                                  //| 
                                                  //| (Board 2 ,53)
                                                  //| NBQNRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  bqnrkbr
                                                  //| 
                                                  //| (Board 3 ,53)
                                                  //| NBQNRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n    p 
                                                  //| pppppp p
                                                  //|  bqnrkbr
                                                  //| 
                                                  //| (BOARD 4 ,53)
                                                  //| NB NRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|        P P
                                                  //|  n    pQ
                                                  //| pppppp p
                                                  //|  bqnrkbr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,54)
                                                  //| NBQNBRKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  bqnbrkr
                                                  //| 
                                                  //| (Board 2 ,54)
                                                  //| NBQNBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  bqnbrkr
                                                  //| 
                                                  //| (Board 3 ,54)
                                                  //| NBQNBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|  n      
                                                  //| pppppp p
                                                  //|  bqnbrkr
                                                  //| 
                                                  //| (BOARD 4 ,54)
                                                  //| NB NBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|  n      
                                                  //| pppppp p
                                                  //|  bqnbrkr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,55)
                                                  //| NQRNBKRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqrnbkrb
                                                  //| 
                                                  //| (Board 2 ,55)
                                                  //| NQRNBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqrnbkrb
                                                  //| 
                                                  //| (Board 3 ,55)
                                                  //| NQRNBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqrnb rb
                                                  //|   P   
                                                  //|        P P
                                                  //|   pp            
                                                  //|       
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqbrn
                                                  //| 
                                                  //| (Board 3 ,5)
                                                  //| RNBKQBRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqbrn
                                                  //| 
                                                  //| (BOARD 4 ,5)
                                                  //| RNBK BRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //| Qp      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqbrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,6)
                                                  //| RNBKQBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqbnr
                                                  //| 
                                                  //| (Board 2 ,6)
                                                  //| RNBKQBNR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rnbkqbnr
                                                  //| 
                                                  //| (Board 3 ,6)
                                                  //| RNBKQBNR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqbnr
                                                  //| 
                                                  //| (BOARD 4 ,6)
                                                  //| RNBK BNR
                                                  //| PPP PPPPPP 
                                                  //|    P    
                                                  //|         
                                                  //| Qp      
                                                  //|   p     
                                                  //| p  ppppp
                                                  //| rnbkqbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,7)
                                                  //| RNBQKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| rnbqkbnr
                                                  //| 
                                                  //| (Board 2 ,7)
                                                  //| RNBQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| rnbqkbnr
                                                  //| 
                                                  //| (Board 3 ,7)
                                                  //| RNBQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       p 
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| rnbqkbnr
                                                  //| 
                                                  //| (BOARD 4 ,7)
                                                  //| RNB KBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       pQ
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| rnbqkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,8)
                                                  //| RNQNBBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| rnqnbbkr
                                                  //| 
                                                  //| (Board 2 ,8)
                                                  //| RNQNBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| rnqnbbkr
                                                  //| 
                                                  //| (Board 3 ,8)
                                                  //| RNQNBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| rnqnbb r
                                                  //| 
                                                  //| (BOARD 4 ,8)
                                                  //| RN NBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| rnqnbb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,9)
                                                  //| RNQKBNRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rnqkbnrb
                                                  //| 
                                                  //| (Board 2 ,9)
                                                  //| RNQKBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rnqkbnrb
                                                  //| 
                                                  //| (Board 3 ,9)
                                                  //| RNQKBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      p  
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| rnqkbnrb
                                                  //| 
                                                  //| (BOARD 4 ,9)
                                                  //| RN KBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      pQ 
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| rnqkbnrb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,10)
                                                  //| RBBKRNNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| rbbkrnnq
                                                  //| 
                                                  //| (Board 2 ,10)
                                                  //| RBBKRNNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| rbbkrnnq
                                                  //| 
                                                  //| (Board 3 ,10)
                                                  //| RBBKRNNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| rbb rnnq
                                                  //| 
                                                  //| (BOARD 4 ,10)
                                                  //| RBBKRNN 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| rbb rnnq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,11)
                                                  //| RBKRNQBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rbkrnqbn
                                                  //| 
                                                  //| (Board 2 ,11)
                                                  //| RBKRNQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rbkrnqbn
                                                  //| 
                                                  //| (Board 3 ,11)
                                                  //| RBKRNQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p     p
                                                  //| p ppppp 
                                                  //| rbkrnqbn
                                                  //| 
                                                  //| (BOARD 4 ,11)
                                                  //| RBKRN BN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|       n 
                                                  //| ppp pppp
                                                  //| bbrkr nq
                                                  //| 
                                                  //| (BOARD 4 ,76)
                                                  //| BBRKRNN 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|       n 
                                                  //| ppp pppp
                                                  //| bbrkr nq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,77)
                                                  //| BBQNNRKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| bbqn rkr
                                                  //| 
                                                  //| (Board 2 ,77)
                                                  //| BBQNNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| bbqn rkr
                                                  //| 
                                                  //| (Board 3 ,77)
                                                  //| BBQNNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|      n  
                                                  //| pppppp p
                                                  //| bbqn rkr
                                                  //| 
                                                  //| (BOARD 4 ,77)
                                                  //| BB NNRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|      n  
                                                  //| pppppp p
                                                  //| bbqn rkr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,78)
                                                  //| BQNRKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| bqnrkbnr
                                                  //| 
                                                  //| (Board 2 ,78)
                                                  //| BQNRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| bqnrkbnr
                                                  //| 
                                                  //| (Board 3 ,78)
                                                  //| BQNRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|      p p
                                                  //| ppppp p 
                                                  //| bqnrkbnr
                                                  //| 
                                                  //| (BOARD 4 ,78)
                                                  //| B NRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|      pQp
                                                  //| ppppp p 
                                                  //| bqnrkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,79)
                                                  //| BQNNRKRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| bqnnrkrb
                                                  //| 
                                                  //| (Board 2 ,79)
                                                  //| BQNNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| bqnnrkrb
                                                  //| 
                                                  //| (Board 3 ,79)
                                                  //| BQNNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| bqnnr rb
                                                  //| 
                                                  //| (BOARD 4 ,79)
                                                  //| B NNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| bqnnr rb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,80)
                                                  //| BQNBRKRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| bqnbrkrn
                                                  //| 
                                                  //| (Board 2 ,80)
                                                  //| BQNBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| bqnbrkrn
                                                  //| 
                                                  //| (Board 3 ,80)
                                                  //| BQNBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| bqnbr rn
                                                  //| 
                                                  //| (BOARD 4 ,80)
                                                  //| B NBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| bqnbr rn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,81)
                                                  //| QRNBKRBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qrnbkrbn
                                                  //| 
                                                  //| (Board 2 ,81)
                                                  //| QRNBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qrnbkrbn
                                                  //| 
                                                  //| (Board 3 ,81)
                                                  //| QRNBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qrnb rbn
                                                  //| 
                                                  //| (BOARD 4 ,81)
                                                  //|  RNBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qrnb rbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,82)
                                                  //| QRBBKRNN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qrbbkrnn
                                                  //| 
                                                  //| (Board 2 ,82)
                                                  //| QRBBKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qrbbkrnn
                                                  //| 
                                                  //| (Board 3 ,82)
                                                  //| QRBBKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qrbb rnn
                                                  //| 
                                                  //| (BOARD 4 ,82)
                                                  //|  RBBKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qrbb rnn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,83)
                                                  //| QNRBKRBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnrbkrbn
                                                  //| 
                                                  //| (Board 2 ,83)
                                                  //| QNRBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnrbkrbn
                                                  //| 
                                                  //| (Board 3 ,83)
                                                  //| QNRBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnrb rbn
                                                  //| 
                                                  //| (BOARD 4 ,83)
                                                  //|  NRBKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnrb rbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,84)
                                                  //| QNNRKRBB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnnrkrbb
                                                  //| 
                                                  //| (Board 2 ,84)
                                                  //| QNNRKRBB
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnnrkrbb
                                                  //| 
                                                  //| (Board 3 ,84)
                                                  //| QNNRKRBB
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnnr rbb
                                                  //| 
                                                  //|   
                                                  //| pkpppppp
                                                  //| r bbnqnr
                                                  //| 
                                                  //| (BOARD 4 ,25)
                                                  //| RKBBN NR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bbnqnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,26)
                                                  //| RQNNBKRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rqnnbkrb
                                                  //| 
                                                  //| (Board 2 ,26)
                                                  //| RQNNBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rqnnbkrb
                                                  //| 
                                                  //| (Board 3 ,26)
                                                  //| RQNNBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| rqnnb rb
                                                  //| 
                                                  //| (BOARD 4 ,26)
                                                  //| R NNBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| rqnnb rb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,27)
                                                  //| RQNBBKRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rqnbbkrn
                                                  //| 
                                                  //| (Board 2 ,27)
                                                  //| RQNBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pQp
                                                  //| rqpn b b k r np
                                                  //| 
                                                  //| p ppppp(BOARD 4 ,55)
                                                  //| 
                                                  //| 
                                                  //| (BOARD 4 ,84)
                                                  //| N  R
                                                  //| r bNkNrRnKqRbBnB
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,12)
                                                  //| RBKRBNQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| rbkrbnqn
                                                  //| 
                                                  //| (Board 2 ,12)
                                                  //| RBKRBNQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| rbkrbnqn
                                                  //| 
                                                  //| (Board 3 ,12)
                                                  //| RBKRBNQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| rb rbnqn
                                                  //| 
                                                  //| (BOARD 4 ,12)
                                                  //| RBKRBN N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|         
                                                  //| ppkppppp
                                                  //| rb rbnqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,13)
                                                  //| RBKRBQNN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rbkrbqnn
                                                  //| 
                                                  //| (Board 2 ,13)
                                                  //| RBKRBQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rbkrbqnn
                                                  //| 
                                                  //| (Board 3 ,13)
                                                  //| RBKRBQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|  p     p
                                                  //| p ppppp 
                                                  //| rbkrbqnn
                                                  //| 
                                                  //| (BOARD 4 ,13)
                                                  //| RBKRB NN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //| Qp     p
                                                  //| p ppppp 
                                                  //| rbkrbqnn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,14)
                                                  //| RBQKBNRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rbqkbnrn
                                                  //| 
                                                  //| (Board 2 ,14)
                                                  //| RBQKBNRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| rbqkbnrn
                                                  //| 
                                                  //| (Board 3 ,14)
                                                  //| RBQKBNRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //| NBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| (Board 3 ,27)
                                                  //| RQNBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| rqnbb rn
                                                  //| 
                                                  //| (BOARD 4 ,27)
                                                  //| R NBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| rqnbb rn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,28)
                                                  //| RQKBNRBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| rqkbnrbn
                                                  //| 
                                                  //| (Board 2 ,28)
                                                  //| RQKBNRBN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|    n q r n b  
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| rqkbnrbn
                                                  //| 
                                                  //| (Board 3 ,28)
                                                  //| RQKBNRBN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|     p   
                                                  //|    p    
                                                  //| ppp  ppp
                                                  //| rqkbnrbn
                                                  //| 
                                                  //| (BOARD 4 ,28)
                                                  //| R KBNRBN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|     pQ  
                                                  //|    p    
                                                  //| ppp  ppp
                                                  //| rqkbnrbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,29)
                                                  //| NRNBKRBQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| nrnbkrbq
                                                  //| 
                                                  //| (Board 2 ,29)
                                                  //| NRNBKRBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|  
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnnr rbb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,85)
                                                  //| QNNRKBBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnnrkbbr
                                                  //| 
                                                  //| (Board 2 ,85)
                                                  //| QNNRKBBR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnnrkbbr
                                                  //| 
                                                  //| (Board 3 ,85)
                                                  //| QNNRKBBR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnnr bbr
                                                  //| 
                                                  //| (BOARD 4 ,85)
                                                  //|  NNRKBBR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnnr bbr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,86)
                                                  //| QNBRKRNB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkrnb
                                                  //| 
                                                  //| (Board 2 ,86)
                                                  //| QNBRKRNB
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkrnb
                                                  //| 
                                                  //| (Board 3 ,86)
                                                  //| QNBRKRNB
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr rnb
                                                  //| 
                                                  //| (BOARD 4 ,86)
                                                  //|  NBRKRNB
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr rnb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,87)
                                                  //| QNBRKBRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkbrn
                                                  //| 
                                                  //| (Board 2 ,87)
                                                  //| QNBRKBRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkbrn
                                                  //| 
                                                  //| (Board 3 ,87)
                                                  //| QNBRKBRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr brn
                                                  //| 
                                                  //| (BOARD 4 ,87)
                                                  //|  NBRKBRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr brn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,88)
                                                  //| QNBRKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkbnr
                                                  //| 
                                                  //| (Board 2 ,88)
                                                  //| QNBRKBNR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qnbrkbnr
                                                  //| 
                                                  //| (Board 3 ,88)
                                                  //| QNBRKBNR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr bnr
                                                  //| 
                                                  //| (BOARD 4 ,88)
                                                  //|  NBRKBNR
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qnbr bnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,89)
                                                  //| QBRKBNRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //| pppppp p
                                                  //| qbrkbnrn
                                                  //| 
                                                  //| (Board 2 ,89)
                                                  //| QBRKBNRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //| pppppp p
                                                  //| qbrkbnrn
                                                  //| 
                                                  //| (Board 3 ,89)
                                                  //| QBRKBNRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|         
                                                  //|     p p 
                                                  //| pppp p p
                                                  //| qbrkbnrn
                                                  //| 
                                                  //| (BOARD 4 ,89)
                                                  //|  BRKBNRN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|         
                                                  //|     pQp 
                                                  //| pppp p p
                                                  //| qbrkbnrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,90)
                                                  //| QBNRKRBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qbnrkrbn
                                                  //| 
                                                  //| (Board 2 ,90)
                                                  //| QBNRKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qbnrkrbn
                                                  //| 
                                                  //| (Board 3 ,90)
                                                  //| QBNRKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qbnr rbn
                                                  //| 
                                                  //| (BOARD 4 ,90)
                                                  //|  BNRKRBN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qbnr rbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,91)
                                                  //| QBBRKRNN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qbbrkrnn
                                                  //| 
                                                  //| (Board 2 ,91)
                                                  //| QBBRKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| pppp ppp
                                                  //| qbbrkrnn
                                                  //| 
                                                  //| (Board 3 ,91)
                                                  //| QBBRKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     p   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qbbr rnn
                                                  //| 
                                                  //| (BOARD 4 ,91)
                                                  //|  BBRKRNN
                                                  //| P PPPPPP
                                                  //|  P      
                                                  //|         
                                                  //|     Q   
                                                  //|         
                                                  //| ppppkppp
                                                  //| qbbr rnn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,34)
                                                  //| NRKRBBQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  rkrbbqn
                                                  //| 
                                                  //| (Board 2 ,34)
                                                  //| NRKRBBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  rkrbbqn
                                                  //| 
                                                  //| (Board 3 ,34)
                                                  //| NRKRBBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|  n      
                                                  //| pp ppppp
                                                  //|  rkrbbqn
                                                  //| 
                                                  //| (BOARD 4 ,34)
                                                  //| NRKRBB N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|  n      
                                                  //| pp ppppp
                                                  //|  rkrbbqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,35)
                                                  //| NRKBBRQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| nrkbbrqn
                                                  //| 
                                                  //| (Board 2 ,35)
                                                  //| NRKBBRQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| nrkbbrqn
                                                  //| 
                                                  //| (Board 3 ,35)
                                                  //| NRKBBRQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| nr bbrqn
                                                  //| 
                                                  //| (BOARD 4 ,35)
                                                  //| NRKBBR N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|         
                                                  //| ppkppppp
                                                  //| nr bbrqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,36)
                                                  //| NRKBBNQR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| nrkbbnqr
                                                  //| 
                                                  //| (Board 2 ,36)
                                                  //| NRKBBNQR
                                                  //| P P          
                                                  //|  r b 
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,56)
                                                  //| NQRBBKRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqrbbkrn
                                                  //| 
                                                  //| (Board 2 ,56)
                                                  //| NQRBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqrbbkrn
                                                  //| 
                                                  //| (Board 3 ,56)
                                                  //| NQRBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqrbb rn
                                                  //| 
                                                  //| (BOARD 4 ,56)
                                                  //| N RBBKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqrbb rn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,57)
                                                  //| NQNRBKRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqnrbkrb
                                                  //| 
                                                  //| (Board 2 ,57)
                                                  //| NQNRBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqnrbkrb
                                                  //| 
                                                  //| (Board 3 ,57)
                                                  //| NQNRBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqnrb rb
                                                  //| 
                                                  //| (BOARD 4 ,57)
                                                  //| N NRBKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqnrb rb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,58)
                                                  //| NQNBRKBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqnbrkbr
                                                  //| 
                                                  //| (Board 2 ,58)
                                                  //| NQNBRKBR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqnbrkbr
                                                  //| 
                                                  //| (Board 3 ,58)
                                                  //| NQNBRKBR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqnbr br
                                                  //| 
                                                  //| (BOARD 4 ,58)
                                                  //| N NBRKBR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqnbr br
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,59)
                                                  //| NQBRKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| nqbrkbnr
                                                  //| 
                                                  //| (Board 2 ,59)
                                                  //| NQBRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| nqbrkbnr
                                                  //| 
                                                  //| (Board 3 ,59)
                                                  //| NQBRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|      p p
                                                  //| ppppp p 
                                                  //| nqbrkbnr
                                                  //| 
                                                  //| (BOARD 4 ,59)
                                                  //| N BRKBNR
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|         
                                                  //|      pQp
                                                  //| ppppp p 
                                                  //| nqbrkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,60)
                                                  //| NQBNRKRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqbnrkrb
                                                  //| 
                                                  //| (Board 2 ,60)
                                                  //| NQBNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqbnrkrb
                                                  //| 
                                                  //| (Board 3 ,60)
                                                  //| NQBNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqbnr rb
                                                  //| 
                                                  //| (BOARD 4 ,60)
                                                  //| N BNRKRB
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqbnr rb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,61)
                                                  //| NQBBRKRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqbbrkrn
                                                  //| 
                                                  //| (Board 2 ,61)
                                                  //| NQBBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nqbbrkrn
                                                  //| 
                                                  //| (Board 3 ,61)
                                                  //| NQBBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqbbr rn
                                                  //| 
                                                  //| (BOARD 4 ,61)
                                                  //| N BBRKRN
                                                  //| PP PPPPP
                                                  //|   P     
                                                  //|         
                                                  //|      Q  
                                                  //|         
                                                  //| pppppkpp
                                                  //| nqbbr rn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,62)
                                                  //| BRNBKQRN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| brnbkqrn
                                                  //| 
                                                  //| (Board 2 ,62)
                                                  //| BRNBKQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| brnbkqrn
                                                  //| 
                                                  //| (Board 3 ,62)
                                                  //| BRNBKQRN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|   p     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| brnbkqrn
                                                  //| 
                                                  //| (BOARD 4 ,62)
                                                  //| BRNBK RN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Qp     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| brnbkqrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,63)
                                                  //| BRNBKQNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| brnbkqnr
                                                  //| 
                                                  //| (Board 2 ,63)
                                                  //| BRNBKQNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| brnbkqnr
                                                  //| 
                                                  //| (Board 3 ,63)
                                                  //| BRNBKQNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|   p     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| brnbkqnr
                                                  //| 
                                                  //| (BOARD 4 ,63)
                                                  //| BRNBK NR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Qp     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| brnbkqnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,64)
                                                  //| BRNQKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| brnqkbnr
                                                  //| 
                                                  //| (Board 2 ,64)
                                                  //| BRNQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| brnqkbnr
                                                  //| 
                                                  //| (Board 3 ,64)
                                                  //| BRNQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       p 
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| brnqkbnr
                                                  //| 
                                                  //| (BOARD 4 ,64)
                                                  //| BRN KBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       pQ
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| brnqkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,65)
                                                  //| BRKRNNQB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| brkr nqb
                                                  //| 
                                                  //| (Board 2 ,65)
                                                  //| BRKRNNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| brkr nqb
                                                  //| 
                                                  //| (Board 3 ,65)
                                                  //| BRKRNNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|      n  
                                                  //| pp ppppp
                                                  //| brkr nqb
                                                  //| 
                                                  //| (BOARD 4 ,65)
                                                  //| BRKRNN B
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|      n  
                                                  //| pp ppppp
                                                  //| brkr nqb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,66)
                                                  //| BRKRNBQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| brkr bqn
                                                  //| 
                                                  //| (Board 2 ,66)
                                                  //| BRKRNBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //|      n  
                                                  //| pppppppp
                                                  //| brkr bqn
                                                  //| 
                                                  //| (Board 3 ,66)
                                                  //| BRKRNBQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|      n  
                                                  //| pp ppppp
                                                  //| brkr bqn
                                                  //| 
                                                  //| (BOARD 4 ,66)
                                                  //| BRKRNB N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|      n  
                                                  //| pp ppppp
                                                  //| brkr bqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,67)
                                                  //| BRKBRNQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| brkbrnqn
                                                  //| 
                                                  //| (Board 2 ,67)
                                                  //| BRKBRNQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| brkbrnqn
                                                  //| 
                                                  //| (Board 3 ,67)
                                                  //| BRKBRNQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| br brnqn
                                                  //| 
                                                  //| (BOARD 4 ,67)
                                                  //| BRKBRN N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|         
                                                  //| ppkppppp
                                                  //| br brnqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,68)
                                                  //| BRKBNRQN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| brkbnrqn
                                                  //| 
                                                  //| (Board 2 ,68)
                                                  //| BRKBNRQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| brkbnrqn
                                                  //| 
                                                  //| (Board 3 ,68)
                                                  //| BRKBNRQN
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| br bnrqn
                                                  //| 
                                                  //| (BOARD 4 ,68)
                                                  //| BRKBNR N
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|         
                                                  //| ppkppppp
                                                  //| br bnrqn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,40)
                                                  //| NNRKBBRQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nnrkbbrq
                                                  //| 
                                                  //| (Board 2 ,40)
                                                  //| NNRKBBRQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nnrkbbrq
                                                  //| 
                                                  //| (Board 3 ,40)
                                                  //| NNRKBBRQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nnr bbrq
                                                  //| 
                                                  //| (BOARD 4 ,40)
                                                  //| NNRKBBR 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nnr bbrq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,41)
                                                  //| NNQRBBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nnqrbbkr
                                                  //| 
                                                  //| (Board 2 ,41)
                                                  //| NNQRBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nnqrbbkr
                                                  //| 
                                                  //| (Board 3 ,41)
                                                  //| NNQRBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| nnqrbb r
                                                  //| 
                                                  //| (BOARD 4 ,41)
                                                  //| NN RBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| nnqrbb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,42)
                                                  //| NNQBRKBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  nqbrkbr
                                                  //| 
                                                  //| (Board 2 ,42)
                                                  //| NNQBRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  nqbrkbr
                                                  //| 
                                                  //| (Board 3 ,42)
                                                  //| NNQBRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n    p 
                                                  //| pppppp p
                                                  //|  nqbrkbr
                                                  //| 
                                                  //| (BOARD 4 ,42)
                                                  //| NN BRKBR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n    pQ
                                                  //| pppppp p
                                                  //|  nqbrkbr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,43)
                                                  //| NNQBBRKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  nqbbrkr
                                                  //| 
                                                  //| (Board 2 ,43)
                                                  //| NNQBBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  nqbbrkr
                                                  //| 
                                                  //| (Board 3 ,43)
                                                  //| NNQBBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|  n      
                                                  //| pppppp p
                                                  //|  nqbbrkr
                                                  //| 
                                                  //| (BOARD 4 ,43)
                                                  //| NN BBRKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|  n      
                                                  //| pppppp p
                                                  //|  nqbbrkr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,44)
                                                  //| NBRNBKQR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrnbkqr
                                                  //| 
                                                  //| (Board 2 ,44)
                                                  //| NBRNBKQR
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nbrnbkqr
                                                  //| 
                                                  //| (Board 3 ,44)
                                                  //| NBRNBKQR
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|    p    
                                                  //|     p   
                                                  //| ppp  ppp
                                                  //| nbrnbkqr
                                                  //| 
                                                  //| (BOARD 4 ,44)
                                                  //| NBRNBK R
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Qp    
                                                  //|     p   
                                                  //| ppp  ppp
                                                  //| nbrnbkqr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,45)
                                                  //| NBRKRNBQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|       n 
                                                  //| pppppppp
                                                  //| nbrkr bq
                                                  //| 
                                                  //| (Board 2 ,45)
                                                  //| NBRKRNBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|         
                                                  //|       n 
                                                  //| pppppppp
                                                  //| nbrkr bq
                                                  //| 
                                                  //| (Board 3 ,45)
                                                  //| NBRKRNBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|       n 
                                                  //| ppp pppp
                                                  //| nbrkr bq
                                                  //| 
                                                  //| (BOARD 4 ,45)
                                                  //| NBRKRNB 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|       n 
                                                  //| ppp pppp
                                                  //| nbrkr bq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,37)
                                                  //| NRQNBBKR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nrqnbbkr
                                                  //| 
                                                  //| (Board 2 ,37)
                                                  //| NRQNBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| pppppp p
                                                  //| nrqnbbkr
                                                  //| 
                                                  //| (Board 3 ,37)
                                                  //| NRQNBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       p 
                                                  //|         
                                                  //| ppppppkp
                                                  //| nrqnbb r
                                                  //| 
                                                  //| (BOARD 4 ,37)
                                                  //| NR NBBKR
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|       Q 
                                                  //|         
                                                  //| ppppppkp
                                                  //| nrqnbb r
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,38)
                                                  //| NRQKBNRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nrqkbnrb
                                                  //| 
                                                  //| (Board 2 ,38)
                                                  //| NRQKBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      p  
                                                  //|         
                                                  //| ppppp pp
                                                  //| nrqkbnrb
                                                  //| 
                                                  //| (Board 3 ,38)
                                                  //| NRQKBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      p  
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| nrqkbnrb
                                                  //| 
                                                  //| (BOARD 4 ,38)
                                                  //| NR KBNRB
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      pQ 
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| nrqkbnrb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,39)
                                                  //| NNRKRBBQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nnrkrbbq
                                                  //| 
                                                  //| (Board 2 ,39)
                                                  //| NNRKRBBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nnrkrbbq
                                                  //| 
                                                  //| (Board 3 ,39)
                                                  //| NNRKRBBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nnr rbbq
                                                  //| 
                                                  //| (BOARD 4 ,39)
                                                  //| NNRKRBB 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nnr rbbq
                                                  //| 
                                                  //| 
                                                  //| p    
                                                  //| ppp pppp
                                                  //| nrnbkrbq
                                                  //| 
                                                  //| (Board 3 ,29)
                                                  //| NRNBKRBQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|         
                                                  //|  p p    
                                                  //| p p pppp
                                                  //| nrnbkrbq
                                                  //| 
                                                  //| (BOARD 4 ,29)
                                                  //| NRNBKRB 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|         
                                                  //|  pQp    
                                                  //| p p pppp
                                                  //| nrnbkrbq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,30)
                                                  //| NRNBKQBR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| nrnbkqbr
                                                  //| 
                                                  //| (Board 2 ,30)
                                                  //| NRNBKQBR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //| ppp pppp
                                                  //| nrnbkqbr
                                                  //| 
                                                  //| (Board 3 ,30)
                                                  //| NRNBKQBR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|   p     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| nrnbkqbr
                                                  //| 
                                                  //| (BOARD 4 ,30)
                                                  //| NRNBK BR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Qp     
                                                  //|    p    
                                                  //| pp  pppp
                                                  //| nrnbkqbr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,31)
                                                  //| NRBKRBNQ
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nrbkrbnq
                                                  //| 
                                                  //| (Board 2 ,31)
                                                  //| NRBKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| ppp pppp
                                                  //| nrbkrbnq
                                                  //| 
                                                  //| (Board 3 ,31)
                                                  //| NRBKRBNQ
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    p    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nrb rbnq
                                                  //| 
                                                  //| (BOARD 4 ,31)
                                                  //| NRBKRBN 
                                                  //| PPPPPP P
                                                  //|       P 
                                                  //|         
                                                  //|    Q    
                                                  //|         
                                                  //| pppkpppp
                                                  //| nrb rbnq
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,32)
                                                  //| NRBQKBNR
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| nrbqkbnr
                                                  //| 
                                                  //| (Board 2 ,32)
                                                  //| NRBQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|      p  
                                                  //| ppppp pp
                                                  //| nrbqkbnr
                                                  //| 
                                                  //| (Board 3 ,32)
                                                  //| NRBQKBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       p 
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| nrbqkbnr
                                                  //| 
                                                  //| (BOARD 4 ,32)
                                                  //| NRB KBNR
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|       pQ
                                                  //|      p  
                                                  //| ppppp  p
                                                  //| nrbqkbnr
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,33)
                                                  //| NRKRBNQB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  rkrbnqb
                                                  //| 
                                                  //| (Board 2 ,33)
                                                  //| NRKRBNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|         
                                                  //|  n      
                                                  //| pppppppp
                                                  //|  rkrbnqb
                                                  //| 
                                                  //| (Board 3 ,33)
                                                  //| NRKRBNQB
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|  n      
                                                  //| pp ppppp
                                                  //|  rkrbnqb
                                                  //| 
                                                  //| (BOARD 4 ,33)
                                                  //| NRKRBN B
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|  n      
                                                  //| pp ppppp
                                                  //|  rkrbnqb
                                                  //| 
                                                  //| 
                                                  //| P    
                                                  //|      p  
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| rbqkbnrn
                                                  //| 
                                                  //| (BOARD 4 ,14)
                                                  //| RB KBNRN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|      pQ 
                                                  //|     p   
                                                  //| pppp  pp
                                                  //| rbqkbnrn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,15)
                                                  //| RKRNNQBB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|     n   
                                                  //| pppppppp
                                                  //| rkr nqbb
                                                  //| 
                                                  //| (Board 2 ,15)
                                                  //| RKRNNQBB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|     n   
                                                  //| pppppppp
                                                  //| rkr nqbb
                                                  //| 
                                                  //| (Board 3 ,15)
                                                  //| RKRNNQBB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|     n   
                                                  //| p pppppp
                                                  //| rkr nqbb
                                                  //| 
                                                  //| (BOARD 4 ,15)
                                                  //| RKRNN BB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|     n   
                                                  //| p pppppp
                                                  //| rkr nqbb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,16)
                                                  //| RKRNBQNB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|     n   
                                                  //| pppppppp
                                                  //| rkr bqnb
                                                  //| 
                                                  //| (Board 2 ,16)
                                                  //| RKRNBQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|     n   
                                                  //| pppppppp
                                                  //| rkr bqnb
                                                  //| 
                                                  //| (Board 3 ,16)
                                                  //| RKRNBQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|     n   
                                                  //| p pppppp
                                                  //| rkr bqnb
                                                  //| 
                                                  //| (BOARD 4 ,16)
                                                  //| RKRNB NB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|     n   
                                                  //| p pppppp
                                                  //| rkr bqnb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,17)
                                                  //| RKRBNQBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rkrbnqbn
                                                  //| 
                                                  //| (Board 2 ,17)
                                                  //| RKRBNQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rkrbnqbn
                                                  //| 
                                                  //| (Board 3 ,17)
                                                  //| RKRBNQBN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|        p
                                                  //| p ppppp 
                                                  //| rkrbnqbn
                                                  //| 
                                                  //| (BOARD 4 ,17)
                                                  //| RKRBN BN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|        p
                                                  //| p ppppp 
                                                  //| rkrbnqbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,18)
                                                  //| RKRBBQNN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rkrbbqnn
                                                  //| 
                                                  //| (Board 2 ,18)
                                                  //| RKRBBQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|         
                                                  //|        p
                                                  //| ppppppp 
                                                  //| rkrbbqnn
                                                  //| 
                                                  //| (Board 3 ,18)
                                                  //| RKRBBQNN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|        p
                                                  //| p ppppp 
                                                  //| rkrbbqnn
                                                  //| 
                                                  //| (BOARD 4 ,18)
                                                  //| RKRBB NN
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|        p
                                                  //| p ppppp 
                                                  //| rkrbbqnn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,19)
                                                  //| RKRBQNBN
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkrbqnbn
                                                  //| 
                                                  //| (Board 2 ,19)
                                                  //| RKRBQNBN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkrbqnbn
                                                  //| 
                                                  //| (Board 3 ,19)
                                                  //| RKRBQNBN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|  p      
                                                  //|         
                                                  //|         
                                                  //| p pppppp
                                                  //| rkrbqnbn
                                                  //| 
                                                  //| (BOARD 4 ,19)
                                                  //| RKRB NBN
                                                  //| PPP PPPP
                                                  //|    P    
                                                  //|  Q      
                                                  //|         
                                                  //|         
                                                  //| p pppppp
                                                  //| rkrbqnbn
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,20)
                                                  //| RKBRNQNB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbrnqnb
                                                  //| 
                                                  //| (Board 2 ,20)
                                                  //| RKBRNQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbrnqnb
                                                  //| 
                                                  //| (Board 3 ,20)
                                                  //| RKBRNQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r brnqnb
                                                  //| 
                                                  //| (BOARD 4 ,20)
                                                  //| RKBRN NB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r brnqnb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,21)
                                                  //| RKBNRQNB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbnrqnb
                                                  //| 
                                                  //| (Board 2 ,21)
                                                  //| RKBNRQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbnrqnb
                                                  //| 
                                                  //| (Board 3 ,21)
                                                  //| RKBNRQNB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bnrqnb
                                                  //| 
                                                  //| (BOARD 4 ,21)
                                                  //| RKBNR NB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bnrqnb
                                                  //| 
                                                  //| 
                                                  //| (Board 1 ,22)
                                                  //| RKBNNQRB
                                                  //| PPPPPPPP
                                                  //|         
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbnnqrb
                                                  //| 
                                                  //| (Board 2 ,22)
                                                  //| RKBNNQRB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| p pppppp
                                                  //| rkbnnqrb
                                                  //| 
                                                  //| (Board 3 ,22)
                                                  //| RKBNNQRB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  p      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bnnqrb
                                                  //| 
                                                  //| (BOARD 4 ,22)
                                                  //| RKBNN RB
                                                  //| PPPP PPP
                                                  //|     P   
                                                  //|         
                                                  //|  Q      
                                                  //|         
                                                  //| pkpppppp
                                                  //| r bnnqrb
                                                  //| 
                                                  //| 
                                                  //| PP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| pp ppppp
                                                  //| nrkbbnqr
                                                  //| 
                                                  //| (Board 3 ,36)
                                                  //| NRKBBNQR
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   p     
                                                  //|         
                                                  //| ppkppppp
                                                  //| nr bbnqr
                                                  //| 
                                                  //| (BOARD 4 ,36)
                                                  //| NRKBBN R
                                                  //| PPPPP PP
                                                  //|      P  
                                                  //|         
                                                  //|   Q     
                                                  //|         
                                                  //| ppkppppp
                                                  //| nr bbnqr
                                                  //| 
                                                  //| 
  val numSolutionsViaBruteForce = solutionViaBruteForce.size
                                                  //> numSolutionsViaBruteForce  : Int = 92

  
  /* Sitting with a board in front of you, you can see that there's only a handful of checking moves.
Black pawns are irrelevant. One move is necessarily wasted to allow a piece to move. Black's knights
have no hope of checkmating. Black's roots take too many moves to get out from behind the pawns. Black's king,
would need like 8 moves to help out in a checkmate.
Black can only checkmate with its Queen or Bishops.
If Black takes a pawn on rank 2, white's king can capture it, avoiding checkmate.
Black can move their queen or bishop to the 3rd or 4th ranks, or their queen to the 5th or 6th rank.
Bishop attacks from the 5th rank or beyong is useless, since white has a minimum of 3 pawns
that can block. Black's queen can capture white's king's pawn on rank 4 or 5 to get a frontal assault.
The queen's frontal assault from rank 3 can not succeed since the king's pawn must not block, and its two neighbours
must move to not attack black's queen.
The queen can almost achieve a frontal assault all the way from rank 6! White's king's pawn captures black's pawn on move 2,
but then that pawn can take black's queen if it goes for the checkmate.
Diagonal attacks are equally possible with Black's Queen and Bishop.
Black's must move diagonal out of rank 8 to get to either rank 3 or 4.
If Black moves to a3, there is only 1 white pawn that can needs to get out of the way. Note that a knight on d1
can't simply move to e3 to avoid the block, it would allow an escape route for the king.
*/

  def isCheckMatable(board: List[Char]): Boolean = {
  // Frontal attack is always possible if white's king moves to the second rank, only knights can block
  (Math.abs(board.indexOf('K') - board.indexOf('Q')) == 4 &&
   {
   val k = board.indexOf('K')
   board(k-1) != 'N' && board(k+1) != 'N'
   }) ||
  (board.indexOf('K') match {
    // The king on a or h is impossible, as the King be between 2 rooks.
    case 0 => false
    case 7 => false
    // abcdefgh
    // 01234567
    // King on b or g file, diagonal attacks are impossible. If king is on b, then by the
    // rules of Fisher, a rook must be on a.
    // Black's rook must also be on a. But a black Q or B must be on a to attack the white King.
    // A frontal attack is possible though, with the queen going to rank 4 or 5.
    case 1 => board(2) == 'R' && (board(5) == 'Q' || (board(4) =='Q' && board(3) != 'N'))
    case 6 => board(5) == 'R' && (board(2) == 'Q' || (board(3) =='Q' && board(4) != 'N'))

    // For the king on c,d,e,f
    case k if 2 to 5 contains k =>
     // Frontal attack from Black's queen to King's Pawn on the 5th rank, making the
     // Queen 4 or 3 over from the king.
     // King's neighbours must be rooks (B and N can block), and two doors down must not be the knights
     (3 == Math.abs(k - board.indexOf('Q'))) &&
       board(k - 1) == 'R' && board(k + 1) == 'R' && board(k - 2) != 'N' && board(k + 2) != 'N' ||
     // Left diagonal attack to the third rank (not possible for king on f)
     ((k <= 4) && ((board(k + 3) == 'Q' || board(k + 3) == 'B')) && board(k - 1) == 'B' && board(k + 1) != 'N' &&
      board(k - 2) != 'B' && board(k - 2) != 'Q' &&
      (if (k >= 3) board(k - 3) != 'N' else true) &&
      (if (k >= 4) board(k - 4) != 'Q' && board(k - 4) != 'B' else true)) ||
     // Left diagonal attack to the fourth rank
     ((board(k + 1) == 'Q' || board(k + 1) == 'B') && board(k - 1) == 'B' && board(k + 1) != 'N' &&
      board(k - 2) == 'N' &&
      (if (k >= 3) board(k - 3) != 'N' else true)) ||
     // Right diagonal to the third rank
     ((k >= 3) && ((board(k - 3) == 'Q' || board(k - 3) == 'B')) && board(k + 1) == 'B' && board(k - 1) != 'N' &&
      board(k + 2) != 'B' && board(k + 2) != 'Q' &&
      (if (k <= 4) board(k + 3) != 'N' else true) &&
      (if (k <= 3) board(k + 4) != 'Q' && board(k + 4) != 'B' else true)) ||
     // Right diagonal to the fourth rank
     ((board(k - 1) == 'Q' || board(k - 1) == 'B') && board(k + 1) == 'B' && board(k - 1) != 'N' &&
      board(k + 2) == 'N' &&
      (if (k <= 4) board(k + 3) != 'N' else true)) ||
     // Castle queenside left diagonal is possible, right diagonal isn't.
     (k == 3 && board(2) == 'R' &&
      // "Move" king to the c file, board(2).
      ((board(2 + 3) == 'Q' || board(2 + 3) == 'B') && board(2 - 1) == 'B' && board(2 + 1) != 'N' && // Board 3 is the K
       board(2 - 2) != 'B' && board(2 - 2) != 'Q' &&
       (if (2 >= 3) board(2 - 3) != 'N' else true)))
   }) // No knight off the board to the left.
  }                                               //> isCheckMatable: (board: List[Char])Boolean

  val allSolutions = allStartingPositions filter isCheckMatable
                                                  //> allSolutions  : List[List[Char]] = List(List(R, N, B, K, R, N, Q, B), List
                                                  //| (R, N, B, K, R, B, N, Q), List(R, N, B, K, R, B, Q, N), List(R, N, B, K, Q
                                                  //| , R, N, B), List(R, N, B, K, Q, N, R, B), List(R, N, B, K, Q, B, R, N), Li
                                                  //| st(R, N, B, K, Q, B, N, R), List(R, N, B, Q, K, B, N, R), List(R, N, Q, N,
                                                  //|  B, B, K, R), List(R, N, Q, K, B, N, R, B), List(R, B, B, K, R, N, N, Q), 
                                                  //| List(R, B, K, R, N, Q, B, N), List(R, B, K, R, B, N, Q, N), List(R, B, K, 
                                                  //| R, B, Q, N, N), List(R, B, Q, K, B, N, R, N), List(R, K, R, N, N, Q, B, B)
                                                  //| , List(R, K, R, N, B, Q, N, B), List(R, K, R, B, N, Q, B, N), List(R, K, R
                                                  //| , B, B, Q, N, N), List(R, K, R, B, Q, N, B, N), List(R, K, B, R, N, Q, N, 
                                                  //| B), List(R, K, B, N, R, Q, N, B), List(R, K, B, N, N, Q, R, B), List(R, K,
                                                  //|  B, B, R, Q, N, N), List(R, K, B, B, N, Q, R, N), List(R, K, B, B, N, Q, N
                                                  //| , R), List(R, Q, N, N, B, K, R, B), List(R, Q, N, B, B, K, R, N), List(R, 
                                                  //| Q, K, B, N, R, B, N), List(N, R, N, B, K, R, B, Q), List(N, R, N, B, K, Q,
                                                  //|  B, R), List(N, R, B, K, R, B, N, Q), List(N, R, B, Q, K, B, N, R), List(N
                                                  //| , R, K, R, B, N, Q, B), List(N, R, K, R, B, B, Q, N), List(N, R, K, B, B, 
                                                  //| R, Q, N), List(N, R, K, B, B, N, Q, R), List(N, R, Q, N, B, B, K, R), List
                                                  //| (N, R, Q, K, B, N, R, B), List(N, N, R, K, R, B, B, Q), List(N, N, R, K, B
                                                  //| , B, R, Q), List(N, N, Q, R, B, B, K, R), List(N, N, Q, B, R, K, B, R), Li
                                                  //| st(N, N, Q, B, B, R, K, R), List(N, B, R, N, B, K, Q, R), List(N, B, R, K,
                                                  //|  R, N, B, Q), List(N, B, R, K, R, Q, B, N), List(N, B, R, K, N, Q, B, R), 
                                                  //| List(N, B, R, K, B, R, N, Q), List(N, B, R, K, B, N, R, Q), List(N, B, R, 
                                                  //| K, B, Q, R, N), List(N, B, R, K, B, Q, N, R), List(N, B, N, Q, B, R, K, R)
                                                  //| , List(N, B, Q, N, R, K, B, R), List(N, B, Q, N, B, R, K, R), List(N, Q, R
                                                  //| , N, B, K, R, B), List(N, Q, R, B, B, K, R, N), List(N, Q, N, R, B, K, R, 
                                                  //| B), List(N, Q, N, B, R, K, B, R), List(N, Q, B, R, K, B, N, R), List(N, Q,
                                                  //|  B, N, R, K, R, B), List(N, Q, B, B, R, K, R, N), List(B, R, N, B, K, Q, R
                                                  //| , N), List(B, R, N, B, K, Q, N, R), List(B, R, N, Q, K, B, N, R), List(B, 
                                                  //| R, K, R, N, N, Q, B), List(B, R, K, R, N, B, Q, N), List(B, R, K, B, R, N,
                                                  //|  Q, N), List(B, R, K, B, N, R, Q, N), List(B, R, K, B, N, N, Q, R), List(B
                                                  //| , R, Q, N, N, B, K, R), List(B, N, R, K, R, B, N, Q), List(B, N, R, Q, K, 
                                                  //| B, N, R), List(B, N, Q, R, N, B, K, R), List(B, N, Q, N, R, B, K, R), List
                                                  //| (B, N, Q, B, N, R, K, R), List(B, B, R, K, R, N, N, Q), List(B, B, Q, N, N
                                                  //| , R, K, R), List(B, Q, N, R, K, B, N, R), List(B, Q, N, N, R, K, R, B), Li
                                                  //| st(B, Q, N, B, R, K, R, N), List(Q, R, N, B, K, R, B, N), List(Q, R, B, B,
                                                  //|  K, R, N, N), List(Q, N, R, B, K, R, B, N), List(Q, N, N, R, K, R, B, B), 
                                                  //| List(Q, N, N, R, K, B, B, R), List(Q, N, B, R, K, R, N, B), List(Q, N, B, 
                                                  //| R, K, B, R, N), List(Q, N, B, R, K, B, N, R), List(Q, B, R, K, B, N, R, N)
                                                  //| , List(Q, B, N, R, K, R, B, N), List(Q, B, B, R, K, R, N, N))
  allSolutions foreach println                    //> List(R, N, B, K, R, N, Q, B)
                                                  //| List(R, N, B, K, R, B, N, Q)
                                                  //| List(R, N, B, K, R, B, Q, N)
                                                  //| List(R, N, B, K, Q, R, N, B)
                                                  //| List(R, N, B, K, Q, N, R, B)
                                                  //| List(R, N, B, K, Q, B, R, N)
                                                  //| List(R, N, B, K, Q, B, N, R)
                                                  //| List(R, N, B, Q, K, B, N, R)
                                                  //| List(R, N, Q, N, B, B, K, R)
                                                  //| List(R, N, Q, K, B, N, R, B)
                                                  //| List(R, B, B, K, R, N, N, Q)
                                                  //| List(R, B, K, R, N, Q, B, N)
                                                  //| List(R, B, K, R, B, N, Q, N)
                                                  //| List(R, B, K, R, B, Q, N, N)
                                                  //| List(R, B, Q, K, B, N, R, N)
                                                  //| List(R, K, R, N, N, Q, B, B)
                                                  //| List(R, K, R, N, B, Q, N, B)
                                                  //| List(R, K, R, B, N, Q, B, N)
                                                  //| List(R, K, R, B, B, Q, N, N)
                                                  //| List(R, K, R, B, Q, N, B, N)
                                                  //| List(R, K, B, R, N, Q, N, B)
                                                  //| List(R, K, B, N, R, Q, N, B)
                                                  //| List(R, K, B, N, N, Q, R, B)
                                                  //| List(R, K, B, B, R, Q, N, N)
                                                  //| List(R, K, B, B, N, Q, R, N)
                                                  //| List(R, K, B, B, N, Q, N, R)
                                                  //| List(R, Q, N, N, B, K, R, B)
                                                  //| List(R, Q, N, B, B, K, R, N)
                                                  //| List(R, Q, K, B, N, R, B, N)
                                                  //| List(N, R, N, B, K, R, B, Q)
                                                  //| List(N, R, N, B, K, Q, B, R)
                                                  //| List(N, R, B, K, R, B, N, Q)
                                                  //| List(N, R, B, Q, K, B, N, R)
                                                  //| List(N, R, K, R, B, N, Q, B)
                                                  //| List(N, R, K, R, B, B, Q, N)
                                                  //| List(N, R, K, B, B, R, Q, N)
                                                  //| List(N, R, K, B, B, N, Q, R)
                                                  //| List(N, R, Q, N, B, B, K, R)
                                                  //| List(N, R, Q, K, B, N, R, B)
                                                  //| List(N, N, R, K, R, B, B, Q)
                                                  //| List(N, N, R, K, B, B, R, Q)
                                                  //| List(N, N, Q, R, B, B, K, R)
                                                  //| List(N, N, Q, B, R, K, B, R)
                                                  //| List(N, N, Q, B, B, R, K, R)
                                                  //| List(N, B, R, N, B, K, Q, R)
                                                  //| List(N, B, R, K, R, N, B, Q)
                                                  //| List(N, B, R, K, R, Q, B, N)
                                                  //| List(N, B, R, K, N, Q, B, R)
                                                  //| List(N, B, R, K, B, R, N, Q)
                                                  //| List(N, B, R, K, B, N, R, Q)
                                                  //| List(N, B, R, K, B, Q, R, N)
                                                  //| List(N, B, R, K, B, Q, N, R)
                                                  //| List(N, B, N, Q, B, R, K, R)
                                                  //| List(N, B, Q, N, R, K, B, R)
                                                  //| List(N, B, Q, N, B, R, K, R)
                                                  //| List(N, Q, R, N, B, K, R, B)
                                                  //| List(N, Q, R, B, B, K, R, N)
                                                  //| List(N, Q, N, R, B, K, R, B)
                                                  //| List(N, Q, N, B, R, K, B, R)
                                                  //| List(N, Q, B, R, K, B, N, R)
                                                  //| List(N, Q, B, N, R, K, R, B)
                                                  //| List(N, Q, B, B, R, K, R, N)
                                                  //| List(B, R, N, B, K, Q, R, N)
                                                  //| List(B, R, N, B, K, Q, N, R)
                                                  //| List(B, R, N, Q, K, B, N, R)
                                                  //| List(B, R, K, R, N, N, Q, B)
                                                  //| List(B, R, K, R, N, B, Q, N)
                                                  //| List(B, R, K, B, R, N, Q, N)
                                                  //| List(B, R, K, B, N, R, Q, N)
                                                  //| List(B, R, K, B, N, N, Q, R)
                                                  //| List(B, R, Q, N, N, B, K, R)
                                                  //| List(B, N, R, K, R, B, N, Q)
                                                  //| List(B, N, R, Q, K, B, N, R)
                                                  //| List(B, N, Q, R, N, B, K, R)
                                                  //| List(B, N, Q, N, R, B, K, R)
                                                  //| List(B, N, Q, B, N, R, K, R)
                                                  //| List(B, B, R, K, R, N, N, Q)
                                                  //| List(B, B, Q, N, N, R, K, R)
                                                  //| List(B, Q, N, R, K, B, N, R)
                                                  //| List(B, Q, N, N, R, K, R, B)
                                                  //| List(B, Q, N, B, R, K, R, N)
                                                  //| List(Q, R, N, B, K, R, B, N)
                                                  //| List(Q, R, B, B, K, R, N, N)
                                                  //| List(Q, N, R, B, K, R, B, N)
                                                  //| List(Q, N, N, R, K, R, B, B)
                                                  //| List(Q, N, N, R, K, B, B, R)
                                                  //| List(Q, N, B, R, K, R, N, B)
                                                  //| List(Q, N, B, R, K, B, R, N)
                                                  //| List(Q, N, B, R, K, B, N, R)
                                                  //| List(Q, B, R, K, B, N, R, N)
                                                  //| List(Q, B, N, R, K, R, B, N)
                                                  //| List(Q, B, B, R, K, R, N, N)
  allSolutions.size                               //> res1: Int = 92
  solutionViaBruteForce foreach ( sb => if (!(allSolutions contains sb._5)) println(sb._5) )
                                                   
  allSolutions foreach (sol => if (!(solutionViaBruteForce.map(_._5).toList contains sol)) println(sol) )
     
  (allStartingPositions diff allSolutions) foreach println
                                                  //> List(R, N, N, B, B, K, R, Q)
                                                  //| List(R, N, N, B, B, K, Q, R)
                                                  //| List(R, N, N, B, B, Q, K, R)
                                                  //| List(R, N, N, B, K, R, B, Q)
                                                  //| List(R, N, N, B, K, Q, B, R)
                                                  //| List(R, N, N, B, Q, K, B, R)
                                                  //| List(R, N, N, K, R, B, B, Q)
                                                  //| List(R, N, N, K, R, Q, B, B)
                                                  //| List(R, N, N, K, B, R, Q, B)
                                                  //| List(R, N, N, K, B, B, R, Q)
                                                  //| List(R, N, N, K, B, B, Q, R)
                                                  //| List(R, N, N, K, B, Q, R, B)
                                                  //| List(R, N, N, K, Q, R, B, B)
                                                  //| List(R, N, N, K, Q, B, B, R)
                                                  //| List(R, N, N, Q, B, B, K, R)
                                                  //| List(R, N, N, Q, B, K, R, B)
                                                  //| List(R, N, N, Q, K, R, B, B)
                                                  //| List(R, N, N, Q, K, B, B, R)
                                                  //| List(R, N, B, N, K, R, Q, B)
                                                  //| List(R, N, B, N, K, B, R, Q)
                                                  //| List(R, N, B, N, K, B, Q, R)
                                                  //| List(R, N, B, N, K, Q, R, B)
                                                  //| List(R, N, B, N, Q, B, K, R)
                                                  //| List(R, N, B, N, Q, K, R, B)
                                                  //| List(R, N, B, B, N, K, R, Q)
                                                  //| List(R, N, B, B, N, K, Q, R)
                                                  //| List(R, N, B, B, N, Q, K, R)
                                                  //| List(R, N, B, B, K, R, N, Q)
                                                  //| List(R, N, B, B, K, R, Q, N)
                                                  //| List(R, N, B, B, K, N, R, Q)
                                                  //| List(R, N, B, B, K, N, Q, R)
                                                  //| List(R, N, B, B, K, Q, R, N)
                                                  //| List(R, N, B, B, K, Q, N, R)
                                                  //| List(R, N, B, B, Q, N, K, R)
                                                  //| List(R, N, B, B, Q, K, R, N)
                                                  //| List(R, N, B, B, Q, K, N, R)
                                                  //| List(R, N, B, K, R, Q, N, B)
                                                  //| List(R, N, B, K, N, R, Q, B)
                                                  //| List(R, N, B, K, N, B, R, Q)
                                                  //| List(R, N, B, K, N, B, Q, R)
                                                  //| List(R, N, B, K, N, Q, R, B)
                                                  //| List(R, N, B, Q, N, B, K, R)
                                                  //| List(R, N, B, Q, N, K, R, B)
                                                  //| List(R, N, B, Q, K, R, N, B)
                                                  //| List(R, N, B, Q, K, N, R, B)
                                                  //| List(R, N, B, Q, K, B, R, N)
                                                  //| List(R, N, K, R, N, B, B, Q)
                                                  //| List(R, N, K, R, N, Q, B, B)
                                                  //| List(R, N, K, R, B, N, Q, B)
                                                  //| List(R, N, K, R, B, B, N, Q)
                                                  //| List(R, N, K, R, B, B, Q, N)
                                                  //| List(R, N, K, R, B, Q, N, B)
                                                  //| List(R, N, K, R, Q, N, B, B)
                                                  //| List(R, N, K, R, Q, B, B, N)
                                                  //| List(R, N, K, N, R, B, B, Q)
                                                  //| List(R, N, K, N, R, Q, B, B)
                                                  //| List(R, N, K, N, B, R, Q, B)
                                                  //| List(R, N, K, N, B, B, R, Q)
                                                  //| List(R, N, K, N, B, B, Q, R)
                                                  //| List(R, N, K, N, B, Q, R, B)
                                                  //| List(R, N, K, N, Q, R, B, B)
                                                  //| List(R, N, K, N, Q, B, B, R)
                                                  //| List(R, N, K, B, R, N, B, Q)
                                                  //| List(R, N, K, B, R, Q, B, N)
                                                  //| List(R, N, K, B, N, R, B, Q)
                                                  //| List(R, N, K, B, N, Q, B, R)
                                                  //| List(R, N, K, B, B, R, N, Q)
                                                  //| List(R, N, K, B, B, R, Q, N)
                                                  //| List(R, N, K, B, B, N, R, Q)
                                                  //| List(R, N, K, B, B, N, Q, R)
                                                  //| List(R, N, K, B, B, Q, R, N)
                                                  //| List(R, N, K, B, B, Q, N, R)
                                                  //| List(R, N, K, B, Q, R, B, N)
                                                  //| List(R, N, K, B, Q, N, B, R)
                                                  //| List(R, N, K, Q, R, N, B, B)
                                                  //| List(R, N, K, Q, R, B, B, N)
                                                  //| List(R, N, K, Q, N, R, B, B)
                                                  //| List(R, N, K, Q, N, B, B, R)
                                                  //| List(R, N, K, Q, B, R, N, B)
                                                  //| List(R, N, K, Q, B, N, R, B)
                                                  //| List(R, N, K, Q, B, B, R, N)
                                                  //| List(R, N, K, Q, B, B, N, R)
                                                  //| List(R, N, Q, N, B, K, R, B)
                                                  //| List(R, N, Q, N, K, R, B, B)
                                                  //| List(R, N, Q, N, K, B, B, R)
                                                  //| List(R, N, Q, B, N, K, B, R)
                                                  //| List(R, N, Q, B, B, N, K, R)
                                                  //| List(R, N, Q, B, B, K, R, N)
                                                  //| List(R, N, Q, B, B, K, N, R)
                                                  //| List(R, N, Q, B, K, R, B, N)
                                                  //| List(R, N, Q, B, K, N, B, R)
                                                  //| List(R, N, Q, K, R, N, B, B)
                                                  //| List(R, N, Q, K, R, B, B, N)
                                                  //| List(R, N, Q, K, N, R, B, B)
                                                  //| List(R, N, Q, K, N, B, B, R)
                                                  //| List(R, N, Q, K, B, R, N, B)
                                                  //| List(R, N, Q, K, B, B, R, N)
                                                  //| List(R, N, Q, K, B, B, N, R)
                                                  //| List(R, B, N, N, B, K, R, Q)
                                                  //| List(R, B, N, N, B, K, Q, R)
                                                  //| List(R, B, N, N, B, Q, K, R)
                                                  //| List(R, B, N, N, K, R, B, Q)
                                                  //| List(R, B, N, N, K, Q, B, R)
                                                  //| List(R, B, N, N, Q, K, B, R)
                                                  //| List(R, B, N, K, R, N, B, Q)
                                                  //| List(R, B, N, K, R, Q, B, N)
                                                  //| List(R, B, N, K, N, R, B, Q)
                                                  //| List(R, B, N, K, N, Q, B, R)
                                                  //| List(R, B, N, K, B, R, N, Q)
                                                  //| List(R, B, N, K, B, R, Q, N)
                                                  //| List(R, B, N, K, B, N, R, Q)
                                                  //| List(R, B, N, K, B, N, Q, R)
                                                  //| List(R, B, N, K, B, Q, R, N)
                                                  //| List(R, B, N, K, B, Q, N, R)
                                                  //| List(R, B, N, K, Q, R, B, N)
                                                  //| List(R, B, N, K, Q, N, B, R)
                                                  //| List(R, B, N, Q, N, K, B, R)
                                                  //| List(R, B, N, Q, B, N, K, R)
                                                  //| List(R, B, N, Q, B, K, R, N)
                                                  //| List(R, B, N, Q, B, K, N, R)
                                                  //| List(R, B, N, Q, K, R, B, N)
                                                  //| List(R, B, N, Q, K, N, B, R)
                                                  //| List(R, B, B, N, N, K, R, Q)
                                                  //| List(R, B, B, N, N, K, Q, R)
                                                  //| List(R, B, B, N, N, Q, K, R)
                                                  //| List(R, B, B, N, K, R, N, Q)
                                                  //| List(R, B, B, N, K, R, Q, N)
                                                  //| List(R, B, B, N, K, N, R, Q)
                                                  //| List(R, B, B, N, K, N, Q, R)
                                                  //| List(R, B, B, N, K, Q, R, N)
                                                  //| List(R, B, B, N, K, Q, N, R)
                                                  //| List(R, B, B, N, Q, N, K, R)
                                                  //| List(R, B, B, N, Q, K, R, N)
                                                  //| List(R, B, B, N, Q, K, N, R)
                                                  //| List(R, B, B, K, R, N, Q, N)
                                                  //| List(R, B, B, K, R, Q, N, N)
                                                  //| List(R, B, B, K, N, R, N, Q)
                                                  //| List(R, B, B, K, N, R, Q, N)
                                                  //| List(R, B, B, K, N, N, R, Q)
                                                  //| List(R, B, B, K, N, N, Q, R)
                                                  //| List(R, B, B, K, N, Q, R, N)
                                                  //| List(R, B, B, K, N, Q, N, R)
                                                  //| List(R, B, B, K, Q, R, N, N)
                                                  //| List(R, B, B, K, Q, N, R, N)
                                                  //| List(R, B, B, K, Q, N, N, R)
                                                  //| List(R, B, B, Q, N, N, K, R)
                                                  //| List(R, B, B, Q, N, K, R, N)
                                                  //| List(R, B, B, Q, N, K, N, R)
                                                  //| List(R, B, B, Q, K, R, N, N)
                                                  //| List(R, B, B, Q, K, N, R, N)
                                                  //| List(R, B, B, Q, K, N, N, R)
                                                  //| List(R, B, K, R, N, N, B, Q)
                                                  //| List(R, B, K, R, B, N, N, Q)
                                                  //| List(R, B, K, R, Q, N, B, N)
                                                  //| List(R, B, K, N, R, N, B, Q)
                                                  //| List(R, B, K, N, R, Q, B, N)
                                                  //| List(R, B, K, N, N, R, B, Q)
                                                  //| List(R, B, K, N, N, Q, B, R)
                                                  //| List(R, B, K, N, B, R, N, Q)
                                                  //| List(R, B, K, N, B, R, Q, N)
                                                  //| List(R, B, K, N, B, N, R, Q)
                                                  //| List(R, B, K, N, B, N, Q, R)
                                                  //| List(R, B, K, N, B, Q, R, N)
                                                  //| List(R, B, K, N, B, Q, N, R)
                                                  //| List(R, B, K, N, Q, R, B, N)
                                                  //| List(R, B, K, N, Q, N, B, R)
                                                  //| List(R, B, K, Q, R, N, B, N)
                                                  //| List(R, B, K, Q, N, R, B, N)
                                                  //| List(R, B, K, Q, N, N, B, R)
                                                  //| List(R, B, K, Q, B, R, N, N)
                                                  //| List(R, B, K, Q, B, N, R, N)
                                                  //| List(R, B, K, Q, B, N, N, R)
                                                  //| List(R, B, Q, N, N, K, B, R)
                                                  //| List(R, B, Q, N, B, N, K, R)
                                                  //| List(R, B, Q, N, B, K, R, N)
                                                  //| List(R, B, Q, N, B, K, N, R)
                                                  //| List(R, B, Q, N, K, R, B, N)
                                                  //| List(R, B, Q, N, K, N, B, R)
                                                  //| List(R, B, Q, K, R, N, B, N)
                                                  //| List(R, B, Q, K, N, R, B, N)
                                                  //| List(R, B, Q, K, N, N, B, R)
                                                  //| List(R, B, Q, K, B, R, N, N)
                                                  //| List(R, B, Q, K, B, N, N, R)
                                                  //| List(R, K, R, N, N, B, B, Q)
                                                  //| List(R, K, R, N, B, N, Q, B)
                                                  //| List(R, K, R, N, B, B, N, Q)
                                                  //| List(R, K, R, N, B, B, Q, N)
                                                  //| List(R, K, R, N, Q, N, B, B)
                                                  //| List(R, K, R, N, Q, B, B, N)
                                                  //| List(R, K, R, B, N, N, B, Q)
                                                  //| List(R, K, R, B, B, N, N, Q)
                                                  //| List(R, K, R, B, B, N, Q, N)
                                                  //| List(R, K, R, Q, N, N, B, B)
                                                  //| List(R, K, R, Q, N, B, B, N)
                                                  //| List(R, K, R, Q, B, N, N, B)
                                                  //| List(R, K, R, Q, B, B, N, N)
                                                  //| List(R, K, N, R, N, B, B, Q)
                                                  //| List(R, K, N, R, N, Q, B, B)
                                                  //| List(R, K, N, R, B, N, Q, B)
                                                  //| List(R, K, N, R, B, B, N, Q)
                                                  //| List(R, K, N, R, B, B, Q, N)
                                                  //| List(R, K, N, R, B, Q, N, B)
                                                  //| List(R, K, N, R, Q, N, B, B)
                                                  //| List(R, K, N, R, Q, B, B, N)
                                                  //| List(R, K, N, N, R, B, B, Q)
                                                  //| List(R, K, N, N, R, Q, B, B)
                                                  //| List(R, K, N, N, B, R, Q, B)
                                                  //| List(R, K, N, N, B, B, R, Q)
                                                  //| List(R, K, N, N, B, B, Q, R)
                                                  //| List(R, K, N, N, B, Q, R, B)
                                                  //| List(R, K, N, N, Q, R, B, B)
                                                  //| List(R, K, N, N, Q, B, B, R)
                                                  //| List(R, K, N, B, R, N, B, Q)
                                                  //| List(R, K, N, B, R, Q, B, N)
                                                  //| List(R, K, N, B, N, R, B, Q)
                                                  //| List(R, K, N, B, N, Q, B, R)
                                                  //| List(R, K, N, B, B, R, N, Q)
                                                  //| List(R, K, N, B, B, R, Q, N)
                                                  //| List(R, K, N, B, B, N, R, Q)
                                                  //| List(R, K, N, B, B, N, Q, R)
                                                  //| List(R, K, N, B, B, Q, R, N)
                                                  //| List(R, K, N, B, B, Q, N, R)
                                                  //| List(R, K, N, B, Q, R, B, N)
                                                  //| List(R, K, N, B, Q, N, B, R)
                                                  //| List(R, K, N, Q, R, N, B, B)
                                                  //| List(R, K, N, Q, R, B, B, N)
                                                  //| List(R, K, N, Q, N, R, B, B)
                                                  //| List(R, K, N, Q, N, B, B, R)
                                                  //| List(R, K, N, Q, B, R, N, B)
                                                  //| List(R, K, N, Q, B, N, R, B)
                                                  //| List(R, K, N, Q, B, B, R, N)
                                                  //| List(R, K, N, Q, B, B, N, R)
                                                  //| List(R, K, B, R, N, N, Q, B)
                                                  //| List(R, K, B, R, N, B, N, Q)
                                                  //| List(R, K, B, R, N, B, Q, N)
                                                  //| List(R, K, B, R, Q, N, N, B)
                                                  //| List(R, K, B, R, Q, B, N, N)
                                                  //| List(R, K, B, N, R, N, Q, B)
                                                  //| List(R, K, B, N, R, B, N, Q)
                                                  //| List(R, K, B, N, R, B, Q, N)
                                                  //| List(R, K, B, N, N, R, Q, B)
                                                  //| List(R, K, B, N, N, B, R, Q)
                                                  //| List(R, K, B, N, N, B, Q, R)
                                                  //| List(R, K, B, N, Q, R, N, B)
                                                  //| List(R, K, B, N, Q, N, R, B)
                                                  //| List(R, K, B, N, Q, B, R, N)
                                                  //| List(R, K, B, N, Q, B, N, R)
                                                  //| List(R, K, B, B, R, N, N, Q)
                                                  //| List(R, K, B, B, R, N, Q, N)
                                                  //| List(R, K, B, B, N, R, N, Q)
                                                  //| List(R, K, B, B, N, R, Q, N)
                                                  //| List(R, K, B, B, N, N, R, Q)
                                                  //| List(R, K, B, B, N, N, Q, R)
                                                  //| List(R, K, B, B, Q, R, N, N)
                                                  //| List(R, K, B, B, Q, N, R, N)
                                                  //| List(R, K, B, B, Q, N, N, R)
                                                  //| List(R, K, B, Q, R, N, N, B)
                                                  //| List(R, K, B, Q, R, B, N, N)
                                                  //| List(R, K, B, Q, N, R, N, B)
                                                  //| List(R, K, B, Q, N, N, R, B)
                                                  //| List(R, K, B, Q, N, B, R, N)
                                                  //| List(R, K, B, Q, N, B, N, R)
                                                  //| List(R, K, Q, R, N, N, B, B)
                                                  //| List(R, K, Q, R, N, B, B, N)
                                                  //| List(R, K, Q, R, B, N, N, B)
                                                  //| List(R, K, Q, R, B, B, N, N)
                                                  //| List(R, K, Q, N, R, N, B, B)
                                                  //| List(R, K, Q, N, R, B, B, N)
                                                  //| List(R, K, Q, N, N, R, B, B)
                                                  //| List(R, K, Q, N, N, B, B, R)
                                                  //| List(R, K, Q, N, B, R, N, B)
                                                  //| List(R, K, Q, N, B, N, R, B)
                                                  //| List(R, K, Q, N, B, B, R, N)
                                                  //| List(R, K, Q, N, B, B, N, R)
                                                  //| List(R, K, Q, B, R, N, B, N)
                                                  //| List(R, K, Q, B, N, R, B, N)
                                                  //| List(R, K, Q, B, N, N, B, R)
                                                  //| List(R, K, Q, B, B, R, N, N)
                                                  //| List(R, K, Q, B, B, N, R, N)
                                                  //| List(R, K, Q, B, B, N, N, R)
                                                  //| List(R, Q, N, N, B, B, K, R)
                                                  //| List(R, Q, N, N, K, R, B, B)
                                                  //| List(R, Q, N, N, K, B, B, R)
                                                  //| List(R, Q, N, B, N, K, B, R)
                                                  //| List(R, Q, N, B, B, N, K, R)
                                                  //| List(R, Q, N, B, B, K, N, R)
                                                  //| List(R, Q, N, B, K, R, B, N)
                                                  //| List(R, Q, N, B, K, N, B, R)
                                                  //| List(R, Q, N, K, R, N, B, B)
                                                  //| List(R, Q, N, K, R, B, B, N)
                                                  //| List(R, Q, N, K, N, R, B, B)
                                                  //| List(R, Q, N, K, N, B, B, R)
                                                  //| List(R, Q, N, K, B, R, N, B)
                                                  //| List(R, Q, N, K, B, N, R, B)
                                                  //| List(R, Q, N, K, B, B, R, N)
                                                  //| List(R, Q, N, K, B, B, N, R)
                                                  //| List(R, Q, B, N, N, B, K, R)
                                                  //| List(R, Q, B, N, N, K, R, B)
                                                  //| List(R, Q, B, N, K, R, N, B)
                                                  //| List(R, Q, B, N, K, N, R, B)
                                                  //| List(R, Q, B, N, K, B, R, N)
                                                  //| List(R, Q, B, N, K, B, N, R)
                                                  //| List(R, Q, B, B, N, N, K, R)
                                                  //| List(R, Q, B, B, N, K, R, N)
                                                  //| List(R, Q, B, B, N, K, N, R)
                                                  //| List(R, Q, B, B, K, R, N, N)
                                                  //| List(R, Q, B, B, K, N, R, N)
                                                  //| List(R, Q, B, B, K, N, N, R)
                                                  //| List(R, Q, B, K, R, N, N, B)
                                                  //| List(R, Q, B, K, R, B, N, N)
                                                  //| List(R, Q, B, K, N, R, N, B)
                                                  //| List(R, Q, B, K, N, N, R, B)
                                                  //| List(R, Q, B, K, N, B, R, N)
                                                  //| List(R, Q, B, K, N, B, N, R)
                                                  //| List(R, Q, K, R, N, N, B, B)
                                                  //| List(R, Q, K, R, N, B, B, N)
                                                  //| List(R, Q, K, R, B, N, N, B)
                                                  //| List(R, Q, K, R, B, B, N, N)
                                                  //| List(R, Q, K, N, R, N, B, B)
                                                  //| List(R, Q, K, N, R, B, B, N)
                                                  //| List(R, Q, K, N, N, R, B, B)
                                                  //| List(R, Q, K, N, N, B, B, R)
                                                  //| List(R, Q, K, N, B, R, N, B)
                                                  //| List(R, Q, K, N, B, N, R, B)
                                                  //| List(R, Q, K, N, B, B, R, N)
                                                  //| List(R, Q, K, N, B, B, N, R)
                                                  //| List(R, Q, K, B, R, N, B, N)
                                                  //| List(R, Q, K, B, N, N, B, R)
                                                  //| List(R, Q, K, B, B, R, N, N)
                                                  //| List(R, Q, K, B, B, N, R, N)
                                                  //| List(R, Q, K, B, B, N, N, R)
                                                  //| List(N, R, N, B, B, K, R, Q)
                                                  //| List(N, R, N, B, B, K, Q, R)
                                                  //| List(N, R, N, B, B, Q, K, R)
                                                  //| List(N, R, N, B, Q, K, B, R)
                                                  //| List(N, R, N, K, R, B, B, Q)
                                                  //| List(N, R, N, K, R, Q, B, B)
                                                  //| List(N, R, N, K, B, R, Q, B)
                                                  //| List(N, R, N, K, B, B, R, Q)
                                                  //| List(N, R, N, K, B, B, Q, R)
                                                  //| List(N, R, N, K, B, Q, R, B)
                                                  //| List(N, R, N, K, Q, R, B, B)
                                                  //| List(N, R, N, K, Q, B, B, R)
                                                  //| List(N, R, N, Q, B, B, K, R)
                                                  //| List(N, R, N, Q, B, K, R, B)
                                                  //| List(N, R, N, Q, K, R, B, B)
                                                  //| List(N, R, N, Q, K, B, B, R)
                                                  //| List(N, R, B, N, K, R, Q, B)
                                                  //| List(N, R, B, N, K, B, R, Q)
                                                  //| List(N, R, B, N, K, B, Q, R)
                                                  //| List(N, R, B, N, K, Q, R, B)
                                                  //| List(N, R, B, N, Q, B, K, R)
                                                  //| List(N, R, B, N, Q, K, R, B)
                                                  //| List(N, R, B, B, N, K, R, Q)
                                                  //| List(N, R, B, B, N, K, Q, R)
                                                  //| List(N, R, B, B, N, Q, K, R)
                                                  //| List(N, R, B, B, K, R, N, Q)
                                                  //| List(N, R, B, B, K, R, Q, N)
                                                  //| List(N, R, B, B, K, N, R, Q)
                                                  //| List(N, R, B, B, K, N, Q, R)
                                                  //| List(N, R, B, B, K, Q, R, N)
                                                  //| List(N, R, B, B, K, Q, N, R)
                                                  //| List(N, R, B, B, Q, N, K, R)
                                                  //| List(N, R, B, B, Q, K, R, N)
                                                  //| List(N, R, B, B, Q, K, N, R)
                                                  //| List(N, R, B, K, R, N, Q, B)
                                                  //| List(N, R, B, K, R, B, Q, N)
                                                  //| List(N, R, B, K, R, Q, N, B)
                                                  //| List(N, R, B, K, N, R, Q, B)
                                                  //| List(N, R, B, K, N, B, R, Q)
                                                  //| List(N, R, B, K, N, B, Q, R)
                                                  //| List(N, R, B, K, N, Q, R, B)
                                                  //| List(N, R, B, K, Q, R, N, B)
                                                  //| List(N, R, B, K, Q, N, R, B)
                                                  //| List(N, R, B, K, Q, B, R, N)
                                                  //| List(N, R, B, K, Q, B, N, R)
                                                  //| List(N, R, B, Q, N, B, K, R)
                                                  //| List(N, R, B, Q, N, K, R, B)
                                                  //| List(N, R, B, Q, K, R, N, B)
                                                  //| List(N, R, B, Q, K, N, R, B)
                                                  //| List(N, R, B, Q, K, B, R, N)
                                                  //| List(N, R, K, R, N, B, B, Q)
                                                  //| List(N, R, K, R, N, Q, B, B)
                                                  //| List(N, R, K, R, B, B, N, Q)
                                                  //| List(N, R, K, R, B, Q, N, B)
                                                  //| List(N, R, K, R, Q, N, B, B)
                                                  //| List(N, R, K, R, Q, B, B, N)
                                                  //| List(N, R, K, N, R, B, B, Q)
                                                  //| List(N, R, K, N, R, Q, B, B)
                                                  //| List(N, R, K, N, B, R, Q, B)
                                                  //| List(N, R, K, N, B, B, R, Q)
                                                  //| List(N, R, K, N, B, B, Q, R)
                                                  //| List(N, R, K, N, B, Q, R, B)
                                                  //| List(N, R, K, N, Q, R, B, B)
                                                  //| List(N, R, K, N, Q, B, B, R)
                                                  //| List(N, R, K, B, R, N, B, Q)
                                                  //| List(N, R, K, B, R, Q, B, N)
                                                  //| List(N, R, K, B, N, R, B, Q)
                                                  //| List(N, R, K, B, N, Q, B, R)
                                                  //| List(N, R, K, B, B, R, N, Q)
                                                  //| List(N, R, K, B, B, N, R, Q)
                                                  //| List(N, R, K, B, B, Q, R, N)
                                                  //| List(N, R, K, B, B, Q, N, R)
                                                  //| List(N, R, K, B, Q, R, B, N)
                                                  //| List(N, R, K, B, Q, N, B, R)
                                                  //| List(N, R, K, Q, R, N, B, B)
                                                  //| List(N, R, K, Q, R, B, B, N)
                                                  //| List(N, R, K, Q, N, R, B, B)
                                                  //| List(N, R, K, Q, N, B, B, R)
                                                  //| List(N, R, K, Q, B, R, N, B)
                                                  //| List(N, R, K, Q, B, N, R, B)
                                                  //| List(N, R, K, Q, B, B, R, N)
                                                  //| List(N, R, K, Q, B, B, N, R)
                                                  //| List(N, R, Q, N, B, K, R, B)
                                                  //| List(N, R, Q, N, K, R, B, B)
                                                  //| List(N, R, Q, N, K, B, B, R)
                                                  //| List(N, R, Q, B, N, K, B, R)
                                                  //| List(N, R, Q, B, B, N, K, R)
                                                  //| List(N, R, Q, B, B, K, R, N)
                                                  //| List(N, R, Q, B, B, K, N, R)
                                                  //| List(N, R, Q, B, K, R, B, N)
                                                  //| List(N, R, Q, B, K, N, B, R)
                                                  //| List(N, R, Q, K, R, N, B, B)
                                                  //| List(N, R, Q, K, R, B, B, N)
                                                  //| List(N, R, Q, K, N, R, B, B)
                                                  //| List(N, R, Q, K, N, B, B, R)
                                                  //| List(N, R, Q, K, B, R, N, B)
                                                  //| List(N, R, Q, K, B, B, R, N)
                                                  //| List(N, R, Q, K, B, B, N, R)
                                                  //| List(N, N, R, B, B, K, R, Q)
                                                  //| List(N, N, R, B, B, K, Q, R)
                                                  //| List(N, N, R, B, B, Q, K, R)
                                                  //| List(N, N, R, B, K, R, B, Q)
                                                  //| List(N, N, R, B, K, Q, B, R)
                                                  //| List(N, N, R, B, Q, K, B, R)
                                                  //| List(N, N, R, K, R, Q, B, B)
                                                  //| List(N, N, R, K, B, R, Q, B)
                                                  //| List(N, N, R, K, B, B, Q, R)
                                                  //| List(N, N, R, K, B, Q, R, B)
                                                  //| List(N, N, R, K, Q, R, B, B)
                                                  //| List(N, N, R, K, Q, B, B, R)
                                                  //| List(N, N, R, Q, B, B, K, R)
                                                  //| List(N, N, R, Q, B, K, R, B)
                                                  //| List(N, N, R, Q, K, R, B, B)
                                                  //| List(N, N, R, Q, K, B, B, R)
                                                  //| List(N, N, B, R, K, R, Q, B)
                                                  //| List(N, N, B, R, K, B, R, Q)
                                                  //| List(N, N, B, R, K, B, Q, R)
                                                  //| List(N, N, B, R, K, Q, R, B)
                                                  //| List(N, N, B, R, Q, B, K, R)
                                                  //| List(N, N, B, R, Q, K, R, B)
                                                  //| List(N, N, B, B, R, K, R, Q)
                                                  //| List(N, N, B, B, R, K, Q, R)
                                                  //| List(N, N, B, B, R, Q, K, R)
                                                  //| List(N, N, B, B, Q, R, K, R)
                                                  //| List(N, N, B, Q, R, B, K, R)
                                                  //| List(N, N, B, Q, R, K, R, B)
                                                  //| List(N, N, Q, R, B, K, R, B)
                                                  //| List(N, N, Q, R, K, R, B, B)
                                                  //| List(N, N, Q, R, K, B, B, R)
                                                  //| List(N, B, R, N, B, K, R, Q)
                                                  //| List(N, B, R, N, B, Q, K, R)
                                                  //| List(N, B, R, N, K, R, B, Q)
                                                  //| List(N, B, R, N, K, Q, B, R)
                                                  //| List(N, B, R, N, Q, K, B, R)
                                                  //| List(N, B, R, K, N, R, B, Q)
                                                  //| List(N, B, R, K, B, R, Q, N)
                                                  //| List(N, B, R, K, B, N, Q, R)
                                                  //| List(N, B, R, K, Q, R, B, N)
                                                  //| List(N, B, R, K, Q, N, B, R)
                                                  //| List(N, B, R, Q, N, K, B, R)
                                                  //| List(N, B, R, Q, B, N, K, R)
                                                  //| List(N, B, R, Q, B, K, R, N)
                                                  //| List(N, B, R, Q, B, K, N, R)
                                                  //| List(N, B, R, Q, K, R, B, N)
                                                  //| List(N, B, R, Q, K, N, B, R)
                                                  //| List(N, B, N, R, B, K, R, Q)
                                                  //| List(N, B, N, R, B, K, Q, R)
                                                  //| List(N, B, N, R, B, Q, K, R)
                                                  //| List(N, B, N, R, K, R, B, Q)
                                                  //| List(N, B, N, R, K, Q, B, R)
                                                  //| List(N, B, N, R, Q, K, B, R)
                                                  //| List(N, B, N, Q, R, K, B, R)
                                                  //| List(N, B, B, R, N, K, R, Q)
                                                  //| List(N, B, B, R, N, K, Q, R)
                                                  //| List(N, B, B, R, N, Q, K, R)
                                                  //| List(N, B, B, R, K, R, N, Q)
                                                  //| List(N, B, B, R, K, R, Q, N)
                                                  //| List(N, B, B, R, K, N, R, Q)
                                                  //| List(N, B, B, R, K, N, Q, R)
                                                  //| List(N, B, B, R, K, Q, R, N)
                                                  //| List(N, B, B, R, K, Q, N, R)
                                                  //| List(N, B, B, R, Q, N, K, R)
                                                  //| List(N, B, B, R, Q, K, R, N)
                                                  //| List(N, B, B, R, Q, K, N, R)
                                                  //| List(N, B, B, N, R, K, R, Q)
                                                  //| List(N, B, B, N, R, K, Q, R)
                                                  //| List(N, B, B, N, R, Q, K, R)
                                                  //| List(N, B, B, N, Q, R, K, R)
                                                  //| List(N, B, B, Q, R, N, K, R)
                                                  //| List(N, B, B, Q, R, K, R, N)
                                                  //| List(N, B, B, Q, R, K, N, R)
                                                  //| List(N, B, B, Q, N, R, K, R)
                                                  //| List(N, B, Q, R, N, K, B, R)
                                                  //| List(N, B, Q, R, B, N, K, R)
                                                  //| List(N, B, Q, R, B, K, R, N)
                                                  //| List(N, B, Q, R, B, K, N, R)
                                                  //| List(N, B, Q, R, K, R, B, N)
                                                  //| List(N, B, Q, R, K, N, B, R)
                                                  //| List(N, Q, R, N, B, B, K, R)
                                                  //| List(N, Q, R, N, K, R, B, B)
                                                  //| List(N, Q, R, N, K, B, B, R)
                                                  //| List(N, Q, R, B, N, K, B, R)
                                                  //| List(N, Q, R, B, B, N, K, R)
                                                  //| List(N, Q, R, B, B, K, N, R)
                                                  //| List(N, Q, R, B, K, R, B, N)
                                                  //| List(N, Q, R, B, K, N, B, R)
                                                  //| List(N, Q, R, K, R, N, B, B)
                                                  //| List(N, Q, R, K, R, B, B, N)
                                                  //| List(N, Q, R, K, N, R, B, B)
                                                  //| List(N, Q, R, K, N, B, B, R)
                                                  //| List(N, Q, R, K, B, R, N, B)
                                                  //| List(N, Q, R, K, B, N, R, B)
                                                  //| List(N, Q, R, K, B, B, R, N)
                                                  //| List(N, Q, R, K, B, B, N, R)
                                                  //| List(N, Q, N, R, B, B, K, R)
                                                  //| List(N, Q, N, R, K, R, B, B)
                                                  //| List(N, Q, N, R, K, B, B, R)
                                                  //| List(N, Q, N, B, B, R, K, R)
                                                  //| List(N, Q, B, R, N, B, K, R)
                                                  //| List(N, Q, B, R, N, K, R, B)
                                                  //| List(N, Q, B, R, K, R, N, B)
                                                  //| List(N, Q, B, R, K, N, R, B)
                                                  //| List(N, Q, B, R, K, B, R, N)
                                                  //| List(N, Q, B, N, R, B, K, R)
                                                  //| List(N, Q, B, B, R, N, K, R)
                                                  //| List(N, Q, B, B, R, K, N, R)
                                                  //| List(N, Q, B, B, N, R, K, R)
                                                  //| List(B, R, N, N, K, R, Q, B)
                                                  //| List(B, R, N, N, K, B, R, Q)
                                                  //| List(B, R, N, N, K, B, Q, R)
                                                  //| List(B, R, N, N, K, Q, R, B)
                                                  //| List(B, R, N, N, Q, B, K, R)
                                                  //| List(B, R, N, N, Q, K, R, B)
                                                  //| List(B, R, N, B, N, K, R, Q)
                                                  //| List(B, R, N, B, N, K, Q, R)
                                                  //| List(B, R, N, B, N, Q, K, R)
                                                  //| List(B, R, N, B, K, R, N, Q)
                                                  //| List(B, R, N, B, K, R, Q, N)
                                                  //| List(B, R, N, B, K, N, R, Q)
                                                  //| List(B, R, N, B, K, N, Q, R)
                                                  //| List(B, R, N, B, Q, N, K, R)
                                                  //| List(B, R, N, B, Q, K, R, N)
                                                  //| List(B, R, N, B, Q, K, N, R)
                                                  //| List(B, R, N, K, R, N, Q, B)
                                                  //| List(B, R, N, K, R, B, N, Q)
                                                  //| List(B, R, N, K, R, B, Q, N)
                                                  //| List(B, R, N, K, R, Q, N, B)
                                                  //| List(B, R, N, K, N, R, Q, B)
                                                  //| List(B, R, N, K, N, B, R, Q)
                                                  //| List(B, R, N, K, N, B, Q, R)
                                                  //| List(B, R, N, K, N, Q, R, B)
                                                  //| List(B, R, N, K, Q, R, N, B)
                                                  //| List(B, R, N, K, Q, N, R, B)
                                                  //| List(B, R, N, K, Q, B, R, N)
                                                  //| List(B, R, N, K, Q, B, N, R)
                                                  //| List(B, R, N, Q, N, B, K, R)
                                                  //| List(B, R, N, Q, N, K, R, B)
                                                  //| List(B, R, N, Q, K, R, N, B)
                                                  //| List(B, R, N, Q, K, N, R, B)
                                                  //| List(B, R, N, Q, K, B, R, N)
                                                  //| List(B, R, K, R, N, B, N, Q)
                                                  //| List(B, R, K, R, N, Q, N, B)
                                                  //| List(B, R, K, R, Q, N, N, B)
                                                  //| List(B, R, K, R, Q, B, N, N)
                                                  //| List(B, R, K, N, R, N, Q, B)
                                                  //| List(B, R, K, N, R, B, N, Q)
                                                  //| List(B, R, K, N, R, B, Q, N)
                                                  //| List(B, R, K, N, R, Q, N, B)
                                                  //| List(B, R, K, N, N, R, Q, B)
                                                  //| List(B, R, K, N, N, B, R, Q)
                                                  //| List(B, R, K, N, N, B, Q, R)
                                                  //| List(B, R, K, N, N, Q, R, B)
                                                  //| List(B, R, K, N, Q, R, N, B)
                                                  //| List(B, R, K, N, Q, N, R, B)
                                                  //| List(B, R, K, N, Q, B, R, N)
                                                  //| List(B, R, K, N, Q, B, N, R)
                                                  //| List(B, R, K, B, R, N, N, Q)
                                                  //| List(B, R, K, B, R, Q, N, N)
                                                  //| List(B, R, K, B, N, R, N, Q)
                                                  //| List(B, R, K, B, N, N, R, Q)
                                                  //| List(B, R, K, B, N, Q, R, N)
                                                  //| List(B, R, K, B, N, Q, N, R)
                                                  //| List(B, R, K, B, Q, R, N, N)
                                                  //| List(B, R, K, B, Q, N, R, N)
                                                  //| List(B, R, K, B, Q, N, N, R)
                                                  //| List(B, R, K, Q, R, N, N, B)
                                                  //| List(B, R, K, Q, R, B, N, N)
                                                  //| List(B, R, K, Q, N, R, N, B)
                                                  //| List(B, R, K, Q, N, N, R, B)
                                                  //| List(B, R, K, Q, N, B, R, N)
                                                  //| List(B, R, K, Q, N, B, N, R)
                                                  //| List(B, R, Q, N, N, K, R, B)
                                                  //| List(B, R, Q, N, K, R, N, B)
                                                  //| List(B, R, Q, N, K, N, R, B)
                                                  //| List(B, R, Q, N, K, B, R, N)
                                                  //| List(B, R, Q, N, K, B, N, R)
                                                  //| List(B, R, Q, B, N, N, K, R)
                                                  //| List(B, R, Q, B, N, K, R, N)
                                                  //| List(B, R, Q, B, N, K, N, R)
                                                  //| List(B, R, Q, B, K, R, N, N)
                                                  //| List(B, R, Q, B, K, N, R, N)
                                                  //| List(B, R, Q, B, K, N, N, R)
                                                  //| List(B, R, Q, K, R, N, N, B)
                                                  //| List(B, R, Q, K, R, B, N, N)
                                                  //| List(B, R, Q, K, N, R, N, B)
                                                  //| List(B, R, Q, K, N, N, R, B)
                                                  //| List(B, R, Q, K, N, B, R, N)
                                                  //| List(B, R, Q, K, N, B, N, R)
                                                  //| List(B, N, R, N, K, R, Q, B)
                                                  //| List(B, N, R, N, K, B, R, Q)
                                                  //| List(B, N, R, N, K, B, Q, R)
                                                  //| List(B, N, R, N, K, Q, R, B)
                                                  //| List(B, N, R, N, Q, B, K, R)
                                                  //| List(B, N, R, N, Q, K, R, B)
                                                  //| List(B, N, R, B, N, K, R, Q)
                                                  //| List(B, N, R, B, N, K, Q, R)
                                                  //| List(B, N, R, B, N, Q, K, R)
                                                  //| List(B, N, R, B, K, R, N, Q)
                                                  //| List(B, N, R, B, K, R, Q, N)
                                                  //| List(B, N, R, B, K, N, R, Q)
                                                  //| List(B, N, R, B, K, N, Q, R)
                                                  //| List(B, N, R, B, K, Q, R, N)
                                                  //| List(B, N, R, B, K, Q, N, R)
                                                  //| List(B, N, R, B, Q, N, K, R)
                                                  //| List(B, N, R, B, Q, K, R, N)
                                                  //| List(B, N, R, B, Q, K, N, R)
                                                  //| List(B, N, R, K, R, N, Q, B)
                                                  //| List(B, N, R, K, R, B, Q, N)
                                                  //| List(B, N, R, K, R, Q, N, B)
                                                  //| List(B, N, R, K, N, R, Q, B)
                                                  //| List(B, N, R, K, N, B, R, Q)
                                                  //| List(B, N, R, K, N, B, Q, R)
                                                  //| List(B, N, R, K, N, Q, R, B)
                                                  //| List(B, N, R, K, Q, R, N, B)
                                                  //| List(B, N, R, K, Q, N, R, B)
                                                  //| List(B, N, R, K, Q, B, R, N)
                                                  //| List(B, N, R, K, Q, B, N, R)
                                                  //| List(B, N, R, Q, N, B, K, R)
                                                  //| List(B, N, R, Q, N, K, R, B)
                                                  //| List(B, N, R, Q, K, R, N, B)
                                                  //| List(B, N, R, Q, K, N, R, B)
                                                  //| List(B, N, R, Q, K, B, R, N)
                                                  //| List(B, N, N, R, K, R, Q, B)
                                                  //| List(B, N, N, R, K, B, R, Q)
                                                  //| List(B, N, N, R, K, B, Q, R)
                                                  //| List(B, N, N, R, K, Q, R, B)
                                                  //| List(B, N, N, R, Q, B, K, R)
                                                  //| List(B, N, N, R, Q, K, R, B)
                                                  //| List(B, N, N, B, R, K, R, Q)
                                                  //| List(B, N, N, B, R, K, Q, R)
                                                  //| List(B, N, N, B, R, Q, K, R)
                                                  //| List(B, N, N, B, Q, R, K, R)
                                                  //| List(B, N, N, Q, R, B, K, R)
                                                  //| List(B, N, N, Q, R, K, R, B)
                                                  //| List(B, N, Q, R, N, K, R, B)
                                                  //| List(B, N, Q, R, K, R, N, B)
                                                  //| List(B, N, Q, R, K, N, R, B)
                                                  //| List(B, N, Q, R, K, B, R, N)
                                                  //| List(B, N, Q, R, K, B, N, R)
                                                  //| List(B, N, Q, N, R, K, R, B)
                                                  //| List(B, N, Q, B, R, N, K, R)
                                                  //| List(B, N, Q, B, R, K, R, N)
                                                  //| List(B, N, Q, B, R, K, N, R)
                                                  //| List(B, B, R, N, N, K, R, Q)
                                                  //| List(B, B, R, N, N, K, Q, R)
                                                  //| List(B, B, R, N, N, Q, K, R)
                                                  //| List(B, B, R, N, K, R, N, Q)
                                                  //| List(B, B, R, N, K, R, Q, N)
                                                  //| List(B, B, R, N, K, N, R, Q)
                                                  //| List(B, B, R, N, K, N, Q, R)
                                                  //| List(B, B, R, N, K, Q, R, N)
                                                  //| List(B, B, R, N, K, Q, N, R)
                                                  //| List(B, B, R, N, Q, N, K, R)
                                                  //| List(B, B, R, N, Q, K, R, N)
                                                  //| List(B, B, R, N, Q, K, N, R)
                                                  //| List(B, B, R, K, R, N, Q, N)
                                                  //| List(B, B, R, K, R, Q, N, N)
                                                  //| List(B, B, R, K, N, R, N, Q)
                                                  //| List(B, B, R, K, N, R, Q, N)
                                                  //| List(B, B, R, K, N, N, R, Q)
                                                  //| List(B, B, R, K, N, N, Q, R)
                                                  //| List(B, B, R, K, N, Q, R, N)
                                                  //| List(B, B, R, K, N, Q, N, R)
                                                  //| List(B, B, R, K, Q, R, N, N)
                                                  //| List(B, B, R, K, Q, N, R, N)
                                                  //| List(B, B, R, K, Q, N, N, R)
                                                  //| List(B, B, R, Q, N, N, K, R)
                                                  //| List(B, B, R, Q, N, K, R, N)
                                                  //| List(B, B, R, Q, N, K, N, R)
                                                  //| List(B, B, R, Q, K, R, N, N)
                                                  //| List(B, B, R, Q, K, N, R, N)
                                                  //| List(B, B, R, Q, K, N, N, R)
                                                  //| List(B, B, N, R, N, K, R, Q)
                                                  //| List(B, B, N, R, N, K, Q, R)
                                                  //| List(B, B, N, R, N, Q, K, R)
                                                  //| List(B, B, N, R, K, R, N, Q)
                                                  //| List(B, B, N, R, K, R, Q, N)
                                                  //| List(B, B, N, R, K, N, R, Q)
                                                  //| List(B, B, N, R, K, N, Q, R)
                                                  //| List(B, B, N, R, K, Q, R, N)
                                                  //| List(B, B, N, R, K, Q, N, R)
                                                  //| List(B, B, N, R, Q, N, K, R)
                                                  //| List(B, B, N, R, Q, K, R, N)
                                                  //| List(B, B, N, R, Q, K, N, R)
                                                  //| List(B, B, N, N, R, K, R, Q)
                                                  //| List(B, B, N, N, R, K, Q, R)
                                                  //| List(B, B, N, N, R, Q, K, R)
                                                  //| List(B, B, N, N, Q, R, K, R)
                                                  //| List(B, B, N, Q, R, N, K, R)
                                                  //| List(B, B, N, Q, R, K, R, N)
                                                  //| List(B, B, N, Q, R, K, N, R)
                                                  //| List(B, B, N, Q, N, R, K, R)
                                                  //| List(B, B, Q, R, N, N, K, R)
                                                  //| List(B, B, Q, R, N, K, R, N)
                                                  //| List(B, B, Q, R, N, K, N, R)
                                                  //| List(B, B, Q, R, K, R, N, N)
                                                  //| List(B, B, Q, R, K, N, R, N)
                                                  //| List(B, B, Q, R, K, N, N, R)
                                                  //| List(B, B, Q, N, R, N, K, R)
                                                  //| List(B, B, Q, N, R, K, R, N)
                                                  //| List(B, B, Q, N, R, K, N, R)
                                                  //| List(B, Q, R, N, N, B, K, R)
                                                  //| List(B, Q, R, N, N, K, R, B)
                                                  //| List(B, Q, R, N, K, R, N, B)
                                                  //| List(B, Q, R, N, K, N, R, B)
                                                  //| List(B, Q, R, N, K, B, R, N)
                                                  //| List(B, Q, R, N, K, B, N, R)
                                                  //| List(B, Q, R, B, N, N, K, R)
                                                  //| List(B, Q, R, B, N, K, R, N)
                                                  //| List(B, Q, R, B, N, K, N, R)
                                                  //| List(B, Q, R, B, K, R, N, N)
                                                  //| List(B, Q, R, B, K, N, R, N)
                                                  //| List(B, Q, R, B, K, N, N, R)
                                                  //| List(B, Q, R, K, R, N, N, B)
                                                  //| List(B, Q, R, K, R, B, N, N)
                                                  //| List(B, Q, R, K, N, R, N, B)
                                                  //| List(B, Q, R, K, N, N, R, B)
                                                  //| List(B, Q, R, K, N, B, R, N)
                                                  //| List(B, Q, R, K, N, B, N, R)
                                                  //| List(B, Q, N, R, N, B, K, R)
                                                  //| List(B, Q, N, R, N, K, R, B)
                                                  //| List(B, Q, N, R, K, R, N, B)
                                                  //| List(B, Q, N, R, K, N, R, B)
                                                  //| List(B, Q, N, R, K, B, R, N)
                                                  //| List(B, Q, N, N, R, B, K, R)
                                                  //| List(B, Q, N, B, R, N, K, R)
                                                  //| List(B, Q, N, B, R, K, N, R)
                                                  //| List(B, Q, N, B, N, R, K, R)
                                                  //| List(Q, R, N, N, B, B, K, R)
                                                  //| List(Q, R, N, N, B, K, R, B)
                                                  //| List(Q, R, N, N, K, R, B, B)
                                                  //| List(Q, R, N, N, K, B, B, R)
                                                  //| List(Q, R, N, B, N, K, B, R)
                                                  //| List(Q, R, N, B, B, N, K, R)
                                                  //| List(Q, R, N, B, B, K, R, N)
                                                  //| List(Q, R, N, B, B, K, N, R)
                                                  //| List(Q, R, N, B, K, N, B, R)
                                                  //| List(Q, R, N, K, R, N, B, B)
                                                  //| List(Q, R, N, K, R, B, B, N)
                                                  //| List(Q, R, N, K, N, R, B, B)
                                                  //| List(Q, R, N, K, N, B, B, R)
                                                  //| List(Q, R, N, K, B, R, N, B)
                                                  //| List(Q, R, N, K, B, N, R, B)
                                                  //| List(Q, R, N, K, B, B, R, N)
                                                  //| List(Q, R, N, K, B, B, N, R)
                                                  //| List(Q, R, B, N, N, B, K, R)
                                                  //| List(Q, R, B, N, N, K, R, B)
                                                  //| List(Q, R, B, N, K, R, N, B)
                                                  //| List(Q, R, B, N, K, N, R, B)
                                                  //| List(Q, R, B, N, K, B, R, N)
                                                  //| List(Q, R, B, N, K, B, N, R)
                                                  //| List(Q, R, B, B, N, N, K, R)
                                                  //| List(Q, R, B, B, N, K, R, N)
                                                  //| List(Q, R, B, B, N, K, N, R)
                                                  //| List(Q, R, B, B, K, N, R, N)
                                                  //| List(Q, R, B, B, K, N, N, R)
                                                  //| List(Q, R, B, K, R, N, N, B)
                                                  //| List(Q, R, B, K, R, B, N, N)
                                                  //| List(Q, R, B, K, N, R, N, B)
                                                  //| List(Q, R, B, K, N, N, R, B)
                                                  //| List(Q, R, B, K, N, B, R, N)
                                                  //| List(Q, R, B, K, N, B, N, R)
                                                  //| List(Q, R, K, R, N, N, B, B)
                                                  //| List(Q, R, K, R, N, B, B, N)
                                                  //| List(Q, R, K, R, B, N, N, B)
                                                  //| List(Q, R, K, R, B, B, N, N)
                                                  //| List(Q, R, K, N, R, N, B, B)
                                                  //| List(Q, R, K, N, R, B, B, N)
                                                  //| List(Q, R, K, N, N, R, B, B)
                                                  //| List(Q, R, K, N, N, B, B, R)
                                                  //| List(Q, R, K, N, B, R, N, B)
                                                  //| List(Q, R, K, N, B, N, R, B)
                                                  //| List(Q, R, K, N, B, B, R, N)
                                                  //| List(Q, R, K, N, B, B, N, R)
                                                  //| List(Q, R, K, B, R, N, B, N)
                                                  //| List(Q, R, K, B, N, R, B, N)
                                                  //| List(Q, R, K, B, N, N, B, R)
                                                  //| List(Q, R, K, B, B, R, N, N)
                                                  //| List(Q, R, K, B, B, N, R, N)
                                                  //| List(Q, R, K, B, B, N, N, R)
                                                  //| List(Q, N, R, N, B, B, K, R)
                                                  //| List(Q, N, R, N, B, K, R, B)
                                                  //| List(Q, N, R, N, K, R, B, B)
                                                  //| List(Q, N, R, N, K, B, B, R)
                                                  //| List(Q, N, R, B, N, K, B, R)
                                                  //| List(Q, N, R, B, B, N, K, R)
                                                  //| List(Q, N, R, B, B, K, R, N)
                                                  //| List(Q, N, R, B, B, K, N, R)
                                                  //| List(Q, N, R, B, K, N, B, R)
                                                  //| List(Q, N, R, K, R, N, B, B)
                                                  //| List(Q, N, R, K, R, B, B, N)
                                                  //| List(Q, N, R, K, N, R, B, B)
                                                  //| List(Q, N, R, K, N, B, B, R)
                                                  //| List(Q, N, R, K, B, R, N, B)
                                                  //| List(Q, N, R, K, B, N, R, B)
                                                  //| List(Q, N, R, K, B, B, R, N)
                                                  //| List(Q, N, R, K, B, B, N, R)
                                                  //| List(Q, N, N, R, B, B, K, R)
                                                  //| List(Q, N, N, R, B, K, R, B)
                                                  //| List(Q, N, N, B, R, K, B, R)
                                                  //| List(Q, N, N, B, B, R, K, R)
                                                  //| List(Q, N, B, R, N, B, K, R)
                                                  //| List(Q, N, B, R, N, K, R, B)
                                                  //| List(Q, N, B, R, K, N, R, B)
                                                  //| List(Q, N, B, N, R, B, K, R)
                                                  //| List(Q, N, B, N, R, K, R, B)
                                                  //| List(Q, N, B, B, R, N, K, R)
                                                  //| List(Q, N, B, B, R, K, R, N)
                                                  //| List(Q, N, B, B, R, K, N, R)
                                                  //| List(Q, N, B, B, N, R, K, R)
                                                  //| List(Q, B, R, N, N, K, B, R)
                                                  //| List(Q, B, R, N, B, N, K, R)
                                                  //| List(Q, B, R, N, B, K, R, N)
                                                  //| List(Q, B, R, N, B, K, N, R)
                                                  //| List(Q, B, R, N, K, R, B, N)
                                                  //| List(Q, B, R, N, K, N, B, R)
                                                  //| List(Q, B, R, K, R, N, B, N)
                                                  //| List(Q, B, R, K, N, R, B, N)
                                                  //| List(Q, B, R, K, N, N, B, R)
                                                  //| List(Q, B, R, K, B, R, N, N)
                                                  //| List(Q, B, R, K, B, N, N, R)
                                                  //| List(Q, B, N, R, N, K, B, R)
                                                  //| List(Q, B, N, R, B, N, K, R)
                                                  //| List(Q, B, N, R, B, K, R, N)
                                                  //| List(Q, B, N, R, B, K, N, R)
                                                  //| List(Q, B, N, R, K, N, B, R)
                                                  //| List(Q, B, N, N, R, K, B, R)
                                                  //| List(Q, B, N, N, B, R, K, R)
                                                  //| List(Q, B, B, R, N, N, K, R)
                                                  //| List(Q, B, B, R, N, K, R, N)
                                                  //| List(Q, B, B, R, N, K, N, R)
                                                  //| List(Q, B, B, R, K, N, R, N)
                                                  //| List(Q, B, B, R, K, N, N, R)
                                                  //| List(Q, B, B, N, R, N, K, R)
                                                  //| List(Q, B, B, N, R, K, R, N)
                                                  //| List(Q, B, B, N, R, K, N, R)
                                                  //| List(Q, B, B, N, N, R, K, R)
     
  def main(args: Array[String]) = {
   println("All Solutions via brute force and thinking about it")
   printAllSolutions
   println("Size via brute force = ", solutionViaBruteForce.size)
   println("Size via thinking = ", allSolutions.size)
   println("Solutions not found via brute force")
   solutionViaBruteForce foreach (sb => if (!(allSolutions contains sb)) println(sb) )
   println("Solutions not found via thinking about it")
   allSolutions foreach (sol => if (!(solutionViaBruteForce.map(_._5).toList contains sol)) println(sol) )
  }                                               //> main: (args: Array[String])Unit
}