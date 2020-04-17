namespace ScrabbleUtil

type coord = int * int

module Coord =

    let mkCoordinate x y = (x, y)
    let getX (x, _) = x
    let getY (_, y) = y

type squareProg = Map<int, string>
type boardProg = {
        prog       : string;
        squares    : Map<int, squareProg>
        usedSquare : int
        center     : coord

        isInfinite : bool   // For pretty-printing purposes only
        ppSquare   : string // For pretty-printing purposes only
    }

module Square =
    let emptyTile = Map.empty
    let addFunc m p f = Map.add p f m

    let singleLetterScore = addFunc emptyTile 0u "pointValue(_pos_) + _acc_"
    let doubleLetterScore = addFunc emptyTile 0u "pointValue(_pos_) * 2 + _acc_"
    let tripleLetterScore = addFunc emptyTile 0u "pointValue(_pos_) * 3 + _acc_"

    let doubleWordScore = addFunc singleLetterScore 1u "_acc_ * 2"
    let tripleWordScore = addFunc singleLetterScore 1u "_acc_ * 3"

    (*
type square = Map<uint32, uint32 -> (char * int)[] -> int -> int>
type board = 
    { center   : coord; 
      usedTile : square;
      squares  : Map<coord, square> }

module Board =
    let mkBoard c dt f pf = {center = c; usedTile = dt; squares = f; print = pf}
    let center b = b.center
    let usedTile b = b.usedTile
    let squares b = b.squares

    let mirrorX ((x, y) : coord) = (-x, y)
    let mirrorY ((x, y) : coord) = (x, -y)
    let mirrorXY = mirrorX >> mirrorY

    let appendFunc f lst = List.fold (fun acc x -> (f x)::acc) lst lst
    let mapProdFst s = List.map (fun x -> (x, s))
    let mirrorBoardQuadrant t = 
       appendFunc mirrorX >> appendFunc mirrorY >> 
       (fun ts m -> List.fold (fun acc c -> Map.add c t acc) m ts)

    let printBoard center radius isInfinite squareChar placed played =
 
        let (minX, maxX, minY, maxY) =
            match isInfinite with
            | true ->
                let getVal f g = 
                    if placed |> Map.isEmpty then
                        g center
                     else
                        placed |> Map.toList |> List.map fst |> (f >> g)

                (getVal (List.minBy fst) fst - radius,
                 getVal (List.maxBy fst) fst + radius,
                 getVal (List.minBy snd) snd - radius,
                 getVal (List.maxBy snd) snd + radius)
            | false ->
                (fst center - radius, fst center + radius,
                 snd center - radius, snd center + radius)


        for y in [minY..maxY] do
            for x in [minX..maxX] do
                match Map.tryFind (x, y) placed, Map.tryFind (x, y) played, Map.tryFind (x, y) squareChar with
                | _, Some c, _ -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Red
                    printf "%c " c
                    System.Console.ForegroundColor <- System.ConsoleColor.Black
                | Some (c, _), _, _ -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Blue
                    printf "%c " c
                    System.Console.ForegroundColor <- System.ConsoleColor.Black
                | None, _, Some c -> printf "%c " c
                | None, None, None _ -> printf "# "
            printf "\n"
*)
type tile = Set<char * int>

module Tile =

    let singleLetter l p : tile = Set.singleton (l, p)
    let wildCard ls : tile = ls |> Set.ofList |> Set.map (fun l -> (l, 0))

    let toString (p : tile) = 
        sprintf "{%s}" (Set.fold (fun acc (c, x) -> sprintf "%s (%c, %d) " acc c x) "" p)

