namespace ScrabbleUtil

open ScrabbleUtil.ServerCommunication
open ScrabbleUtil

module StandardBoard =

    let singleLetterScore = Map.add 0 "_result_ := pointValue(_pos_) + _acc_" Map.empty
    let doubleLetterScore = Map.add 0 "_result_ := pointValue(_pos_) * 2 + _acc_" Map.empty
    let tripleLetterScore = Map.add 0 "_result_ := pointValue(_pos_) * 3 + _acc_" Map.empty

    let doubleWordScore = Map.add 1 "_result_ := _acc_ * 2" singleLetterScore
    let tripleWordScore = Map.add 1 "_result_ := _acc_ * 3" singleLetterScore

    let abs v result = 
        let code = 
            "if (_v_ < 0) then { 
                _result_ := _v_ * -1 
             }
             else {
                _result_ := _v_
            }"
        code.Replace("_v_", v).Replace("_result_", result)

    let twsCheck x y =
        let code = "(_x_ = 7 /\ (_y_ = 0 \/ _y_ = 7))"

        code.Replace("_x_", x).Replace("_y_", y) + " \/ " +
        code.Replace("_x_", y).Replace("_y_", x)

    let dwsCheck x y = 
        let code = "_x_ = _y_ /\ _x_ < 7 /\ _x_ > 2"

        code.Replace("_x_", x).Replace("_y_", y)

    let tlsCheck x y =
        let code = "(_x_ = 2 /\ (_y_ = 2 \/ _y_ = 6))"

        code.Replace("_x_", x).Replace("_y_", y) + " \/ " +
        code.Replace("_x_", y).Replace("_y_", x)

    let dlsCheck x y =
        let code = "((_x_ = 0 /\ _y_ = 4) \/ (_x_ = 1 /\ (_y_ = 1 \/ _y_ = 5)) \/ (_x_ = 4 /\ _y_ = 7))"

        code.Replace("_x_", x).Replace("_y_", y) + " \/ " +
        code.Replace("_x_", y).Replace("_y_", x)
        
    let insideCheck x y = 
        let code = "_x_ <= 7 /\ _y_ <= 7"

        code.Replace("_x_", x).Replace("_y_", y)


    let checkSquare f v els = 
        let code = 
            "if (_guard_) then {
                _result_ := _v_ 
             }
             else {
                _els_
             }"

        code.Replace("_guard_", f "xabs" "yabs").Replace("_v_", v).Replace("_els_", els) 
    
    let standardBoardProg =
        "declare xabs;
         declare yabs;
         " + abs "_x_" "xabs" + ";
         " + abs "_y_" "yabs" + ";
         " + checkSquare twsCheck "4"
                (checkSquare dwsCheck "3"
                    (checkSquare tlsCheck "2"
                        (checkSquare dlsCheck "1"
                            (checkSquare insideCheck "0"
                                "_result_ := -1"))))

    let squares = [(0, singleLetterScore); 
                   (1, doubleLetterScore); 
                   (2, tripleLetterScore);
                   (3, doubleWordScore);
                   (4, tripleWordScore)] |>
                   Map.ofList

    let prettyPrint = 
        "if (_input_ = 0) then {
            _result_ := charToInt (' ')
         }
         else {
             if (_input_ = 1) then {
                 _result_ := charToInt ('a')
             }
             else { 
                if (_input_ = 2) then {
                    _result_ := charToInt ('b')
                }
                else { 
                    if (_input_ = 3) then {
                        _result_ := charToInt ('c')
                    }
                    else { 
                        if (_input_ = 4) then {
                            _result_ := charToInt ('d')
                        }
                        else {
                            _result_ := charToInt ('#')
                        }
                    }
                }
            }
        }"

    let standardBoard : boardProg = {
        prog = standardBoardProg;
        squares = squares;
        usedSquare = 0;
        center = (0, 0);
        isInfinite = false;
        ppSquare = prettyPrint
    }



(*
module StandardBoard =
    open Tile
    open Board

    let mkStandardBoard () =
        let bf = 
             ([(7, 7); (7, 0); (0, 7)] |> mirrorBoardQuadrant tripleWordScore) >>        
             ([for i in 3..6 do yield (i, i)] |> mirrorBoardQuadrant doubleWordScore) >>
             ([(3, 3); (6, 3); (3, 6)] |> mirrorBoardQuadrant tripleLetterScore) >>
             ([(4, 0); (1, 1); (5, 1); (0, 4); (7, 4); (1, 5); (4, 7)] |> mirrorBoardQuadrant doubleLetterScore) 

        let tf b =
            function
            | c when Coord.getX c >= -7 && Coord.getY c >= -7  && 
                     Coord.getX c <= 7 && Coord.getY c <= 7  ->
                Some (match Map.tryFind c b with
                      | Some t -> t
                      | None -> singleLetterScore)
            | _ -> None


        let center = (0, 0)
        let tiles = fun c -> tf (bf Map.empty) c
        let print = Board.printBoard center tiles 8 false

        Board.mkBoard center singleLetterScore tiles print

module InfiniteStandardBoard =
     open Tile
     open Board

     let mkBoard () =

        let tf =
            function
            | (0, 0)  -> tripleWordScore

            | (x, y) when x = y && x > 3 && x <= 6 -> doubleWordScore

            | (3, 3) -> tripleLetterScore
            | (6, 3) -> tripleLetterScore
            | (3, 6) -> tripleLetterScore

            | (4, 0) -> doubleLetterScore
            | (1, 1) -> doubleLetterScore
            | (5, 1) -> doubleLetterScore
            | (0, 4) -> doubleLetterScore
            | (7, 4) -> doubleLetterScore
            | (1, 5) -> doubleLetterScore
            | (4, 7) -> doubleLetterScore

            | (_, _) -> singleLetterScore

        let center = (0, 0)
        let tiles =  fun (x : int, y : int) -> Some (tf (System.Math.Abs x % 7, System.Math.Abs y % 7))
        let print = Board.printBoard center tiles 7 true

        Board.mkBoard center singleLetterScore tiles print

module Torus =
    open System

    let mkBoard outerRadius innerRadius =

        let doubleLetterScores start x acc = 
            let dlwInner = (outerRadius - (innerRadius / 2)) |> float
            let dlwOuter = (outerRadius + (innerRadius / 2)) |> float

            let fraction = (2.0 * Math.PI) / x
            [for angle in start .. fraction .. 2.0 * Math.PI + start do 
                yield (Math.Cos angle * dlwInner |> Math.Round |> int, 
                       Math.Sin angle * dlwInner |> Math.Round |> int); 
                yield (Math.Cos angle * dlwOuter |> Math.Round |> int, 
                       Math.Sin angle * dlwOuter |> Math.Round |> int)] |>

            List.fold (fun acc c -> Map.add c Tile.doubleLetterScore acc) acc

        let trippleLetterScores start x acc = 

            let fraction = (2.0 * Math.PI) / x
            [for angle in start .. fraction .. 2.0 * Math.PI + start do 
                yield (Math.Cos angle * (float outerRadius) |> Math.Round |> int, 
                       Math.Sin angle * (float outerRadius) |> Math.Round |> int)] |>

            List.fold (fun acc c -> Map.add c Tile.tripleLetterScore acc) acc

        let startMap = 
            Map.empty |>
            doubleLetterScores 0.123 20.0 |>
            trippleLetterScores 0.35 10.0

        let tf ((x, y) : coord) =
            let fx = float x
            let fy = float y
            let distance = Math.Sqrt (fx * fx + fy * fy) |> Math.Round|> int
            let innerDistance = outerRadius - innerRadius
            let outerDistance = outerRadius + innerRadius
            
            let placeTiles =
                function
                | (0, y) when y = innerDistance -> Tile.tripleWordScore
                | (0, y) when y = outerDistance -> Tile.tripleWordScore
                | (x, 0) when x = innerDistance -> Tile.tripleWordScore
                | (x, 0) when x = outerDistance -> Tile.tripleWordScore
                | (x, y) when x = y && distance = innerDistance -> Tile.tripleWordScore
                | (x, y) when x = y && distance = outerDistance -> Tile.tripleWordScore

                | (0, y) when y <> outerRadius -> Tile.doubleWordScore
                | (x, 0) when x <> outerRadius -> Tile.doubleWordScore
                | (x, y) when x = y && distance <> outerRadius -> Tile.doubleWordScore
                | (x, y) when x = y && distance <> outerRadius -> Tile.doubleWordScore

(*
                | (0, y) -> Tile.doubleWordScore
                | (x, 0) -> Tile.doubleWordScore
                | (x, y) when x = y && x <> diameter / 2 -> Tile.doubleWordScore

                | (x, y) when y = diameter / 4 && x % 4 = 2 -> Tile.tripleLetterScore
                | (x, y) when y = diameter - diameter / 4 && x % 4 = 2 -> Tile.tripleLetterScore

                | (x, y) when y = diameter / 2 && x % 3 = 1 -> Tile.doubleLetterScore
*)
                | (x, y) -> 
                    match Map.tryFind (x, y) startMap with
                    | Some tile -> tile
                    | None      -> Tile.singleLetterScore

 //           printfn "Distance (%d, %d) %d" x y distance
            if  distance >= innerDistance && distance <= outerDistance then
                let coord = (Math.Abs x, Math.Abs y)
    //            printfn "coordinate %A\tdistance %A" coord distance
                Some (placeTiles coord)
            else
                None

        let center = (0, 0)
        let tiles =  tf
        let print = Board.printBoard center tiles (outerRadius + innerRadius + 1) false

        Board.mkBoard (outerRadius, 0) Tile.singleLetterScore tiles print
        

*)

module English =
    let tiles scale =

        [(Tile.singleLetter 'A' 1,  9u);
         (Tile.singleLetter 'B' 3,  2u);
         (Tile.singleLetter 'C' 3,  2u);
         (Tile.singleLetter 'D' 2,  8u);
         (Tile.singleLetter 'E' 1,  12u);
         (Tile.singleLetter 'F' 4,  2u);
         (Tile.singleLetter 'G' 2,  3u);
         (Tile.singleLetter 'H' 4,  2u);
         (Tile.singleLetter 'I' 1,  9u);
         (Tile.singleLetter 'J' 8,  1u);
         (Tile.singleLetter 'K' 5,  1u);
         (Tile.singleLetter 'L' 1,  4u);
         (Tile.singleLetter 'M' 3,  2u);
         (Tile.singleLetter 'N' 1,  6u);
         (Tile.singleLetter 'O' 1,  8u);
         (Tile.singleLetter 'P' 3,  2u);
         (Tile.singleLetter 'Q' 10, 1u);
         (Tile.singleLetter 'R' 1,  6u);
         (Tile.singleLetter 'S' 1,  4u);
         (Tile.singleLetter 'T' 1,  6u);
         (Tile.singleLetter 'U' 1,  4u);
         (Tile.singleLetter 'V' 4,  2u);
         (Tile.singleLetter 'W' 4,  2u);
         (Tile.singleLetter 'X' 8,  1u);
         (Tile.singleLetter 'Y' 4,  2u);
         (Tile.singleLetter 'Z' 10, 1u);
         (Tile.wildCard ['A'..'Z'], 2u)] |>
         List.map (fun (p, x) -> (p, x * scale))

    let tilesNoWildcards scale =

        [(Tile.singleLetter 'A' 1,  9u);
         (Tile.singleLetter 'B' 3,  2u);
         (Tile.singleLetter 'C' 3,  2u);
         (Tile.singleLetter 'D' 2,  8u);
         (Tile.singleLetter 'E' 1,  12u);
         (Tile.singleLetter 'F' 4,  2u);
         (Tile.singleLetter 'G' 2,  3u);
         (Tile.singleLetter 'H' 4,  2u);
         (Tile.singleLetter 'I' 1,  9u);
         (Tile.singleLetter 'J' 8,  1u);
         (Tile.singleLetter 'K' 5,  1u);
         (Tile.singleLetter 'L' 1,  4u);
         (Tile.singleLetter 'M' 3,  2u);
         (Tile.singleLetter 'N' 1,  6u);
         (Tile.singleLetter 'O' 1,  8u);
         (Tile.singleLetter 'P' 3,  2u);
         (Tile.singleLetter 'Q' 10, 1u);
         (Tile.singleLetter 'R' 1,  6u);
         (Tile.singleLetter 'S' 1,  4u);
         (Tile.singleLetter 'T' 1,  6u);
         (Tile.singleLetter 'U' 1,  4u);
         (Tile.singleLetter 'V' 4,  2u);
         (Tile.singleLetter 'W' 4,  2u);
         (Tile.singleLetter 'X' 8,  1u);
         (Tile.singleLetter 'Y' 4,  2u);
         (Tile.singleLetter 'Z' 10, 1u)] |>
         List.map (fun (p, x) -> (p, x * scale))