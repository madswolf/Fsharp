module Programfs
open System

// Exercise 2.1
let rec downto1 (n:int) : int list =
    if n > 0
        then n :: downto1 (n-1)
        else []

let rec downto2 (n:int) : int list =
    match n with
    | n when n > 0 -> n :: downto2 (n-1)
    | _ -> []

// Exercise 2.2
let removeOddIdx xs =
    let indicies = [0 .. List.length xs - 1]
    let listWithIndex = List.zip indicies xs
    let x, y = List.filter (fun (index, item) -> index % 2 <> 1) listWithIndex |> List.unzip
    y

// Exercise 2.3
let rec combinePair xs =
    match xs with
    | x1 :: x2 :: newList -> (x1, x2) :: combinePair newList
    | _ -> []
    
type complex = float * float 

let mkComplex x y = complex(x,y)

let complexToPair c = 
    let x,y = c
    (x,y)

let (|+|) c1 c2 =
    let a,b = c1
    let c,d = c2
    complex(a+c,b+d)

let (|*|) c1 c2 =
    let a,b = c1
    let c,d = c2
    complex(a*c - b*d, b*c + a*d)

let (~-.) p = 
    let a,b = p
    complex(-a,-b)

let (|-|) c1 c2 = 
    c1 |+| -.c2

let (~%) c =
    let a,b = c
    complex((a / (a*a + b*b)), ( -b / (a*a + b*b)))
        
let (|/|) c1 c2 =
    c1 |*| %c2

let explode1 (l:string) = 
    let arr = l.ToCharArray()
    
    let list = List.ofArray arr
    list

let rec explode2 (l:string) =
    match l with
    | "" -> [] 
    | s -> s.[0] :: explode2 (s.Remove (0,1)) 

let implode (arr: char list) = 
    let thing = List.foldBack (fun c s -> string c + s) arr ""
    thing

let implodeRev (arr: char list) = 
    let thing = List.fold (fun s c -> string c + s) "" arr
    thing

let toUpper s = 
    explode1 s |> List.map System.Char.ToUpper |> implode

let toUpper2 = explode1 >> List.map System.Char.ToUpper >> implode

let rec ack m = 
    match m with
    |0,n -> n+1
    |m,0 -> ack((m-1,1))
    |m,n -> ack(m-1,ack(m,n-1))

let time func = 
    let start = System.DateTime.Now
    let res = func ()
    let finish = System.DateTime.Now
    (res, finish - start)


let timeArg1 func a = 
    let thing = fun () -> func (a)
    time thing

let rec downto3 f n e =
    match n with
    | n when n > 0 -> downto3 f (n-1) (f n e)
    | n when n <= 0 -> e

let downto3alt (f: int -> 'a -> 'a) n e = 
    if n > 0 then
        let range = [1..n]
        let result = List.foldBack f range e
        result
    else
        e

let fac (n:int) :int = 
    let thingy = fun x y -> x * y
    downto3 thingy n 1

let range (g:(int -> 'a)) (n:int) :'a list =
    downto3 (g >> fun y z -> y :: z) n []

type word = (char * int) list

let hello:word = ('H',4)::('E',1)::('L',1)::('L',1)::[('O',2)];; 

type squareFun = word -> int -> int -> int

let Letterscore (word:word) (pos:int) (multi:int) (accumulator:int)  = snd (word.Item(pos)) * multi  + accumulator

let letterScoreGenerator = fun (n:int) -> (fun (word:word) pos accumulator -> Letterscore word pos n accumulator)

let (singleLetterScore:squareFun) = letterScoreGenerator 1
let (doubleLetterScore:squareFun) = letterScoreGenerator 2
let (tripleLetterScore:squareFun) = letterScoreGenerator 3

let (doubleWordScore:squareFun) =
    fun word pos accumulator -> accumulator * 2
let (tripleWordScore:squareFun) =
    fun word pos accumulator -> accumulator * 3

let isVowel c =
    match System.Char.ToLower c with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

let isConsonant c = 
    if System.Char.IsLetter c then 
        if isVowel c then false else true
    else false

let (oddConsonants:squareFun) = 
    fun word pos accumulator -> 
    if ((List.filter (fst >> isConsonant) word).Length % 2 = 1) then -accumulator else accumulator

type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;

let calculatePoints  (squares:square list)  (word:word)=
    let thing = List.mapi (fun index square ->  List.map (fun (innersquare:int * squareFun) ->  (fst innersquare,snd innersquare word index) ) square) squares|>
                List.fold (fun a b -> a @ b) [] |>
                List.sortBy(fun square -> fst square) |>
                List.map (fun square -> snd square) |>
                List.fold(fun acumulator square -> square << acumulator) (fun x -> x) 
    thing 0

[<EntryPoint>]
let main argv =
    
    // Test of Exercise 2.1
    let result1 = downto1 5
    let result2 = downto2 10
    let result3 = downto1 0
    let result4 = downto2 -42

    printfn "Exercise 2.1 \n
    Given downto1 5, returns %A \n
    Given downto2 10, returns %A \n
    Given downto1 0, returns %A \n
    Given downto1 -42, returns %A \n" result1 result2 result3 result4

    // Test of Exercise 2.2
    let result1 = removeOddIdx ([] : int list)
    let result2 = removeOddIdx [true]
    let result3 = removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]

    printfn "Exercise 2.2 \n
    Given removeOddIdx [] : int list returns %A \n
    Given removeOddIdx [true] returns %A \n
    Given removeOddIdx [Marry; had; a; little; lamb; its; fleece; was; white; as; snow] returns %A \n" result1 result2 result3

    // Test of Exercise 2.3
    let result1 = combinePair ([] : int list)
    let result2 = combinePair [true; false]
    let result3 = combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]

    printfn "Exercise 2.2 \n
    Given combinePair [] : int list returns %A \n
    Given combinePair [true; false] returns %A \n
    Given combinePair [Marry; had; a; little; lamb; its; fleece; was; white; as; snow] returns %A \n" result1 result2 result3

    // Test of Exercise 2.4
    let result1 = mkComplex 1.0 2.0
    let result2 = complexToPair result1
    let result3 = complex(2.0, 2.0) |+| complex(1.0, 1.0)
    let result4 = complex(2.0, 2.0) |*| complex(1.0, 1.0)
    let result5 = complex(2.0, 2.0) |-| complex(1.0, 1.0)
    let result6 = complex(2.0, 2.0) |/| complex(1.0, 1.0)

    printfn "Exercise 2.2 \n
    Given mkComplex 1.0 2.0 returns %A \n
    Given complexToPair result1 returns %A \n
    Given Complex(2.0, 2.0) |+| Complex(1.0, 1.0) returns %A \n
    Given Complex(2.0, 2.0) |*| Complex(1.0, 1.0) returns %A \n
    Given Complex(2.0, 2.0) |-| Complex(1.0, 1.0) returns %A \n    
    Given Complex(2.0, 2.0) |/| Complex(1.0, 1.0) returns %A \n" result1 result2 result3 result4 result5 result6

    0 // return an integer exit code
