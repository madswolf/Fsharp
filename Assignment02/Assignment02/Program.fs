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

// Exercise 2.4
type complex = Complex of float * float

let mkComplex x y =
    Complex(x, y)

let complexToPair (c:complex) =
    let (Complex(a, b)) = c
    (a, b)

let (|+|) c1 c2 =
    let (Complex(a, b)) = c1
    let (Complex(c, d)) = c2
    Complex(a + c, b + d)

let (|*|) c1 c2 =
    let (Complex(a, b)) = c1
    let (Complex(c, d)) = c2
    Complex(a * c - b * d, b * c + a * d)

let (~-.) p =
    let (Complex(a, b)) = p
    Complex(-a, -b)

let (|-|) c1 c2 =
    c1 |+| -.c2

let (~%) c =
    let (Complex(a, b)) = c
    Complex((a / (a * a + b * b)), (-b / (a * a + b * b)))

let (|/|) c1 c2 =
    c1 |*| %c2


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
    let result3 = Complex(2.0, 2.0) |+| Complex(1.0, 1.0)
    let result4 = Complex(2.0, 2.0) |*| Complex(1.0, 1.0)
    let result5 = Complex(2.0, 2.0) |-| Complex(1.0, 1.0)
    let result6 = Complex(2.0, 2.0) |/| Complex(1.0, 1.0)

    printfn "Exercise 2.2 \n
    Given mkComplex 1.0 2.0 returns %A \n
    Given complexToPair result1 returns %A \n
    Given Complex(2.0, 2.0) |+| Complex(1.0, 1.0) returns %A \n
    Given Complex(2.0, 2.0) |*| Complex(1.0, 1.0) returns %A \n
    Given Complex(2.0, 2.0) |-| Complex(1.0, 1.0) returns %A \n    
    Given Complex(2.0, 2.0) |/| Complex(1.0, 1.0) returns %A \n" result1 result2 result3 result4 result5 result6

    0 // return an integer exit code
