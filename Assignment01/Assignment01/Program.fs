open System

[<EntryPoint>]
let main argv =
    // Exercise 1.1
    let sqr x = x*x

    let result = sqr 5

    printfn "Exercise 1.1 \n
    Given 5, the result is: %i \n" result

    // Exercise 1.2
    let pow x n = System.Math.Pow(x,n)

    let result = pow 2.0 2.0

    printfn "Exercise 1.2 \n
    Given 2 and 2, the result is: %f \n" result

    // Exercise 1.3
    let rec sum = function
        | 1 -> 1
        | x -> x + sum(x-1)

    let result = sum 3

    printfn "Exercise 1.3 \n
    Given 3, the result is: %i \n" result

    // Exercise 1.4
    let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | x -> fib(x-1) + fib(x-2)

    let result = fib 4

    printfn "Exercise 1.4 \n
    Given 4, the result is: %i \n" result

    // Exercise 1.5
    let rec sum2 = function
        | m, 0 -> m
        | m, n -> m + n + sum2(m, n-1)

    let result = sum2 (3, 3)
    
    printfn "Exercise 1.5 \n
    Given 3 and 3, the result is: %i \n" result

    // Exercise 1.6
    let rec fact = function
        | 0 -> 1
        | n -> n * fact(n-1)

    let rec power = function
        | (x,0) -> 1.0
        | (x,n) -> x * power(x,n-1)
    (*
    - (System.Math.PI, fact -1) raises a Stack Overflow Exception, since it tries to take the factorial value of a negative number
       but is presumably still of type int.
    - fact(fact 4) is of type int.
    - power(System.Math.PI, fact 2) is of type float.
    - (power, fact) is of type (float * int -> float) * (int -> int).
    *)

    // Exercise 1.7
    let a = 5
    let f a = a + 1
    let g b = (f b) + a

    (*
    We're not sure exactly what's meant by "find the environment" but these are the bindings:
    a |-> 5
    f |-> a + 1 (this a is different than the a declared above that is also the a used below)
    g |-> (f b) + a
    *)

    let fvalue = f 3
    let gvalue = g 3

    // f 3 = 4 and g 3 = 9

    printfn "Exercise 1.7 \n
    Function f given 3 returns %i \n
    Function g given 3 returns %i \n" fvalue gvalue

    // Exercise 1.8
    let dup s = s + s

    let result = dup "hi "

    printfn "Exercise 1.8 \n
    Given hi returns %s \n" result

    // Exercise 1.9
    let rec dupn = function
        | s, 0 -> ""
        | s, 1 -> s + s
        | s, n -> dupn (s, n-1) + s

    let result = dupn ("hi ", 3)

    printfn "Exercise 1.9 \n
    Given hi and 3 returns %s \n" result

    // Exercise 1.10
    let timediff (t1h, t1m) (t2h, t2m) =
        let t1 = t1h * 60 + t1m
        let t2 = t2h * 60 + t2m
        t2-t1

    let result1 = timediff (12, 34) (11, 35)
    let result2 = timediff (12, 34) (13, 35)

    printfn "Exercise 1.10 \n
    Given (12, 34) and (11, 35) returns %i \n
    Given (12, 34) and (13, 35) returns %i \n" result1 result2

    // Exercise 1.11
    let minutes t = timediff (00, 00) t

    let result1 = minutes (14, 24)
    let result2 = minutes (23, 1)
    
    printfn "Exercise 1.11 \n
    Given (14, 24) returns %i \n
    Given (23, 1) returns %i \n" result1 result2

    // Exercise 1.12
    let rec bin = function
        | n, k when n = k || k = 0 -> 1
        | n, k when n > k -> bin (n-1, k-1) + bin (n-1, k)

    let result1 = bin (2, 1)
    let result2 = bin (4, 2)
    let result3 = bin (4, 4)
    

    printfn "Exercise 1.11 \n
    Given (2, 1) returns %i \n
    Given (4, 2) returns %i \n
    Given (4, 4) returns %i \n" result1 result2 result3

    // Exercise 1.13
    let rec f = function
        | (0,y) -> y
        | (x,y) -> f(x-1, x*y)

    (*
    1. f is of type int * int -> int
    2. f terminates only if given a non-negative integer as x-value.
       If given a negative x-value it will continue forever as it always calls itself with x-1, meaning it will never become 0.
    3. The evaluation steps for f(2, 3) is f(2, 3) -> f(1, 6) -> f(0, 6) -> 6
    4. It multiplies the factorial value of x with y.
    *)

    // Exercise 1.14
    // Where dis???

    // Exercise 1.15
    let test(c,e) = if c then e else 0

    (*
    1. The type of test is bool * int -> int
    2. test(false, fact(-1)) will result in a StackOverflow Exception, because it's strict and a function call is given as an argument.
    3. if false then fact -1 else 0 will return 0, because the if-statement is false so it never reaches the check of fact -1.
    *)

    // Exercise 1.16

    let curry f x y = f(x, y)

    let uncurry g (x, y) = g x y

    // Scrabble Assignments

    // Exercise 1.17

    let isVowel c =
        match System.Char.ToLower c with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false

    let result1 = isVowel 'I'
    let result2 = isVowel 'i'
    let result3 = isVowel 'Q'

    printfn "Exercise 1.17 \n
    Given I returns %b \n
    Given i returns %b \n
    Given Q returns %b \n" result1 result2 result3

    // Exercise 1.18

    let isConsonant c = 
        if System.Char.IsLetter c then 
            if isVowel c then false else true
        else false

    let result1 = isConsonant 'I'
    let result2 = isConsonant 'i'
    let result3 = isConsonant 'Q'

    printfn "Exercise 1.18 \n
    Given I returns %b \n
    Given i returns %b \n
    Given Q returns %b \n" result1 result2 result3

    // Exercise 1.19

    let empty (c: Char, v: int) =
        fun (x:int) -> (c, v)

    let theLetterA = empty ('A', 1)

    let result1 = theLetterA 0
    let result2 = theLetterA 42
    let result3 = theLetterA -762  

    printfn "Exercise 1.19 \n
    Given 0 returns %A \n
    Given 42 returns %A \n
    Given -762 returns %A \n" result1 result2 result3

    // Exercise 1.20
    
    let add (pos:int) (c:Char, v:int) (word:(int -> Char * int)) = function 
        | x when x = pos -> (c, v)
        | x -> word x

    let theLettersAB = add 1 ('B', 3) theLetterA

    let result1 = theLettersAB 0
    let result2 = theLettersAB 1
    let result3 = theLettersAB 42 

    printfn "Exercise 1.19 \n
    Given 0 returns %A \n
    Given 1 returns %A \n
    Given 42 returns %A \n" result1 result2 result3
    
    0 // return an integer exit code
