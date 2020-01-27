open System

[<EntryPoint>]
let main argv =
    // Exercise 1.1
    let sqr x = x*x
    let result = sqr 5

    printfn "Exercise 1.1 \nGiven 5, the result is: %i \n" result

    // Exercise 1.2
    let pow x n = System.Math.Pow(x,n)
    let result = pow 2.0 2.0

    printfn "Exercise 1.2 \nGiven 2 and 2, the result is: %f \n" result

    // Exercise 1.3
    let rec sum = function
    | 1 -> 1
    | x -> x + sum(x-1)
    let result = sum 3

    printfn "Exercise 1.3 \nGiven 3, the result is: %i \n" result

    // Exercise 1.4
    let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | x -> fib(x-1) + fib(x-2)
    let result = fib 4

    printfn "Exercise 1.4 \nGiven 4, the result is: %i \n" result

    // Exercise 1.5
    let rec sum2 = function
    | m, 0 -> m
    | m, n -> m + n + sum2(m, n-1)
    let result = sum2 (3, 3)
    
    printfn "Exercise 1.5 \nGiven 3 and 3, the result is: %i \n" result

    (*
       Exercise 1.6

    (System.Math.PI, fact -1) raises a Stack Overflow Exception, since it tries to take the factorial value of a negative number
    but is presumably still of type int
    fact(fact 4) is of type int
    power(System.Math.PI, fact 2) is of type float
    (power, fact) is of type (float * int -> float) * (int -> int)
    *)

    0 // return an integer exit code
