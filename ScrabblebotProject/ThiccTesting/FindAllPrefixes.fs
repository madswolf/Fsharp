module ThiccScrabbleBot.findPrefixes

open System
open Xunit
open ValidityEngine
open GenerationEngine
open State
open System.IO
open ScrabbleUtil
open Dictionary

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let dict = 
    readLines "D:\code\Fsharp\ScrabblebotProject\EnglishDictionary.txt" |>
    Seq.fold (fun acc x ->  insert x acc) (empty " ")


[<Fact>]
let getPerpendicularWord_starting20_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [('H',(0,0));('E',(1,0));('L',(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = findPrefixes map dict 1
    let expected = [(("L", (2,0)),false);(("HEL", (2,0)),true);(("E", (1,0)),false);(("H", (0,0)),false)]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected
