module ThiccScrabbleBot.IsHorisontalTests

open System
open Xunit
open ValidityEngine

[<Fact>]
let IsHorisonal_given_horisontal_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let actual = isHorisontalMove move
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let IsHorisonal_given_vertical_hello_returns_false() =
    
    let move = [((0,1),'H');((0,0),'E');((0,2),'L');((0,3),'L');((0,4),'O')]
    let actual = isHorisontalMove move
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let IsHorisonal_given_vertical_shuffled_hello_returns_false() =
    
    let move = [((0,1),'H');((0,0),'E');((0,2),'L');((0,3),'L');((0,4),'O')]
    let actual = isHorisontalMove move
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let IsHorisonal_given_horisontal_shuffled_hello_returns_true() =
    
    let move = [((1,0),'H');((0,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let actual = isHorisontalMove move
    let expected = true
    Assert.Equal(expected, actual)



