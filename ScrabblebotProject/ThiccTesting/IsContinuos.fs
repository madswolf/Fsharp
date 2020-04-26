module ThiccScrabbleBot.IsContinuosTests
open System
open Xunit
open ValidityEngine
[<Fact>]
let isContinuosMove_given_horisontal_continuos_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let actual = isContinuosMove move true
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_continuos_hello_returns_true() =
    
    let move = [((0,0),'H');((0,-1),'E');((0,-2),'L');((0,-3),'L');((0,-4),'O')]
    let actual = isContinuosMove move false
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_continuos_shuffled_hello_returns_true() =
    
    let move = [((0,0),'H');((0,-1),'E');((0,-2),'L');((0,-3),'L');((0,-4),'O')]
    let actual = isContinuosMove move false
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_horisontal_continuos_shuffled_hello_returns_true() =
    
    let move = [((1,0),'H');((0,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let actual = isHorisontalMove move
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_horisontal_non_continuos_hello_returns_false() =
    
    let move = [((0,0),'H');((1,0),'E');((3,0),'L');((4,0),'O')]
    let actual = isContinuosMove move true
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_non_continuos_shuffled_hello_returns_false() =
    
    let move = [((0,0),'H');((0,-1),'E');((0,-3),'L');((0,-4),'O')]
    let actual = isContinuosMove move false
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_horisontal_non_continuos_shuffled_hello_returns_false() =
    
    let move = [((0,0),'H');((1,0),'E');((3,0),'L');((4,0),'O')]
    let actual = isContinuosMove move false
    let expected = false
    Assert.Equal(expected, actual)
