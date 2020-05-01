module ThiccScrabbleBot.IsContinuosTests
open System
open Xunit
open ValidityEngine
[<Fact>]
let isContinuosMove_given_horisontal_continuos_hello_returns_true() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0));((4,0),('O',0))]
    let actual = isContinuosMove move true
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_continuos_hello_returns_true() =
    
    let move = [((0,0),('H',0));((0,-1),('E',0));((0,-2),('L',0));((0,-3),('L',0));((0,-4),('O',0))]
    let actual = isContinuosMove move false
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_continuos_shuffled_hello_returns_true() =
    
    let move = [((0,0),('H',0));((0,-1),('E',0));((0,-2),('L',0));((0,-3),('L',0));((0,-4),('O',0))]
    let actual = isContinuosMove move false
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_horisontal_non_continuos_hello_returns_false() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((3,0),('L',0));((4,0),('O',0))]
    let actual = isContinuosMove move true
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_vertical_non_continuos_shuffled_hello_returns_false() =
    
    let move = [((0,0),('H',0));((0,-1),('E',0));((0,-3),('L',0));((0,-4),('O',0))]
    let actual = isContinuosMove move false
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_horisontal_non_continuos_shuffled_hello_returns_false() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((3,0),('L',0));((4,0),('O',0))]
    let actual = isContinuosMove move false
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isContinuosMove_given_elbow_non_continuos_shuffled_hello_returns_false() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((2,1),('O',0))]
    let actual = isContinuosMove move false
    let expected = false
    Assert.Equal(expected, actual)


