module Tests

open Xunit
open TestProject.Twin
    
[<Fact>]
let rec ``Twin 4`` () =
    let arr = [1; 2; 3; 4; 5; 5]
    let result = twinFn arr
    Assert.Equal(Some 4, result)
    
[<Fact>]
let rec ``Twin 1`` () =
    let arr = [3; 3]
    let result = twinFn arr
    Assert.Equal(Some 1, result)
    
[<Fact>]
let rec ``Twin 5`` () =
    let arr = [10; 20; 30; 5; 40; 50; 40; 15]
    let result = twinFn arr
    Assert.Equal(Some 5, result)
    
[<Fact>]
let rec ``Twin 3`` () =
    let arr = [3; 4; 6; 7; 6]
    let result = twinFn arr
    Assert.Equal(Some 3, result)