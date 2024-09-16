module Tests

open Xunit
open TestProject.Twin
open TestProject.Palindrome
open TestProject.RotateTransform
    
// Twin

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
    
// AlmostPalindrome
    
[<Fact>]
let rec ``Almost Palindrome 1`` () =
    Assert.True(almostPalindromeFn "abcdcbg")
 
[<Fact>]
let rec ``Almost Palindrome 2`` () =
    Assert.True(almostPalindromeFn "abccia")   
    
[<Fact>]
let rec ``Almost Palindrome 3`` () =
    Assert.False(almostPalindromeFn "abcdaaa")
 
[<Fact>]
let rec ``Almost Palindrome 4`` () =
    Assert.False(almostPalindromeFn "1234312")
    
// RotateTransform

let toArray2D (list: int list list) : int[][] =
    list |> List.map List.toArray |> List.toArray
    
let areEqualArrays (arr1: int[][]) (arr2: int[][]) =
    // Verifica se o número de linhas é o mesmo
    if arr1.Length <> arr2.Length then
        false
    else
        // Verifica se cada linha é igual usando Array.forall2
        Array.forall2 (fun row1 row2 -> Array.forall2 (=) row1 row2) arr1 arr2

[<Fact>]
let ``Test 1: Rotate 2x2 matrix clockwise once`` () =
    let input = toArray2D [[2; 4]; [0; 0]]
    let expected = toArray2D [[0; 2]; [0; 4]]
    let result = rotateTransform input 1
    areEqualArrays expected result |> Assert.True

[<Fact>]
let ``Test 2: Rotate 2x2 matrix counterclockwise once`` () =
    let input = toArray2D [[2; 4]; [0; 0]]
    let expected = toArray2D [[4; 0]; [2; 0]]
    let result = rotateTransform input -1
    areEqualArrays expected result |> Assert.True

[<Fact>]
let ``Test 3: Rotate 4x4 matrix clockwise twice`` () =
    let input = toArray2D [
        [1; 4; 0; 0];
        [2; 8; 0; 0];
        [0; 0; 3; 5];
        [0; 0; 7; 1]
    ]
    let expected = toArray2D [
        [1; 7; 0; 0];
        [5; 3; 0; 0];
        [0; 0; 8; 2];
        [0; 0; 4; 1]
    ]
    let result = rotateTransform input 2
    areEqualArrays expected result |> Assert.True

[<Fact>]
let ``Test 4: Rotate 4x4 matrix counterclockwise twice`` () =
    let input = toArray2D [
        [4; 3; 1; 2];
        [2; 1; 3; 4];
        [0; 0; 0; 0];
        [0; 0; 0; 0]
    ]
    let expected = toArray2D [
        [0; 0; 0; 0];
        [0; 0; 0; 0];
        [4; 3; 1; 2];
        [2; 1; 3; 4]
    ]
    let result = rotateTransform input -2
    areEqualArrays expected result |> Assert.True

[<Fact>]
let ``Test 5: Rotate 6x6 matrix clockwise five times`` () =
    let input = toArray2D [
        [2; 3; 5; 0; 0; 0];
        [1; 7; 1; 0; 0; 0];
        [5; 3; 2; 0; 0; 0];
        [0; 0; 0; 1; 3; 4];
        [0; 0; 0; 2; 8; 2];
        [0; 0; 0; 4; 3; 1]
    ]
    let expected = toArray2D [
        [0; 0; 0; 5; 1; 2];
        [0; 0; 0; 3; 7; 3];
        [0; 0; 0; 2; 1; 5];
        [4; 2; 1; 0; 0; 0];
        [3; 8; 3; 0; 0; 0];
        [1; 2; 4; 0; 0; 0]
    ]
    let result = rotateTransform input 5
    areEqualArrays expected result |> Assert.True