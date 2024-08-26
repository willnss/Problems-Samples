module TestProject.Twin

// Creates a function that given an array, it returns the index where
// if split in two-subarrays (last element of the first array has index of (foundIndex-1)), the sum of them are equal.

//Samples:
// code > twins([10, 20, 30, 5, 40, 50, 40, 15]) ➞ 5
// The foundIndex 5 : [10+20+30+5+40]=[50+40+15]

//Desired tests:
// Test.assertEquals(twins([1, 2, 3, 4, 5, 5]), 4)
// Test.assertEquals(twins([3, 3]), 1)
// Test.assertEquals(twins([10, 20, 30, 5, 40, 50, 40, 15]), 5)
// Test.assertEquals(twins([3, 4, 6, 7, 6]), 3)

let twinFn (arr: int list) =
    let total = List.sum arr
    if total % 2 <> 0 then
        None
    else
        let target = total / 2
        let rec findIx currentSum index remainingList =
            match remainingList with
            | [] -> None
            | head :: tail ->
                let newSum = currentSum + head
                if newSum = target then
                    Some (index + 1)
                else
                    findIx newSum (index + 1) tail
        findIx 0 0 arr