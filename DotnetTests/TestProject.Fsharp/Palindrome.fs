module TestProject.Palindrome
open Operators
(*
A string is an almost-palindrome if, by changing only one character, you can make it a palindrome.
Create a function that returns true if a string is an almost-palindrome and false otherwise.

AlmostPalindrome("abcdcbg") ➞ true
// Transformed to "abcdcba" by changing "g" to "a".

AlmostPalindrome("abccia") ➞ true
// Transformed to "abccba" by changing "i" to "b".

AlmostPalindrome("abcdaaa") ➞ false
// Can't be transformed to a palindrome in exactly 1 turn.

AlmostPalindrome("1234312") ➞ false
*)

//original
let almostPalindromeFnOriginal (text: string) =
    let reversed = new string(Seq.rev text |> Array.ofSeq)
    let limit = text.Length / 2
    let rec checkAlmostPalindrome iteration limit accDiff (originalText: string) (reversedText: string) =
        if (iteration < limit) then
            if originalText.[iteration].Equals(reversedText.[iteration]) then
                checkAlmostPalindrome (iteration + 1) limit accDiff originalText reversedText
            else
                checkAlmostPalindrome (iteration + 1) limit (accDiff + 1) originalText reversedText
        else
            accDiff <= 1
    checkAlmostPalindrome 0 limit 0 text reversed
    
// melhorada (funcional)
let almostPalindromeFn (text: string) =
    let limit = text.Length / 2
    let reversed = Seq.rev text |> Seq.toArray
    
    // val zip: source1 'T1 seq -> source2 'T2 seq -> ('T1 * 'T2) seq
    Seq.zip text reversed
    |> Seq.take limit
    |> Seq.fold (fun acc (originalChar, reversedChar) -> 
        if originalChar = reversedChar then acc else acc + 1) 0
    |> (fun accDiff -> accDiff <= 1)