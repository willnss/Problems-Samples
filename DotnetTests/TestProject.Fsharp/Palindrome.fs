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
    // reverto a string com Seq (equivalente IEnumerable) / Array
    let reversed = new string(Seq.rev text |> Array.ofSeq)
    // meio da string pra comparar a porção que faça sentido e contar quantos carcteres nao sao validos
    let limit = text.Length / 2
    // func acumuladora pra percorrer as strings originais até o limite e aplicar ao final a decisao
    let rec checkAlmostPalindrome iteration limit accDiff (originalText: string) (reversedText: string) =
        if (iteration < limit) then
            // se carcter igual nao contabilizo caracter divergente, caso contrario contabilizo 
            if originalText.[iteration].Equals(reversedText.[iteration]) then
                checkAlmostPalindrome (iteration + 1) limit accDiff originalText reversedText
            else
                checkAlmostPalindrome (iteration + 1) limit (accDiff + 1) originalText reversedText
        else
            // decisao se é palindromo ou quase palindromo
            accDiff <= 1
    // chama a func
    checkAlmostPalindrome 0 limit 0 text reversed
    
// melhorada (funcional)
let almostPalindromeFn (text: string) =
    // limite pra comparação e nao contar duas vezes caracter incompativel 
    let limit = text.Length / 2
    // reverte o texto
    let reversed = Seq.rev text |> Seq.toArray

    // operador |> chamado de pipe captura o valor a esquerda
    // e passa como parametro para a função a direita

    // crio uma sequência Seq de tupulas onde cada uma contr um char do texto origem e reverso
    // val zip: source1 'T1 seq -> source2 'T2 seq -> ('T1 * 'T2) seq
    Seq.zip text reversed
    // filtro pra ter so metade 
    |> Seq.take limit
    // função acumuladora como o reduce
    |> Seq.fold (fun acc (originalChar, reversedChar) -> 
        if originalChar = reversedChar then acc else acc + 1) 0
    // jogo o resultado para a função que decide se é quase palindromo
    |> (fun accDiff -> accDiff <= 1)
