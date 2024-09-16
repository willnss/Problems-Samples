module TestProject.RotateTransform

(*
Create a function to rotate a two-dimensional matrix of "N * N" integer elements "num" times, 
where if "num" is positive, the rotation is clockwise, and if not, counterclockwise.
*)
let rotateTransform (matrix: int[][]) num =
    // Normaliza o número de rotações para estar no intervalo de 0 a 3
    let rotations = (num % 4 + 4) % 4
    
    // Funções auxiliares de rotação
    let rotateClockwise (m: int[][]) =
        let rows = Array.length m
        let cols = Array.length m.[0]
        Array.init cols (fun i -> Array.init rows (fun j -> m.[rows - j - 1].[i]))

    let rotateCounterClockwise (m: int[][]) =
        let rows = Array.length m
        let cols = Array.length m.[0]
        Array.init cols (fun i -> Array.init rows (fun j -> m.[j].[cols - i - 1]))

    // Aplica rotações dependendo do valor de "rotations"
    match rotations with
    | 0 -> matrix
    | 1 -> rotateClockwise matrix
    | 2 -> rotateClockwise (rotateClockwise matrix)
    | 3 -> rotateCounterClockwise matrix
    | _ -> matrix  // Isso nunca ocorrerá devido à normalização anterior
