namespace Common

open System
open System.IO


module InputReader =
    let readFile filePath =
       File.ReadAllLines filePath

    let toInt sequence =
        sequence |> Seq.map System.Int32.Parse

module Tools =

    let flatten array = 
        seq {for x in 0..((Array2D.length1 array)-1) -> array[x,*]}
        |> Seq.collect id

    let isCoordinateInRange array (x,y) = 
        x >= 0 && y >=0 && x< (Array2D.length1 array) && y < (Array2D.length2 array)

    let getAdjacentCoordinates (x,y) array = 
        Seq.allPairs [x - 1..x + 1] [y-1..y+1] 
        |> Seq.except [(x,y)]
        |> Seq.filter (isCoordinateInRange array)

    let getAdjacentCoordinatesWithoutDiagonals (x,y) array = 
        [(x - 1,y); (x + 1,y); (x, y - 1); (x, y + 1)] |> Seq.filter (isCoordinateInRange array)