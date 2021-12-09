namespace AdventOfCode2021

module Day9 =

    let setup (lines:string[]) =
        lines
        |> Seq.map (Seq.map(fun c -> c.ToString() |> int)) 
        |> array2D

    let isInRange array (x,y) = 
        x >= 0 && y >=0 && x<(Array2D.length1 array) && y<(Array2D.length2 array)

    let getNeighbourCoords (x,y) array = 
        [(x - 1,y); (x + 1,y); (x, y - 1); (x, y + 1)] |> Seq.filter (isInRange array)
       
    let getValue (array:int[,]) (x,y) =
        array.[x,y]

    let isLowest (array:int[,]) (x,y)=
        let checkedValue = array.[x,y]
        getNeighbourCoords (x,y) array 
        |> Seq.map (getValue array)
        |> Seq.min 
        |> function
            | x when checkedValue < x -> Some(checkedValue)
            | _ -> None

    let flatten (array) = 
        seq {for x in 0..((Array2D.length1 array)-1) -> array[x,*]}
        |> Seq.collect id
        
    let getUpperNeighbours (array:int[,]) (x,y)=
        getNeighbourCoords (x,y) array
        |> Seq.filter (fun (i,j) -> 
            array.[i,j] 
            |> fun v ->  v< 9 && v > array.[x, y])
        
    let rec getBasin array points acc=
        match points with
        | s when Seq.isEmpty s -> acc
        | _ ->
            let newAcc = acc |> Seq.append points |> Seq.distinct
            let newPoints = 
                points
                |> Seq.collect (getUpperNeighbours array)
                |> Seq.distinct
                |> Seq.except newAcc
            getBasin array newPoints newAcc

    let rec getBasinSize (array:int[,]) (x,y) =
        getBasin array (seq{(x,y)}) (Seq.empty)
        |> Seq.distinct
        |> Seq.length

    let puzzle1 input =
        input
        |> setup
        |> fun arr -> Array2D.mapi(fun i j _ -> isLowest arr (i,j)) arr
        |> flatten 
        |> Seq.map(function Some(v) -> v + 1 |_ -> 0) 
        |> Seq.sum
           
    let puzzle2 input =
        let array = input |> setup
        array
        |> Array2D.mapi(fun i j _ ->
           match isLowest array (i,j) with
           | Some(_) -> Some(i,j)
           | _ -> None)
        |> flatten
        |> Seq.map (function Some(lowestPoint) -> getBasinSize array lowestPoint |_ -> 0)
        |> Seq.sortDescending
        |> Seq.take 3       
        |> Seq.fold (*) 1