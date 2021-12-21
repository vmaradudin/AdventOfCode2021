namespace AdventOfCode2021

open System
open Common.Types
open System.IO
open Common.Tools

module Day15 =

    type Point =
        struct
            val X: int
            val Y: int
            new(x: int, y: int) = { X = x; Y = y }
    end

    type Node (point:Point, weight: int ) =
            member val Point: Point = point
            member val Weight: int = weight
            member val Unvisited : bool = true with get, set
            member val Distance : int = Int32.MaxValue with get, set

    let setup scale (lines:string[]) =
        let pattern = 
            lines
            |> Array.map (fun s -> (s|> Array.ofSeq|> Array.map (fun a -> a |> string |> int)))
            |> Array.transpose
            |> array2D
        let p1 = pattern |> Array2D.length1
        let p2 = pattern |> Array2D.length2
        Array2D.init (p1*scale) (p2*scale) (fun i j -> ((pattern.[i % p1,j % p2] + (i / p1) + (j / p2)) % 9 
                    |> fun a -> 
                        match a with 
                        | 0 -> 9 
                        | _ -> a))
        |> Array2D.mapi(fun i j v -> new Node(new Point(i,j), v))


    let findPath (matrix:Node[,]) =
        let size = matrix.GetUpperBound(0)+1;

        let neighborNodes (c:Node) =
            getAdjacentCoordinatesWithoutDiagonals (c.Point.X, c.Point.Y) matrix
            |> Seq.map (fun (x,y) -> matrix.[x,y])
        
        let StartNode = matrix.[0,0]
        let DestinationNode = matrix.[size-1, size-1];
        let mutable CurrentNode = StartNode;
        CurrentNode.Distance <- 0;
        let mutable touched:Set<int*Point> = Set.empty


        let AddTouched (node:Node) =
            touched<- touched|> Set.add (node.Distance, node.Point)
        let RemoveTouched (node:Node) =
            touched<- touched|> Set.remove (node.Distance, node.Point)
        AddTouched CurrentNode
        let NextUnvisited() =
            touched |> Set.minElement ||> fun _ point -> matrix.[point.X, point.Y] 

        while DestinationNode.Unvisited do
            let neighbors = neighborNodes CurrentNode
            neighbors 
            |> Seq.iter (fun a -> 
                    if a.Unvisited then 
                        a.Distance <- min (CurrentNode.Distance + a.Weight) a.Distance
                        AddTouched a)
            CurrentNode.Unvisited <- false;
            RemoveTouched CurrentNode
            if DestinationNode.Unvisited then
                CurrentNode <- NextUnvisited()

        DestinationNode.Distance

    let puzzle1 input = 
        input
        |> setup 1
        |> findPath
        
    
    let puzzle2 input = 
        input
        |> setup 5
        |> findPath

    let Solution = (new Solution(15, puzzle1, puzzle2) :> ISolution).Execute