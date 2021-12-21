namespace AdventOfCode2021

open System
open Common.Types

module Day19 =

    type Point3D =
        struct
            val x: int
            val y: int
            val z: int
            val isScanner: bool
            new(x: int, y:int, z:int) = { x=x; y =y; z=z; isScanner=false }
            new(x: int, y:int, z:int, isScanner:bool) = { x=x; y =y; z=z; isScanner=isScanner }
            static member (-) (a:Point3D, n: Point3D) =
                       new Point3D(a.x - n.x, a.y - n.y, a.z - n.z, a.isScanner)
            static member (+) (a:Point3D, n: Point3D) =
                new Point3D(a.x + n.x, a.y + n.y, a.z + n.z, a.isScanner)
        end
        with override this.ToString()=
            (if this.isScanner then "S| " else "") + this.x.ToString() + " , " + this.y.ToString() + " , " + this.z.ToString()

    let rec setup (acc) (input:string[])=
        let part = 
            input 
            |> Array.skipWhile (fun line -> line |> String.IsNullOrWhiteSpace || line.StartsWith("---"))
            |> Array.takeWhile (fun line -> line |> String.IsNullOrWhiteSpace |> not)
            |> Array.map(fun a -> a.Split(",")|> fun aa -> new Point3D(aa.[0]|> int,aa.[1]|> int,aa.[2]|> int ))
            
        if (part |> Array.isEmpty) then
            acc
        else
            let remainder = 
                input
                |> Array.skipWhile (fun line -> line |> String.IsNullOrWhiteSpace || line.StartsWith("---"))
                |> Array.skipWhile (fun line -> line |> String.IsNullOrWhiteSpace |> not)
            setup ([|part|]|> Array.append acc) remainder
            

    let allProjections =
        [|
            fun (p:Point3D) -> Point3D(p.x, p.y, p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(p.x, -p.z, p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(p.x, -p.y, -p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(p.x, p.z, -p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.y, p.x, p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(p.z, p.x, p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(p.y, p.x, -p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.z, p.x, -p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.x, -p.y, p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.x, -p.z, -p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.x, p.y, -p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.x, p.z, p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(p.y, -p.x, p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(p.z, -p.x, -p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.y, -p.x, -p.z, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.z, -p.x, p.y, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.z, p.y, p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(p.y, p.z, p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(p.z, -p.y, p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.y,-p.z, p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.z, -p.y, -p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(-p.y, p.z, -p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(p.z, p.y, -p.x, p.isScanner)
            fun (p:Point3D) -> Point3D(p.y, -p.z, -p.x, p.isScanner)
        |]

    let getProjection (projection: Point3D -> Point3D) (scannerData:Point3D[]) =
        scannerData |> Array.map projection
        

    let intersect (scannerData1:Point3D[]) (scannerData2:Point3D[])=
        Array.allPairs (scannerData1 |> Array.filter (fun p -> not p.isScanner)) (scannerData2|> Array.filter (fun p -> not p.isScanner))
        |> Array.map (fun (point1, point2) -> point2 - point1)
        |> Array.countBy id
        |> Array.tryFind(fun (_, count) -> count >= 12)
        |> function
            |Some(shift, _) -> Some(shift)
            |_ -> None 

    let merge (scannerData1:Point3D[]) (scannerData2:Point3D[])=
        allProjections
        |> Array.tryPick(fun projection -> (getProjection projection scannerData2) |> fun projectedScanner2 -> projectedScanner2 |> intersect scannerData1 |> function |Some(shift) -> Some(shift,projectedScanner2) |_ -> None)
        |> function
            | Some(shift, projection) -> Some((projection|> Array.map(fun point -> point - shift) |> Array.append scannerData1) |> Array.distinct |> Array.sort)
            | _ -> None
    
    let rec map (scannersData:Point3D[][])=
        if scannersData.Length = 1 then
            scannersData.[0]
        else
                scannersData.[1..] 
                |> Array.map(fun s2 -> 
                    merge scannersData.[0] s2 
                    |> function
                        |Some(mergeResult) -> mergeResult, true
                        |_ -> s2, false
                    )
                |> fun array -> ((array |> Array.filter (fun (_, merged) -> merged) |> Array.map fst)|> fun a -> if a.Length>0 then a else [|scannersData.[0]|])
                                |> Array.append (array |> Array.filter (fun (_,merged) -> not merged) |> Array.map fst)
                                |> map

    let getManhattanDistance (point1:Point3D) (point2:Point3D) =
        abs((point1.x - point2.x)) + abs((point1.y - point2.y)) + abs((point1.z - point2.z))

    let puzzle1 (input) = 
        let s = 
            input 
            |> setup Array.empty
            |> Array.map (fun sd -> sd |> Array.append [|new Point3D(0,0,0,true)|])
            |> map

        let scanners = 
            s 
            |> Array.filter (fun a -> a.isScanner)

        s 
        |> Array.filter (fun a -> not a.isScanner) 
        |> Array.length

    let puzzle2 input = 
        let s = 
            input 
            |> setup Array.empty
            |> Array.map (fun sd -> sd |> Array.append [|new Point3D(0,0,0,true)|])
            |> map

        let scanners = 
            s 
            |> Array.filter (fun a -> a.isScanner)

        let scannersMD =
            Array.allPairs scanners scanners
            |> Array.map(fun (v1,v2) -> getManhattanDistance v1 v2)
            |> Array.max

        scannersMD
        
    let Solution = (new Solution(19, puzzle1, puzzle2) :> ISolution).Execute