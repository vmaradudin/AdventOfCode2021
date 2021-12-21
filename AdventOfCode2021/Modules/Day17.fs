namespace AdventOfCode2021

open System
open Common.Types

module Day17 =

    type Packet(version:int64, typeId: int64, value:int64, children:Packet[]) =
        member val Version: byte = version |> byte
        member val TypeId: byte = typeId |> byte
        member val Value : int64 = value
        member val Children : Packet[] = children with get, set

    let setup (lines:string[]) =
        lines
        |> Array.head
        |> fun a -> a.Replace("target area: x=","").Replace(" y=","").Split([|"..";","|], StringSplitOptions.RemoveEmptyEntries)
        |> fun a -> a|>Array.map int
    
    let isWithinTargetArea (x1,x2) (y1,y2) (x,y)=
        (x>=x1) && (x<=x2) && (y>=y1) && (y<=y2)

    let rec move (pX,pY) (vX,vY) (rX1,rX2) (rY1,rY2) maxY=
        let newMaxY= max maxY pY
        match (pX,pY) with
            | (x,y) when (x,y) |> isWithinTargetArea (rX1,rX2) (rY1,rY2) -> Some(maxY)
            | (x,y) when y < rY1 -> None
            |_ ->
                let newPosition = (pX + vX), (pY + vY)
                let newVelocity = 
                    ((match vX with
                        | x when x>0 -> x-1
                        | x when x<0 -> x+1
                        |_ -> 0), (vY - 1))
                move newPosition newVelocity (rX1,rX2) (rY1,rY2) newMaxY

    let findOptions (rX1,rX2) (rY1,rY2)=
            seq{for x in (-rX2)..rX2 do
                 for y in rY1..(-rY1) do
                  match move (0,0) (x,y) (rX1,rX2) (rY1,rY2) Int32.MinValue with 
                  |Some(maxY) -> yield ((x,y),maxY) 
                  |_ -> ignore}

    let puzzle1 (input:string[]) = 
            input
            |> setup
            |> fun a -> findOptions (a.[0],a.[1]) (a.[2],a.[3])
            |> Seq.maxBy snd
            |> snd
    
    let puzzle2 input = 
           input
           |> setup
           |> fun a -> findOptions (a.[0],a.[1]) (a.[2],a.[3])
           |> Seq.map fst
           |> Seq.distinct
           |> Seq.length
        
    let Solution = (new Solution(17, puzzle1, puzzle2) :> ISolution).Execute