namespace AdventOfCode2021

open System
open Common.Types

module Day16 =

    type Packet(version:int64, typeId: int64, value:int64, children:Packet[]) =
        member val Version: byte = version |> byte
        member val TypeId: byte = typeId |> byte
        member val Value : int64 = value
        member val Children : Packet[] = children with get, set

    let setup (lines:string[]) =
        lines
        |> Array.head

    let hexTobin (v:char) =
        match v with
            |'0' -> "0000"
            |'1' -> "0001"
            |'2' -> "0010"
            |'3' -> "0011"
            |'4' -> "0100"
            |'5' -> "0101"
            |'6' -> "0110"
            |'7' -> "0111"
            |'8' -> "1000"
            |'9' -> "1001"
            |'A' -> "1010"
            |'B' -> "1011"
            |'C' -> "1100"
            |'D' -> "1101"
            |'E' -> "1110"
            |'F' -> "1111"

    let binToDec b =
        (b, (0L,1L))
        ||> Seq.foldBack(fun c (d, p) -> d + (c|>string|>int64)*p, p*2L)
        |> fst

    let rec parsePacket (binString:string) : Packet*string =
        
        let rec readLiteralPacket acc (input:string) =
            let portion = input.Substring(0,5)
            let value = portion.Substring(1)
            let signalBit = portion.[0]
            let result = acc + value
            let restInput = input.Substring(5)
            match signalBit with
            | '0' -> (result |> binToDec, restInput)
            | _ -> readLiteralPacket result restInput
    
        let rec readNumberOfPackets number (acc:Packet[]) (input:string) =
            match number with
            | 0 -> (acc,input)
            | v when v<0  && ((input.TrimStart('0')) |> String.IsNullOrWhiteSpace) -> (acc,"")
            | _ -> (parsePacket input)
                |> fun (packet, restInput) -> readNumberOfPackets (number - 1) ( [|packet|]|>Array.append acc) restInput

        let rec readOperatorPacket acc (input:string) =
            let lengthTypeId = input.[0]
            let length =
                match lengthTypeId with
                | '0' -> input.Substring(1,15) |> binToDec |> int
                | '1' -> input.Substring(1,11) |> binToDec |> int
            match lengthTypeId with
                | '0' -> (readNumberOfPackets -1 Array.empty (input.Substring(16,length))) ||> fun packets restInput -> packets, input.Substring(16+length)
                | '1' -> readNumberOfPackets length Array.empty (input.Substring(12))


        let packetVersion = binString.Substring(0,3) |> binToDec
        let packetTypeId = binString.Substring(3,3) |> binToDec
        let packetContent = binString.Substring(6)
        match packetTypeId with
            | 4L -> (readLiteralPacket "" packetContent) ||> fun value restInput -> ((new Packet(packetVersion, packetTypeId, value,Array.empty)),restInput)
            | _ -> (readOperatorPacket "" packetContent) ||> fun childPackets restInput -> ((new Packet(packetVersion, packetTypeId, 0L, childPackets)), restInput)

    let decode (line:string) =
            line
            |> Array.ofSeq
            |> Array.map hexTobin
            |> String.Concat
            |> parsePacket
            |> fst
        
    let rec sumVersions (packet:Packet) =
         ((packet.Version|>int) + (packet.Children |> Array.sumBy sumVersions))

    let rec calculate (packet: Packet) =
        match packet.TypeId with
        |4uy -> packet.Value
        |0uy -> packet.Children|> Array.sumBy calculate
        |1uy -> packet.Children|> Array.fold (fun acc v -> acc*(calculate v)) 1L
        |2uy -> packet.Children|> Array.map calculate |> Array.min
        |3uy -> packet.Children|> Array.map calculate |> Array.max
        |5uy -> packet.Children|> Array.map calculate |> fun a -> if (a|> Array.head) > (a|>Array.last) then 1 else 0
        |6uy -> packet.Children|> Array.map calculate |> fun a -> if (a|> Array.head) < (a|>Array.last) then 1 else 0
        |7uy -> packet.Children|> Array.map calculate |> fun a -> if (a|> Array.head) = (a|>Array.last) then 1 else 0

    let puzzle1 (input:string[]) = 
            input
            |> setup
            |> decode
            |> sumVersions
        
    
    let puzzle2 input = 
            input
            |> setup
            |> decode
            |> calculate
        
    let Solution = new Solution(16, puzzle1, puzzle2)