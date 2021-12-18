namespace AdventOfCode2021

open System
open Common.Types

module Day18 =

    type SnailfishNumber (left: SnailfishNumber option, right: SnailfishNumber option, value: byte option)=
        member val Left: SnailfishNumber option = left
        member val Right: SnailfishNumber option = right
        member val Value : byte option = value
        static member (+) (a:SnailfishNumber, n: SnailfishNumber) =
            new SnailfishNumber(Some(a), Some(n), None)

        with override this.ToString() =
            match this.Value with
            | Some(v) -> v.ToString()
            | _ -> "[" + this.Left.Value.ToString() + "," + this.Right.Value.ToString() + "]"
            

    let rec getNumber (acc:string) (bracketStack:string) (str:string) =
        let firstIteration = acc|>String.IsNullOrWhiteSpace
        
        if firstIteration then
            if str.StartsWith("[") then
                let strippedPair = str.Substring(1, str.Length-2)
                let firstNumber = getNumber "" "" strippedPair
                let secondNumber = getNumber "" "" (strippedPair.Remove(0,firstNumber.ToString().Length+1))
                new SnailfishNumber(Some(firstNumber),Some(secondNumber), None)
            else
                let ci = str.IndexOfAny([|',';']'|])
                
                let firstNumberString = if ci>0 then str.Substring(0,ci) else str
                new SnailfishNumber (None,None,Some(firstNumberString |> byte))
        else
            let symbol = str |> Seq.head |> string
            let rest = str |> Seq.tail |> String.Concat
            let newAcc = acc + symbol
            let newBracketStack =
                match symbol with
                |"[" -> symbol + bracketStack
                |"]" -> bracketStack.Substring(1)
                |_ -> bracketStack

            if newBracketStack|>String.IsNullOrWhiteSpace then 
                let firstNumberString = newAcc
                if rest.Length=0 then
                    getNumber "" "" firstNumberString
                else
                    new SnailfishNumber(Some(getNumber "" "" firstNumberString), Some(getNumber "" "" rest),None)
            else
                getNumber newAcc newBracketStack rest

    let rec parseSnailfishNumber line = getNumber "" "" line

    let rec findLeftmostNestedPair startPosition currentNestingLevel maxNestingLevel (line:string) = 
        if line.Length <= startPosition then
            None
        else
            let symbol = line.[startPosition] |> string
            let nextNestingLevel = 
                currentNestingLevel + 
                match symbol with
                    |"[" ->  1
                    |"]" -> -1
                    |_ ->    0
            if currentNestingLevel=maxNestingLevel then
                Some(startPosition)
            else
                findLeftmostNestedPair (startPosition + 1) nextNestingLevel maxNestingLevel line

    let explode (line:string) =
        findLeftmostNestedPair 0 0 5 line
        |> function
           |None -> line
           |Some(index) ->
            let pairToExplode = line.Substring(index, line.IndexOf(']',index)-index);
            let exploider = pairToExplode.Split(',',StringSplitOptions.RemoveEmptyEntries) |> Array.map byte |> fun a -> (a.[0], a.[1])
            let leftValueIndex = 
                line.Substring(0,index).LastIndexOfAny([|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';|])
                |> fun lastDigitIndex -> if lastDigitIndex < 0 then lastDigitIndex else (line.Substring(0,lastDigitIndex).LastIndexOfAny([|']';'[';','|]) + 1)
            let rightValueIndex = line.IndexOfAny([|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';|], index+pairToExplode.Length)
            line
            |> fun a -> 
                 if rightValueIndex > 0 then
                     let oldRightValueLength = a.IndexOfAny([|']';'[';','|],rightValueIndex) - rightValueIndex
                     let newRightValue = (a.Substring(rightValueIndex,oldRightValueLength) |> byte) + (snd exploider)
                     a.Remove(rightValueIndex, oldRightValueLength).Insert(rightValueIndex,newRightValue.ToString())
                 else a
            |> fun a ->
                 a.Remove(index-1, (pairToExplode.Length + 2)).Insert(index-1, "0")
            |> fun a ->
                 if leftValueIndex>0 then
                     let oldLeftLength = a.IndexOfAny([|']';'[';','|],leftValueIndex) - leftValueIndex
                     let newLeftValue = (a.Substring(leftValueIndex,oldLeftLength) |> byte) + (fst exploider)
                     a.Remove(leftValueIndex,oldLeftLength).Insert(leftValueIndex,newLeftValue.ToString())
                 else a
            

    let split (line:string) =
        line.Split([|"[";"]";","|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryFind (fun a -> a.Length>1)
        |> function 
            |None -> line
            |Some(value) -> 
                let newValue = 
                    value
                    |> float
                    |> fun v -> v / 2.0
                    |> fun divided -> (Math.Floor(divided) |> byte |> string, Math.Ceiling(divided) |> byte |> string)
                    |> fun (v1,v2) -> "[" + v1 + "," + v2 + "]"
                let index = line.IndexOf(value |> string)
                line.Remove(index, (value|>string).Length).Insert(index, newValue)

    let add (line1:string) (line2:string) =
        if line1.Length = 0 then 
            line2
        else
            "["+line1+","+line2+"]"
    
    let rec reduce line =
        let explodeResult = explode line
        if explodeResult <> line then
            reduce explodeResult
        else
            let splitResult = split line
            if line <> splitResult then
                reduce splitResult
            else
                line

    let sum line1 line2 =
        let result = add line1 line2
        reduce result

    let rec magnitute (number:SnailfishNumber) =
        match number.Value with
        | Some(v) -> v |> int
        | _ ->
            match (number.Left, number.Right) with
            | Some(left),Some(right) -> 3 * magnitute left + 2 * magnitute right
            | _ -> 0

    let puzzle1 (input) = 
            input 
            |> Array.tail 
            |> Array.fold sum (input|>Array.head)
            |> parseSnailfishNumber
            |> magnitute
           

    let puzzle2 input = 
            Array.allPairs input input 
            |> Array.filter(fun (v1,v2) -> v1 <> v2)
            |> Array.map(fun (v1, v2) -> (sum v1 v2) |> parseSnailfishNumber |> magnitute)
            |> Array.max
        
    let Solution = new Solution(18, puzzle1, puzzle2)