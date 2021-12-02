namespace Common
open System.IO

module InputReader =
    let readFile filePath =
       File.ReadAllLines filePath

    let toInt sequence =
        sequence |> Seq.map System.Int32.Parse