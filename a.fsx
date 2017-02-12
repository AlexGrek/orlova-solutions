open System

let parseip (str: string) = 
    str.Split([|'.'|])
        |> Array.map System.Int32.Parse
        |> Array.toList



let tobinary arr =
    List.map (fun (x: int) -> System.Convert.ToString(x, 2)) arr


let intBin (a: int) = System.Convert.ToString(a, 2)

let n = 16

let k = 6

let explode (s:string) =
        [for c in s -> c]

let count1s str = explode str |> List.filter (fun x -> x = '1') |> List.length;;

let toBigInt ilist = 
    List.reduce (fun acc el -> (acc <<< 8) ||| el) ilist