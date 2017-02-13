open System


let completeString count (s: string) = 
    (String.replicate (count - s.Length) "0") + s

let to8 = completeString 8

let to32 = completeString 32


let parseip (str: string) = 
    str.Split([|'.'|])
        |> Array.map System.Int32.Parse
        |> Array.toList

let unparseip (strs: int list) =
    let map = List.map (fun x -> x.ToString()) strs
    System.String.Join (".", map)

let tobinary arr =
    List.map (fun (x: int) -> (to8 (System.Convert.ToString(x, 2)))) arr

let toint (i: string) = System.Convert.ToInt32(i, 2)

let intBin (a: int) = System.Convert.ToString(a, 2)

let uintBin (a: uint32) = System.Convert.ToString(System.Convert.ToInt32(a), 2)

let n = 16
let k = 6

let explode (s:string) =
        [for c in s -> c]

let count1s str = explode str |> List.filter (fun x -> x = '1') |> List.length

let count0s str = explode str |> List.filter (fun x -> x = '0') |> List.length


let fromBigInt bgint = 
        List.rev [ for i in 0 .. 3 -> (bgint >>> (i * 8)) &&& 255 ]

let toBigInt ilist = 
    List.reduce (fun acc el -> (acc <<< 8) ||| el) ilist

let fromBigUInt (bgint: uint32) = 
    List.rev [ for i in 0 .. 3 -> int ((bgint >>> (i * 8)) &&& (uint32 255)) ]

let toBigUInt ilist = 
    List.fold (fun (acc: uint32) (el: int) -> (acc <<< 8) ||| System.Convert.ToUInt32(el)) (uint32 0) ilist

let XORmasks a b = 
    (toBigUInt a) ^^^ (toBigUInt b)
let simplifyMask = tobinary

let charsToString (a: char list) = List.fold (fun str x -> str + x.ToString()) "" a



let unsimplifyMask zeros =
    [ for i in 1 .. (32 - zeros) -> '1' ] @ [ for i in 1 .. zeros -> '0' ]

let split10string (lst: char list) =
    let rec splitmore (l: char list) acc = 
        match l with
        | [] -> acc
        | x when List.length x >= 8  -> splitmore l.[8..] ((charsToString l.[0..7]) :: acc)
        | x -> failwith (sprintf "This is bullshit: %A" x)
    splitmore lst [] |> List.rev

let decimalizeMask = unsimplifyMask >> split10string >> List.map toint

let getBounds xoreduint = 
    let str = uintBin xoreduint |> to32
    (count1s str, str.IndexOf('1'))


(XORmasks (parseip "255.255.0.0") (parseip "255.255.255.0")) ^^^ uint32 (255 <<< 16 - 8) |> uintBin |> to32