open System


let completeString count (s: string) = 
    match count with
    | 0 ->
        printfn "returning just nothing" 
        ""
    | x when (x - s.Length) = 0 -> 
        s
    | x when (x - s.Length) < 0 -> 
        printfn "Fuck, I cannot complete string %s to %d" s count
        s
    | _ -> (String.replicate (count - s.Length) "0") + s


let to8 = completeString 8

let to32 = completeString 32


let parseip (str: string) = 
    str.Split([|'.'|])
        |> Array.map System.Int32.Parse
        |> Array.toList

let unparseip (strs: int list) =
    let map = List.map (fun x -> x.ToString()) strs
    System.String.Join (".", map)


let unparsePrefix (pref: int) (strs: int list) =
    let map = List.map (fun x -> x.ToString()) strs
    System.String.Join (".", map) + "/" + pref.ToString()


let tobinary arr =
    List.map (fun (x: int) -> (to8 (System.Convert.ToString(x, 2)))) arr


let toint (i: string) = System.Convert.ToInt32(i, 2)


let intBin (a: int) = System.Convert.ToString(a, 2)


let uintBin (a: uint32) = System.Convert.ToString(System.Convert.ToInt64(a), 2)


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


let simplifyMask = tobinary >> List.map count1s >> List.reduce (+)


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
    //printfn "%s :: %A" str (count1s str, str.IndexOf('1'))
    (count1s str, str.IndexOf('1'))


let uintBin32 = uintBin >> to32


let ciscoMaskify (uint: uint32) (a, b) =
    [ for i in 1..(pown 2 a) -> uint ||| (uint32 i <<< (32 - b - a))]


let ciscoNets mask submask ip =
    let xored = XORmasks mask submask
    ciscoMaskify (toBigUInt ip)
        <| ( xored |> getBounds)
    |> List.map fromBigUInt

// ciscoNets (parseip "255.254.0.0") (parseip "255.255.254.0") (parseip "17.8.0.0");;


let classicMaskify (str: char list) (a, b) =
    let generate = intBin >> completeString a >> explode >> List.rev
    [
        for i in 1..((pown 2 a) - 1) ->
            str.[..b-1] @ generate i @ str.[b+a..]
    ]
    |> List.map charsToString


let classicNets mask submask ip =
    let xored = XORmasks mask submask
    classicMaskify (toBigUInt ip |> uintBin |> to32 |> explode)
        <| getBounds xored
    |> List.map (toint >> fromBigInt)

// classicNets (parseip "255.255.0.0") (parseip "255.255.255.0") (parseip "0.0.0.0");

type ipclass = A | B | C | D | NOPE

let getCategory = function
    | [x; _; _; _] when x < 127 -> A
    | [x; _; _; _] when x < 191 -> B
    | [x; _; _; _] when x < 223 -> C
    | [x; _; _; _] when x < 239 -> D
    | _ -> NOPE


let getDefaultMask ip =
    let category = getCategory ip
    match category with
    | A -> [255; 0; 0; 0]
    | B -> [255; 255; 0; 0]
    | C -> [255; 255; 255; 0]
    | _ -> failwith "wtf"


let broadcastNets mask submask ip =
    let xored = XORmasks mask submask
    classicMaskify (toBigUInt ip |> uintBin |> to32 |> explode)
        <| getBounds xored
    |> List.map (toint >> fromBigInt) |> List.rev |> List.head



let generateBitMask (max: int) =
    (pown 2 max) - 1



let broadcastHostsClassic mask submask ip =
    let xored = XORmasks mask submask
    let ones = (submask |> toBigUInt |> uintBin |> count0s)
    classicMaskify (((toBigUInt ip) ||| (uint32 <| generateBitMask ones)) |> uintBin |> to32 |> explode)
        <| getBounds xored
    |> List.map (toint >> fromBigInt)


let broadcastHostsCisco mask submask ip =
    let xored = XORmasks mask submask
    let ones = (submask |> toBigUInt |> uintBin |> count0s)
    ciscoMaskify ((toBigUInt ip) ||| (uint32 <| generateBitMask ones))
        <| getBounds xored
    |> List.map fromBigUInt



let generateVals max uip =
        [
            for x in 1..max -> uip ||| uint32 x
        ]


let listHostsi func mask submask ip i =
    let hosts: int list list = func mask submask ip
    let uip = toBigUInt hosts.[i-1]
    let usub = toBigUInt ip
    generateVals (uintBin uip |> count0s) uip
    |> List.map fromBigUInt

let prefix submask =
    (toBigUInt submask) |> uintBin32 |> count1s

let countsubnets mask submask =
    (pown 2 (XORmasks mask submask |> uintBin32 |> count1s)) - 2

let counthosts submask =
    (pown 2 ((toBigUInt submask) |> uintBin32 |> count0s)) - 2

let solve mask submask ip i =
    let pref = prefix submask
    printfn "Class: %A, default mask: %s" (getCategory ip) (unparseip <| getDefaultMask ip)
    printfn "Mask: %A" (tobinary mask)
    printfn "Subnet Mask: %A (/%d)" (tobinary submask) pref
    printfn "Possible subnets: %d and hosts: %d" (countsubnets mask submask) (counthosts submask)
    printfn "Networks: "
    printfn "   Classic: %A" (classicNets mask submask ip |> List.map (unparsePrefix pref))
    printfn "   Cisco: %A" (ciscoNets mask submask ip |> List.map (unparsePrefix pref))
    printfn "Hosts for %d subnet (cisco): %A" i (listHostsi ciscoNets mask submask ip i |> List.map unparseip)
    printfn "Hosts for %d subnet (classic): %A" i (listHostsi classicNets mask submask ip i |> List.map unparseip)
    printfn "Broadcast: "
    printfn "   all subnets: %A" ( (broadcastNets mask submask ip) |> unparseip)
    printfn "   all hosts (classic): %A" (broadcastHostsClassic mask submask ip |> List.map unparseip)
    printfn "   all hosts (cisco): %A" (broadcastHostsCisco mask submask ip |> List.map unparseip)


let go (mask: string) (submask: string) (ip: string) (i: int) =
    solve (parseip mask) (parseip submask) (parseip ip) i


go "255.255.255.0" "255.255.255.240" "143.1.37.0" 3;; // nastya 1

go "255.255.0.0" "255.255.252.0" "138.0.0.0" 4;; // nastya 3

go "255.255.0.0" "255.255.255.128" "12.85.0.0" 4;; // nastya 2

go "255.255.0.0" "255.255.248.0" "17.5.0.0" 3;;

