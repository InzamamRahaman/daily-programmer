
let fixWhitespace (str : string) = 
    str.Trim() + " "

let (|StartBrack|EndBrack|NotABrack|) ch = 
    match ch with
    | '(' | '{' | '[' -> StartBrack
    | ')' | '}' | ']' -> EndBrack
    | _ -> NotABrack

let mapCombine def op map key value = 
    match Map.tryFind key map with
    | None -> map |> Map.add key (op def value)
    | Some x -> map |> Map.add key (op x value)

let combine = mapCombine "" (fun x y -> x + (fixWhitespace y))

let accumulate (map, str, level) ch = 
    match ch with
    | StartBrack -> (combine map level str, "", level + 1)
    | EndBrack -> (combine map level str, "", level - 1)
    | NotABrack -> (map, str + ch.ToString(), level)

let myFst (a, b, c) = a

let extractPhrases xs = 
    xs |> Seq.fold accumulate (Map.empty, "", 0) |> myFst |> Map.toList
       |> List.rev |> List.map snd |> List.reduce (+) 
       |> (fun str -> str.Trim())

