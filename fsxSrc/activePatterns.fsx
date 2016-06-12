

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let testNumber input =
    match input with 
    | Even -> printfn "%d is even" input
    | Odd  -> printfn "%d is odd"  input

testNumber 3
testNumber 100

let (|Int|_|) str =
    match System.Int32.TryParse(str) with 
    | (true, int) -> Some(int)
    | _ -> None

let (|Bool|_|) str =
    match System.Boolean.TryParse(str) with 
    | (true, bool) -> Some(bool)
    | _ -> None

let testParse str =
    match str with 
    | Int i  -> printfn "int: %i" i
    | Bool b -> printfn "bool: %b" b 
    | _      -> printfn "something else: %s" str

testParse "12"
testParse "false"
testParse "abc"