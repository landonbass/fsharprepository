

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let testNumber input =
    match input with 
    | Even -> printfn "%d is even" input
    | Odd  -> printfn "%d is odd"  input

testNumber 3
testNumber 100

