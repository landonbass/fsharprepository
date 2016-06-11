type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind switchFunction twoTrackInput =
    match twoTrackInput with 
    | Success s -> switchFunction s
    | Failure f -> Failure f

type Request = {name: string; email:string}

let validate1 input =
    if input.name = "" then Failure "name cannot be bank"
    else Success input

let validate2 input =
    if input.name.Length > 50 then Failure "name must be 50 chars or less"
    else Success input

let validate3 input =
    if input.email = "" then Failure "email cannot be blank"
    else Success input

let combinedValidation =
    validate1 >> bind validate2 >> bind validate3

let input1 = {name=""; email=""}
let input2 = {name="landonbass"; email=""}
let input3 = {name="landonbass12345678901234567890123456789012345678901"; email="landon@landon.com"}
let input4 = {name="landonbass"; email="landon@Landon.com"}

combinedValidation input1 |> printfn "result1=%A"
combinedValidation input2 |> printfn "result2=%A"
combinedValidation input3 |> printfn "result3=%A"
combinedValidation input4 |> printfn "result4=%A"

let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput

let combinedValidation2 x =
    x
    |> validate1
    >>= validate2
    >>= validate3

combinedValidation2 input1 |> printfn "result1=%A"
combinedValidation2 input2 |> printfn "result2=%A"
combinedValidation2 input3 |> printfn "result3=%A"
combinedValidation2 input4 |> printfn "result4=%A"

let (>==>) switch1 switch2 x =
    match switch1 x with
    | Success s -> switch2 s
    | Failure f -> Failure f 

let combinedValidation3 =
    validate1
    >==> validate2
    >==> validate3

combinedValidation3 input1 |> printfn "result1=%A"
combinedValidation3 input2 |> printfn "result2=%A"
combinedValidation3 input3 |> printfn "result3=%A"
combinedValidation3 input4 |> printfn "result4=%A"

let (>=>>) switch1 switch2 =
    switch1 >> (bind switch2)

let combinedValidation4 =
    validate1
    >=>> validate2
    >=>> validate3

combinedValidation4 input1 |> printfn "result1=%A"
combinedValidation4 input2 |> printfn "result2=%A"
combinedValidation4 input3 |> printfn "result3=%A"
combinedValidation4 input4 |> printfn "result4=%A"

let formatEmail input =
    {input with email = input.email.Trim().ToLower()}

let switch f x =
    f x |> Success

let usecase =
    validate1 >=>> validate2 >=>> validate3 >=>> switch formatEmail

usecase input4 |> printfn "usercase=%A"

let map oneTrackFunction twoTrackInput =
    match twoTrackInput with 
    | Success s -> Success (oneTrackFunction s)
    | Failure f -> Failure failwith

// let usecase2 = validate1 >=>> validate2 >=>> validate3 >> map formatEmail

let tee f x =
    f x |> ignore
    x

let updateDatabase input = () //dead end

let usecase3 = validate1 >=>> validate2 >=>> validate3 >=>> switch formatEmail >=>> switch (tee updateDatabase)

let tryCatch f x =
    try f x |> Success
    with | ex -> Failure ex.Message

let usecase4 = validate1 >=>> validate2 >=>> validate3 >=>> switch formatEmail >=>> tryCatch (tee updateDatabase)
   
let plus addSuccess addFailure switch1 switch2 x =
    match (switch1 x), (switch2 x) with 
    | Success s1, Success s2 -> Success (addSuccess s1 s2)
    | Failure f1, Success _ -> Failure f1
    | Success _, Failure f2 -> Failure f2
    | Failure f1, Failure f2 -> Failure (addFailure f1 f2)

let (&&&) v1 v2 =
    let addSuccess r1 r2 = r1
    let addFailure s1 s2 = s1 + "; " + s2
    plus addSuccess addFailure v1 v2

let combinedValidation5 =
    validate1 &&& validate2 &&& validate3

combinedValidation5 input1 |> printfn "result1=%A"
combinedValidation5 input2 |> printfn "result2=%A"
combinedValidation5 input3 |> printfn "result3=%A"
combinedValidation5 input4 |> printfn "result4=%A"

let doubleMap successFunc failureFunc twoTrackInput =
    match twoTrackInput with 
    | Success s -> Success (successFunc s)
    | Failure f -> Failure (failureFunc f)

type Config = {debug:bool}
let debugLogger twoTrackInput =
    let success x = printfn "DEBUG. Success so far: %A" x; x
    let failure = id
    doubleMap success failure twoTrackInput

let injectableLogger config =
    if config.debug then debugLogger else id

let releaseConfig = {debug=false}
let debugConfig = {debug=true}

let usecase5 config = combinedValidation5 >> map formatEmail >> injectableLogger config

input4 |> usecase5 releaseConfig |> ignore
input4 |> usecase5 debugConfig |> ignore


