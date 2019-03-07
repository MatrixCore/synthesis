module Synthesis

let abelar a =
    a > 12 && a < 3097 && a % 12 = 0

let area b h = 
    match b >= 0.0 && h >= 0.0 with
        |true -> (0.5 * b) * h
        |false -> failwith "Invalid dimension"

let zollo n =
    match n < 0 with
        |true -> -1 * n
        |false -> n * 2

let min num1 num2 =
    match num1 > num2 with
        |true -> num2
        |false -> num1

let max num1 num2 =
    match num1 < num2 with
        |true -> num2
        |false -> num1

let ofTime h m s =
    (h * 3600) + (m * 60) + (s)

let toTime time =
// Remeber to deal with negatives
    match time <= 0 with
    |true -> 0,0,0
    |false ->   let h = time / 3600
                let m = (time - (h * 3600)) / 60
                let s = (time - (h * 3600) - (m * 60))
                h,m,s
    

let digits number =
    let rec FindDigit num digits = 
        match num = 0 && digits <> 0 with
        | true -> digits // base case
        | false -> FindDigit (num/10) (digits + 1) //recursive case        
    FindDigit number 0
    

let minmax (a,b,c,d) =
    let numMin = min (min a b) (min c d)
    let numMax = max (max a b) (max c d)
    numMin, numMax
        
let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz num =
    //let ThreeDivide n = ()
  
    //let FiveDivide n = ()

    //let ThreeFiveDivide n = ()
    failwith "Not implemtented"

let monthDay _ _ = 
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"