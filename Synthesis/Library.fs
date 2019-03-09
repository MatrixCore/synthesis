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
        
let isLeap year =
    match year < 1582 with
    |true -> failwith "What the fuck?"
    |false -> match year % 4 = 0 && (not(year % 100 = 0) || year % 400 = 0) with
              |true -> true
              |false -> false
    
    

let month which = 
    match which with | 1 -> "January", 31
                     | 2 -> "February", 28
                     | 3 -> "March", 31
                     | 4 -> "April", 30
                     | 5 -> "May", 31
                     | 6 -> "June", 30
                     | 7 -> "July", 31
                     | 8 -> "August", 31
                     | 9 -> "September", 30
                     | 10 -> "October", 31
                     | 11 -> "November", 30
                     | 12 -> "December", 31
                     | _ -> failwith "Invalid month"


let toBinary num =
    match num >= 0 with
    |false -> failwith "Negative int"
    |true -> 

let bizFuzz num =
    let rec FB count a b c = match count > num with
        |true -> a,b,c //base case
        |false -> match count % 15 with 
            |0 -> FB (count + 1) (a + 1) (b + 1) (c + 1)
            |_ -> match count % 5 with 
                |0 -> FB (count + 1) a (b + 1) c
                |_ -> match count % 3 with 
                    |0 -> FB (count + 1) (a + 1) b c
                    |_ -> FB (count + 1) a b c
    FB 1 0 0 0

let monthDay _ _ = 
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"