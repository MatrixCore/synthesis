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
    |true -> failwith "Year less than 1582"
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
    match num < 0 with
    |true -> failwith "Negative int"
    |false -> 
    let rec BinaryConvert remain binary = 
        match remain > 0 || binary = "" with
        | false -> binary
        | true -> BinaryConvert (remain/2) (string(remain%2) + binary)
    BinaryConvert num ""

let bizFuzz num =
    let rec FB count a b c = match count > num with
        |true -> a, b, c //base case
        |false -> match count % 15 with
            |0 -> FB (count + 1) (a + 1) (b + 1) (c + 1)
            |_ -> match count % 5 with
                |0 -> FB (count + 1) a (b + 1) c
                |_ -> match count % 3 with
                    |0 -> FB (count + 1) (a + 1) b c
                    |_ -> FB (count + 1) a b c
    FB 1 0 0 0

let monthDay d y = 
    let rec FindMonth days count leap = 
        let a,b = month count
        match days > b with
           | false -> a
           | true -> match leap = 1 && count = 2 with 
                     | true -> FindMonth (days-b-leap) (count+1) leap
                     | false -> FindMonth (days-b) (count+1) leap

    match isLeap y with
    | true -> 
        match d >= 1 && d <= 366 with
        | false -> failwith "Invalid day"
        | true -> FindMonth d 1 1
    | false -> 
        match d >= 1 && d <= 365 with
        | false -> failwith "Invalid day"
        | true -> FindMonth d 1 0
        

let coord Coord1 Coord2 =
    let sqrt n =
        let rec calculate guess i =
            match i with
            | 10 -> guess
            | _ -> 
            let g = (guess + n/guess) / 2.0
            calculate g (i+1)

        match n <= 0.0 with
        | true -> failwith "Impossibru!"
        | _ -> calculate (n/2.0) 0
    //End of Square Root Function
    let x1, y1 = Coord1
    let x2, y2 = Coord2
    let dist = sqrt ((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2))
    failwith "Not implemented"