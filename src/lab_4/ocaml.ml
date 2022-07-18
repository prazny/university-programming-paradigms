let rec zad1 pred xs =
    match xs with
    [] -> []
    | h::t -> if pred h then h::zad1 pred t
                else zad1 pred t;;


let pred1 x = x > 5;;
zad1 pred1 [1;7;3;9;2;7];;

let zad1b pred xs =
    let rec find pred xs acc = match xs with
        [] -> acc
        | h :: t -> if pred h then find pred t (h::acc) else find pred t acc
    in find pred xs [];;
zad1b pred1 [1;7;3;9;2;7];;

let funcZad2 x = x *. x;;
let listOfFloats = [4.; 6.;1.5];;

let zad2 func points dx =
    let rec zad2Tail func points dx result =
        match points with
        [] -> List.rev result
        | h::t -> zad2Tail func t dx  (((func (h+.dx) -. func (h))/.dx)::result)
    in zad2Tail func points dx [];;

zad2 funcZad2 listOfFloats 0.0000001;;


