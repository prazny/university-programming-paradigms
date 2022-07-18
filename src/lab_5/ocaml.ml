let list =      [2; 6; 7; 1; 9; 2];;
let positions = [0; 4; 2; 5; 1; 3];;

let compareZipped a b =
    if fst a < fst b then -1
    else if fst a > fst b then 1
    else 0;;

let zad1 list positions =
    let rec zipLists(xs, ys) =
        match (xs,ys) with
        (h1::t1,h2::t2) -> (h1,h2)::zipLists(t1,t2)
        | _ -> []
    and unzip ps =
        match ps with
            [] -> ([], [])
            | (h1,h2)::t -> let (l1,l2) = unzip t in (h1::l1, h2::l2)
    in snd (unzip (List.sort compareZipped (zipLists(positions, list)))) ;;
zad1 list positions;;

(* zad 2 *)
let (++) f g = fun x -> f x + g x;;
let ($.) f g = fun x -> f ( g x);;

let f x = 0;;
let g x = x * x;;
let h x = 2 * x;;
let pairs = [(g, f); (h, f); (g, f)];;

let rec zad2 pairs operator =
   match pairs with
        h1::t1 -> ( (operator) (fst h1)  (snd h1)) $. (zad2 t1 (operator) )
        |[] -> fun x -> x;;

(zad2 [(g, f); (h, f); (g, f)]  (++))(2);;
