let get_sub_list xs value len =
    let rec get_sub_list_tail xs value len rest_len acc =
        if rest_len = 0 then acc
        else if rest_len < len || List.hd xs = value then  get_sub_list_tail (List.tl xs) value len (rest_len-1) (List.hd xs :: acc)
        else get_sub_list_tail (List.tl xs) value len rest_len acc
    in List.rev (get_sub_list_tail xs value len len []);;


get_sub_list [2; 6; 2;1] 6 2;;
get_sub_list ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] 'b' 3;;

type 'a wybor = Tak | Nie of 'a * 'a wybor;;
let (&&) l f =
    Nie(l, f);;

let rec makeList wyb =
    match wyb with
        |Tak -> []
        |Nie(x, y) -> x :: makeList y;;


let ob = 2 && 3 && 4 && 7 && Tak;;
makeList ob;;

let ab = 'a' && 'b' && 'c' && 'd' && Tak;;
makeList ab;;