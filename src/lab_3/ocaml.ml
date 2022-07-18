let zad2 value xs =
    let rec find (value, xs, list, pos) =
        match xs with
            (h :: _) when h = value -> find(value, List.tl xs, pos :: list, pos+1)
            | (h :: _) -> find(value, List.tl xs, list, pos+1)
            | _ -> match list with
                (h :: _) -> list
                | _ -> raise (Failure "brak elementu w liscie")
    in find(value, xs, [], 1);;

zad2 1 [3;4;1;7;5;1;7;1] ;;
