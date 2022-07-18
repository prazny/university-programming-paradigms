type 'a bt=Empty | Node of 'a * 'a bt * 'a bt;;

let rec countSubtree t1 t2 =
    let rec isEqual t1 t2 =
         (*t1 = t2;;*)
         match (t1, t2) with
            | (Empty, Empty) -> true
            | (_, Empty) -> false
            | (Empty, _) -> false
            | (Node(val1, nl1, nr1), Node(val2, nl2, nr2)) -> (val1 = val2 && (isEqual nl1  nl2) && (isEqual nr1 nr2))
    in
        if isEqual t1 t2 then 1
        else match t1 with
            | Empty -> 0
            | Node(_, nl, nr) -> 0 + (countSubtree nl t2) + (countSubtree nr t2);;

let t1 = Node(5, Node(2, Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty)), Empty), Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty)));;
let t2 = Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty));;
(*
            5                                                     1
        2       1                                             4       3
    1
4       3    4      3
*)
countSubtree t1 t2;;