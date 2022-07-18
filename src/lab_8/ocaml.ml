type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let rec mapTree tree funct =
    match tree with
        | Empty -> Empty
        | Node (value, nl, nr) -> Node((funct value), mapTree nl funct, mapTree nr funct);;


let funct_one value =
    value * value;;

let funct_two value_list =
    List.fold_left (fun sum elem -> sum+elem)(0) value_list;;


let n1 = Node(5, Node(2, Empty, Empty), Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty)));;
let n2 = Node([1;2], Node([4], Empty, Empty), Node([3;4;5], Empty, Empty));;

mapTree n1 funct_one;;
mapTree n2 funct_two;;