type 'a bt = Lisc of 'a | Wezel of 'a bt * 'a bt | WezelPoj of 'a bt;;

let pred1 elem = elem > 2;;
let pred2 elem = elem > 90;;

let rec checkPred tree pred =
    match tree with
        | Lisc(value) -> pred value
        | WezelPoj(t1) -> checkPred t1 pred
        | Wezel(t1, t2) -> (checkPred t1 pred) || (checkPred t2 pred);;
let t = Wezel(Wezel(Wezel(Lisc(5), Lisc(1)), WezelPoj(Lisc(4))), Lisc(3));;

checkPred t pred1;;
checkPred t pred2;;
