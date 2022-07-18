type ordering = LT  | EQ | GT;;
module type ORDER =
sig
    type t
    val compare: t -> t -> ordering
end;;

module QUEUE (Key : ORDER) =
struct
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    exception Pusta of string

    let empty () = KolejkaPusta
    let rec enqueue (value, q) =
        match q with
            | KolejkaPusta -> Skladowa(value, KolejkaPusta)
            | Skladowa(v, next) -> Skladowa(v, enqueue (value, next))
    let dequeue q =
        match q with
            | KolejkaPusta -> KolejkaPusta
            | Skladowa(_, next) -> next

    let first q =
        match q with
            | KolejkaPusta -> raise(Pusta "kolejka jest pusta")
            | Skladowa (value, _) -> value

    let isEmpty q =
        (q = KolejkaPusta)

    let printList q =
        let rec create_list(q, list) =
            match q with
                | KolejkaPusta -> List.rev list
                | Skladowa(elem, next) -> create_list(next, elem :: list)
        in create_list(q, [])

    let filterCount q w op =
        let rec count_tail q w op count =
            match q with
                | KolejkaPusta -> count
                | Skladowa(elem, next) -> if op="<" && (Key.compare elem w) = LT then count_tail next w op (count+1)
                                        else if op="<=" && ((Key.compare elem w) = LT or (Key.compare elem w) = EQ) then count_tail next w op (count+1)
                                        else if op=">" && (Key.compare elem w) = GT then count_tail next w op (count+1)
                                        else if op=">=" && ((Key.compare elem w) = GT or (Key.compare elem w) = EQ) then count_tail next w op (count+1)
                                        else count_tail next w op count


        in count_tail q w op 0
end;;

(*if (Key.compare elem w) = op then count_tail next w op (count+1)
                                        else if (Key.compare elem w) = op then count_tail next w op (count+1)
                                        else if (Key.compare elem w) = op then count_tail next w op (count+1)
                                        else count_tail next w op count*)

module IntOrder: ORDER with type t = int =
struct
    type t = int
    let compare i1 i2 = if i1 = i2 then EQ
        else if i1 > i2 then GT
        else LT
end;;

module IntPairOrder: ORDER with type t = int * int =
struct
    type t = int * int
    let compare i1 i2 = if (fst i1 + snd i1) < (fst i2 + snd i2)  then LT
    else if (fst i1 + snd i1) > (fst i2 + snd i2) then GT
    else EQ
end;;

module IntQueue = QUEUE(IntOrder);;
module IntPairQueue = QUEUE(IntPairOrder);;

IntQueue.empty();;
IntQueue.filterCount (IntQueue.enqueue(7, IntQueue.enqueue(5, IntQueue.empty()))) 5 ">=";; (* 1 *)
IntQueue.printList (IntQueue.enqueue(7, IntQueue.enqueue(5, IntQueue.empty())));;

IntPairQueue.empty();;
                                                (*5*)                       (*9*)                           (*5*)
IntPairQueue.filterCount (IntPairQueue.enqueue((2, 3), IntPairQueue.enqueue((4, 5), IntPairQueue.empty()))) (1, 4) ">=";;
IntPairQueue.printList (IntPairQueue.enqueue((2, 3), IntPairQueue.enqueue((4, 5), IntPairQueue.empty())));;