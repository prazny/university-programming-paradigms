
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
(* funkcje z wykładu *)
(* zwraca pierwszych n elmentow listy lxs w postaci zwyklej listy *)
let rec ltake (n, lxs) =
    match (n, lxs) with
        (0, _) -> []
        | (_, LNil) -> []
        | (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

let rec toLazyList xs =
    match xs with
        [] -> LNil
        | h::t -> LCons(h, function () -> toLazyList t);;

let zad1 lxs =
    let rec lmap_tail lxs n =
        match lxs with
            LNil -> LNil
            (*| LCons(x, xf) -> LCons(pow x n :: 1, function () -> lmap_tail (xf()) (n+1) )*)
            |LCons(x, xf) -> LCons(x, function () -> addWielokrotnosci (lxs) 1 n x )
    and addWielokrotnosci xf wystapienie docelowo liczba =
        match xf with
            LNil -> LNil
            | LCons(x, xfn) -> if wystapienie < docelowo then LCons(x *. liczba, function () -> addWielokrotnosci xf (wystapienie+1) docelowo (x *. liczba))
                                else lmap_tail (xfn()) (docelowo+1)


    in lmap_tail lxs 1;;


ltake (10, zad1 (toLazyList [2.; 3.; 4.; 5.]));;




type slowo = {value: int; freq: int};;
let directt = [{value=1; freq=1};{value=2; freq=4};{value=3; freq=2};{value=4; freq=8}];;


let rec usunWyraz wyraz direct =
    let rec usunWyraz_tail wyraz direct ret_direct =
        match direct with
            | [] -> List.rev ret_direct
            | (hd :: tl) ->
                if wyraz = hd.value then
                 if hd.freq > 1 then usunWyraz_tail wyraz tl ({value=wyraz; freq=(hd.freq - 1)} :: ret_direct)
                 else usunWyraz_tail wyraz tl ret_direct
                else usunWyraz_tail wyraz tl (hd :: ret_direct)
    in usunWyraz_tail wyraz direct [];;

usunWyraz 1 directt ;;

