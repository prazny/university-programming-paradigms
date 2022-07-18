let arr = [|
[|1; 2; 3; 4; 5|];
[|2; 2; 3; 4; 5|];
[|3; 2; 3; 4; 5|];
[|4; 2; 3; 4; 5|];
[|5; 2; 3; 4; 5|];
    |];;

let sumRows arr =
    let rec sumRows_tail arr sum_arr counter =
        if counter >= Array.length arr then sum_arr
        else sumRows_tail  arr (Array.append sum_arr [|(Array.fold_left (fun (sum) elem -> (sum+elem))(0) arr.(counter))|]) (counter+1)
    in sumRows_tail arr [||] 0;;
sumRows arr;;


let arr = [|
[|1; 2; 3; 4; 5|];
[|2; 2; 3; 4; 5|];
[|3; 2; 3; 4; 5|];
[|4; 2; 3; 4; 5|];
[|5; 2; 3; 4; 5|];
    |];;

let sumRows_imp arr =
        let sum_arr = Array.make (Array.length arr) 0
        in for i=0 to (Array.length arr)-1 do
            for j=0 to (Array.length (arr.(i)))-1 do
                sum_arr.(i) <- sum_arr.(i) + arr.(i).(j)
            done;
        done;
        sum_arr
;;

sumRows_imp arr;;



module type OBSLUGA_KOLEJKI =
sig
  type 'a tk
  exception Pusta of string
  val tworz_pusta: unit -> 'a tk
  val do_kolejki: 'a * 'a tk -> 'a tk
  val z_kolejki: 'a tk -> 'a tk
  val pierwszy_element: 'a tk -> 'a
  val kolejka_pusta: 'a tk -> bool
end;;

module QUEUE : OBSLUGA_KOLEJKI =
struct
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    exception Pusta of string

    let tworz_pusta () = KolejkaPusta
    let rec do_kolejki (value, q) =
        match q with
            | KolejkaPusta -> Skladowa(value, KolejkaPusta)
            | Skladowa(v, next) -> Skladowa(v, do_kolejki (value, next))
    let z_kolejki q =
        match q with
            | KolejkaPusta -> KolejkaPusta
            | Skladowa(_, next) -> next

    let pierwszy_element q =
        match q with
            | KolejkaPusta -> raise(Pusta "kolejka jest pusta")
            | Skladowa (value, _) -> value

    let kolejka_pusta q =
        (q = KolejkaPusta)
end;;

QUEUE.pierwszy_element (QUEUE.do_kolejki(7, QUEUE.do_kolejki(5, QUEUE.tworz_pusta())));;
QUEUE.pierwszy_element (QUEUE.z_kolejki(QUEUE.do_kolejki(7, QUEUE.do_kolejki(5, QUEUE.tworz_pusta()))));;

QUEUE.pierwszy_element (QUEUE.tworz_pusta());;