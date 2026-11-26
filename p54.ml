open Printf

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type suit =
  | Club
  | Diamond
  | Heart
  | Spade

type player = Player of int
type card = rank * suit
type hand = card list
type turn = hand * hand
type game = turn list

type score =
  | High_card
  | One_pair
  | Two_pairs
  | Three_of_a_kind
  | Straight
  | Flush
  | Full_house
  | Four_of_a_kind
  | Straight_flush
  | Royal_flush

let rank_of_card (r, _) = r
let suit_of_card (_, s) = s

let rank_of_char = function
  | '2' -> Two
  | '3' -> Three
  | '4' -> Four
  | '5' -> Five
  | '6' -> Six
  | '7' -> Seven
  | '8' -> Eight
  | '9' -> Nine
  | 'T' -> Ten
  | 'J' -> Jack
  | 'Q' -> Queen
  | 'K' -> King
  | 'A' -> Ace
  | r -> invalid_arg (sprintf "invalid rank: %c" r)
;;

let int_of_rank = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14
;;

let suit_of_char = function
  | 'C' -> Club
  | 'D' -> Diamond
  | 'H' -> Heart
  | 'S' -> Spade
  | s -> invalid_arg (sprintf "invalid suit: %c" s)
;;

let card_of_string s : card =
  let cs = List.of_seq (String.to_seq s) in
  match cs with
  | [ r; s ] -> rank_of_char r, suit_of_char s
  | _ -> invalid_arg (sprintf "invalid card: %s" s)
;;

let distrib xs =
  let rec aux acc xs' =
    match xs' with
    | [] | [ _ ] -> [ acc ]
    | x :: y :: ys -> if x = y then aux (acc + 1) (y :: ys) else acc :: aux 1 (y :: ys)
  in
  List.sort compare xs |> aux 1 |> List.sort compare
;;

let diffs xs =
  let len = List.length xs - 1 in
  List.map2 (fun r2 r1 -> r2 - r1) (List.tl xs) (List.take len xs)
;;

let score_of_hand (h : hand) : score =
  let exception Score of score in
  try
    let ranks = List.sort compare (List.map rank_of_card h) in
    let suits = List.sort compare (List.map suit_of_card h) in
    let rank_distrib = distrib ranks in
    let suit_distrib = distrib suits in
    (* royal flush *)
    if suit_distrib = [ 5 ] && ranks = [ Ten; Jack; Queen; King; Ace ]
    then raise (Score Royal_flush);
    (* straight flush *)
    if rank_distrib = [ 5 ] && suit_distrib = [ 5 ] then raise (Score Straight_flush);
    (* four of a kind *)
    if rank_distrib = [ 1; 4 ] then raise (Score Four_of_a_kind);
    (* full house *)
    if rank_distrib = [ 2; 3 ] then raise (Score Full_house);
    (* flush *)
    if suit_distrib = [ 5 ] then raise (Score Flush);
    (* straight *)
    let rank_ints = List.map int_of_rank ranks in
    if diffs rank_ints = [ 1; 1; 1; 1 ] then raise (Score Straight);
    (* three of a kind *)
    if List.mem 3 rank_distrib then raise (Score Three_of_a_kind);
    (* two pairs *)
    (match rank_distrib with
     | [ 1; 2; 2 ] -> raise (Score Two_pairs)
     | _ -> ());
    (* one pair *)
    if List.mem 2 rank_distrib then raise (Score One_pair);
    (* high card *)
    raise (Score High_card)
  with
  | Score s -> s
;;

let max_rank h =
  let ranks = List.map rank_of_card (List.sort compare h) in
  List.fold_left (fun acc c -> if acc < c then c else acc) Two ranks
;;

let max_rank_pair h =
  let ranks = List.map rank_of_card (List.sort compare h) in
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | a :: b :: cs -> if a = b && acc < a then aux a (b :: cs) else aux acc (b :: cs)
  in
  aux Two ranks
;;

let max_rank_triple h =
  let ranks = List.map rank_of_card (List.sort compare h) in
  List.iter (fun r -> printf "%d " (int_of_rank r)) ranks;
  print_endline "";
  let rec aux acc = function
    | [] | [ _ ] | [ _; _ ] -> acc
    | a :: b :: c :: ds ->
      let rest = b :: c :: ds in
      if a = b && b = c && acc < a then aux a rest else aux acc rest
  in
  aux Two ranks
;;

let max_rank_quad h =
  let ranks = List.map rank_of_card (List.sort compare h) in
  let rec aux acc = function
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> acc
    | a :: b :: c :: d :: es ->
      let rest = b :: c :: d :: es in
      if a = b && b = c && c = d && acc < a then aux a rest else aux acc rest
  in
  aux Two ranks
;;

let eval_turn ((h1, h2) : turn) : player =
  let exception Winner of player in
  try
    let s1, s2 = score_of_hand h1, score_of_hand h2 in
    (* score difference *)
    if s1 > s2 then raise (Winner (Player 1));
    if s2 > s1 then raise (Winner (Player 2));
    (* score tie *)
    let max_f =
      match s1 with
      | High_card | Straight | Flush | Straight_flush | Royal_flush -> max_rank
      | One_pair | Two_pairs -> max_rank_pair
      | Three_of_a_kind | Full_house -> max_rank_triple
      | Four_of_a_kind -> max_rank_quad
    in
    let max_1, max_2 = max_f h1, max_f h2 in
    if max_1 > max_2 then raise (Winner (Player 1));
    if max_2 > max_1 then raise (Winner (Player 2));
    (* rank tie *)
    let test_card (r1, _) (r2, _) =
      if r1 > r2 then raise (Winner (Player 1));
      if r2 > r1 then raise (Winner (Player 2))
    in
    List.iter2 test_card (List.rev h1) (List.rev h2);
    (* tie *)
    raise (Winner (Player (-1)))
  with
  | Winner p -> p
;;

let eval_game : game -> player list = List.map eval_turn

let read_match filename : turn list =
  let parse_row s =
    let cards = List.map card_of_string (String.split_on_char ' ' s) in
    let h1 = List.take 5 cards |> List.sort compare in
    let h2 = List.drop 5 cards |> List.take 5 |> List.sort compare in
    h1, h2
  in
  List.map parse_row (In_channel.with_open_text filename In_channel.input_lines)
;;

let game = read_match "p54.in"

let _ =
  eval_game game |> List.filter (fun p -> p = Player 1) |> List.length |> printf "%d\n"
;;
