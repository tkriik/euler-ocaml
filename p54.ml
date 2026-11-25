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

type card = rank * suit
type hand = card list
type turn = hand * hand

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

let rec distrib xs =
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

exception Score of score

let score_of_hand (h : hand) : score =
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
let test s = String.split_on_char ' ' s |> List.map card_of_string |> score_of_hand
