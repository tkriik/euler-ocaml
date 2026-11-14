type date = { weekday : int; day : int; month : int; year : int }

let is_leap = function
  | year when year mod 4 = 0 ->
      if year mod 100 = 0 then year mod 400 = 0 else true
  | _ -> false

let days_in_month month ~year =
  match month with
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap year then 29 else 28
  | _ -> 31

let pos_mod n d = 1 + ((n - 1) mod d)

let next = function
  | { weekday; day; month; year } ->
      let wd = pos_mod (weekday + 1) 7 in
      let d = pos_mod (day + 1) (days_in_month month ~year) in
      let m = if d = 1 then pos_mod (month + 1) 12 else month in
      let y = if d = 1 && m = 1 then year + 1 else year in
      { weekday = wd; day = d; month = m; year = y }

let start = { weekday = 1; day = 1; month = 1; year = 1900 }

let ans =
  Seq.iterate next start
  |> Seq.take_while (fun { year } -> year < 2001)
  |> Seq.filter (fun { year } -> 1900 < year)
  |> Seq.filter (fun { weekday; day } -> weekday = 7 && day = 1)
  |> Seq.length
