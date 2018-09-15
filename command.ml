type command =
  | Raise of int
  | Call
  | Fold
  | Show
  | Status
  | Manual
  | None
  | Exit

let parse str =
  let str_ins = String.lowercase_ascii (String.trim str) in
  if (str_ins = "call")
  then Call
  else if (str_ins = "fold")
  then Fold
  else if (str_ins = "show")
  then Show
  else if (str_ins = "exit")
  then Exit
  else if (str_ins = "status")
  then Status
  else if (str_ins = "manual")
  then Manual
  else let ind = String.index_opt str_ins ' ' in
    match ind with
    | None -> None
    | Some i -> begin let first_word = String.sub str_ins 0 i in
        if first_word = "raise"
        then let second_word = String.lowercase_ascii
                 (String.trim (String.sub str_ins i
                                 ((String.length str_ins)-i)))
          in try Raise (int_of_string second_word) with
          | _ -> None
        else None
      end
