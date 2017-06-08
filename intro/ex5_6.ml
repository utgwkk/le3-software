let rec quicker l sorted = match l with
    [] -> sorted
  | x :: xs ->
      let left, right = List.partition ((>) x) xs in
      let right_sorted = x :: (quicker right sorted) in
      quicker left right_sorted

let alist = [1; 1; 4; 5; 1; 4]
