(*
 * リスト l の要素を，lの先頭の要素xより小さいもの (left) とx以上のもの (right) に分ける．
 * そして，left と，right を quicker でソートしたリストの先頭に x を追加したもの (right_sorted) に対して，
 * 再帰的に quicker を適用する．
 * *)
let rec quicker l sorted = match l with
    [] -> sorted
  | x :: xs ->
      let left, right = List.partition ((>) x) xs in
      let right_sorted = x :: (quicker right sorted) in
      quicker left right_sorted

(* for testing the behavior of sorting function. *)
let alist = [1; 1; 4; 5; 1; 4]
