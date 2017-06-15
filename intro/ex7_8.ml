(*
 * まず，修正前の charge 関数と同じように，大きい硬貨から順に使ってお金をくずす．
 * くずすのに成功した場合はそのまま結果を返す．
 * 失敗した場合は，失敗する前の状態に戻り，次に大きい硬貨を使ってお金をくずす．
 * これをくずすのに成功するまで繰り返す．
 * どのように硬貨を組み合わせてもくずすのに失敗する場合は，例外がそのまま伝播する．
 *
 * これは，例外を用いて深さ優先探索とバックトラックをしていると言い換えられる．
*)
let rec change = function
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
      if c > total then change (rest, total)
      else (
        try
          c :: change (coins, total - c)
        with Failure "change" -> change (rest, total)
      )
  | _ -> failwith "change"
