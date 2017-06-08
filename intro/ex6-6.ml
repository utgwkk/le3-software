type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec reflect tr =
  match tr with
    Lf -> Lf
  | Br (value, left, right) -> Br (value, reflect right, reflect left)

(*
 * reverse() はリストを逆順に並べ替える関数である．
 * preorder(reflect(t)) = reverse(postorder(t))
 * inorder(reflect(t)) = reverse(inorder(t))
 * postorder(reflect(t)) = reverse(preorder(t))
 *)