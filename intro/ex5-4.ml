let next n = n + 1
let twice n = n * 2

let target = [1; 2; 3; 4; 5; 6]
let mapped = List.map twice (List.map next target)
let mapped' = List.map (fun x -> twice (next x)) target
