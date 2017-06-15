let makemult = fun maker -> fun x ->
	if x < 1 then 1 else x * maker maker (x + -1) in
		let factorial = fun x -> makemult makemult x
	in factorial 10;;
