let gcd a b =
  let rec gcd' a b = if b = 0 then a else gcd' b (a mod b) in
  if a < b then gcd' b a else gcd' a b

let lcm a b = a * b / gcd a b
