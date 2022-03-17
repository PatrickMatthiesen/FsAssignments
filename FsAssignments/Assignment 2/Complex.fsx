
2.4
type Complex = Complex of float * float

let mkComplex x y : Complex = Complex (x,y)

let complexToPair (Complex (a, b)) = (a, b)

//return type not needed
let inline (|+|) (Complex (a1,b1)) (Complex (a2, b2)) :Complex = mkComplex (a1 + a2) (b1 + b2)
let inline (|*|) (Complex (a1,b1)) (Complex (a2, b2)) :Complex = mkComplex ((a1 * a2) - (b1 * b2)) ((b1 * a2) + (a1 * b2))

//(mkComplex 1. 2.) |+| (mkComplex 1. 2.) 
(mkComplex 3.3 2.0) |+| (mkComplex -3.2 -2.0)
//(mkComplex 1. 2.) |*| (mkComplex 1. 2.) 

let inline (|-|) (c1: Complex) (Complex (a2, b2)) :Complex = c1 |+| mkComplex (-a2) (-b2)

//(mkComplex 0 0) |-| (mkComplex 1 1)

//tilde-prefixed operators and bang-prefixed operators
// it seems to overwrite all of the - prefix operators
//let inline (~-) (Complex (a,b)) :Complex = mkComplex (-a) (-b);;

//-mkComplex 1 1


let inline (|/|) (c1: Complex) (Complex (a2, b2)) :Complex = c1 |*| mkComplex (a2 / (a2**2. + b2**2.)) (((-b2) / (a2**2. + b2**2.)))



//(mkComplex 1. 2.) (-(mkComplex 1. 2.)) 

// i have no fucking idea
//let inline (|/|) (c1:unit) (c2:complex) = c2
//((mkComplex (-3.3) 10.3) |/| (mkComplex (-3.2) (-2.0)))
//(-0.7050561798, -2.778089888)