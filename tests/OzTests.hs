--New tests for MF

let x = y+2 in x+y; y = 3; main = f x;

main = f 3 -4; f y x = y-x;

main = f; f = -3-4-(-2);

main = f; f = -3-4*(*(-2));

main = f; f = -(3-4)-(-2);

main = f+x; f = -3-4-(-2); x = true;

main = f+x; f = -3-4-(-2); x = True;

main = f+x; f = -3-4-(-2); x == true;

main = f+x; f = false; x = true;

main = f+x; f = False; x = true;

f x y = if x == true then false else y; main = f true false;

f x y = if x == true then false else y; main = f True false;

f x y = if x == true then false else y; main = f true true;

y=let x=3 in x/x; main = x;

y=let x=3 in x/x; main = y/y;

y=let x=3 in x/x; x= true; main = y*(-x);

y=let x=3 in x/x; x= y-(-y)/(-y); main = y*(-x);

x y z = if y<z then x*x else x-x; main = x 3 3;

 x y z = if Not(y<z) then x*x else x-x; main = x 3 3;

 x y z = if y<z then x*x else x-x; main = x 3 (-3);

 x y z = if y<z then x*x else x-x; main = x 3 -3;

 x y z = if y<z then x*x else x-x; main = x 3 -(-3);
