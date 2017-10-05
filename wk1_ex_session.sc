import javax.swing.DefaultBoundedRangeModel

println("session test")
//def loop: Boolean = loop
//def x = loop
//val x = loop

1 + 2

def abs(x: Double) = if(x < 0) -x else x
def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < 0.001
def improve(guess: Double, x: Double) =
  (guess + x / guess)/  2