val test1= List(1,2,3)
val test2 =List(1, 3, -5)
println("* test1 ::test2  --"+ test1 ::test2) //con
println("** test1 ++ test2 --"+ test1 ++ test2) //concatenate
/*Scala translated (for... yield...) to map, flatMap, filter */
//1.
for(x <- test1) yield x * 2
test1.map(x => x *2)

//2.
/*for (x <- e1 if f; s) yield e2
where f is a filter and s is a (potentially empty) sequence of
generators and filters, is translated to   */
for(x <- test1 if x >1; y <- test2) yield x * y
for(x <- test1 if x >1; y <- test2) yield (x, y)

/*for (x <- e1.withFilter(x => f); s) yield e2,
withFilter not producing intermediate list,
in which filter is absorbed in generator
x taken from e1 with filter taking anonymous function,
taken x and give back to exprssion f */
for(x <-test1.withFilter(x => x>1); y <- test2) yield x * y
for(x <-test1.withFilter(x => x>1); y <- test2) yield (x, y)

//3.
/* for (x <- e1; y <-e2; s) yield e3 , which is translated into
 * e1.flatMap(x => for (y <- e2; s) yield e3 ) */
for(x <- test1; y <- test2; if x*x > y*y) yield (x,y)
test1.flatMap(x => for (y <- test2; if x*x > y*y) yield (x, y))
test1.map(x => for (y <- test2; if x*x > y*y) yield (x, y))

for(x <- test1; y <- test2; if x*x > y*y) yield (x*y)
test1.flatMap(x => for (y <- test2; if x*x > y*y) yield (x*y))
test1.map(x => for (y <- test2; if x*x > y*y) yield (x*y))

def for_loop(N: Int) = {
  for {
    x <- 2 to N
    y <- 2 to x
    if (x % y == 0)
  } yield (x, y)
}

def map_loop(N: Int) ={
  (2 to N) flatMap (x =>
    (2 to x) withFilter(y=> x % y == 0) map (y => (x, y)))
}
for_loop(5)
map_loop(5)
/*Take pairs consisting of two numbers from 1 to N, then find pairs whose sum is Prime   */

def isPrime(n: Int) =
  List.range(2, n) forall (x => n % x != 0)
def isPrime2(n: Int) =
  (2 until n) forall (x => n % x != 0)
def isPrime3(N: Int) = {
  var bool_prime = true
  for {
    i <- 2 until N
    if (N % i == 0)
  }yield bool_prime = false
  bool_prime
}
def isPrime4(N: Int) = {
  for {
    i <- 2 until N
    if (N % i != 0)
  }yield true
}
def for_isPrime(N: Int) = {
  for {
    i <- 1 to N
    j <- 1 to i
    if isPrime(i + j)
  }yield (i, j)
}

def map_isPrime(N: Int) = {
  (1 to N) flatMap (x => (1 to x) withFilter (y => (2 until (x + y)) forall
    (z => (x + y) % z != 0)) map (y => (x, y)))
}
def map_isPrime2(N: Int) = {
  (1 to N) flatMap (x => (1 to x) withFilter (y => isPrime(x+y)) map (y => (x, y)))
}
println("**********************************")
println("for_isPrime(n)-yield (i, j) : " +for_isPrime(7))
println("map_isPrime(n) : "+ map_isPrime(7))
println("map_isPrime2(n) : "+ map_isPrime2(7))
println("***********************************")
println("isPrime(113) : "+isPrime(113))
println("isPrime(90) : "+isPrime(90))
println("isPrime3(113) : "+isPrime(113))
println("isPrime3(90) : "+isPrime(90))
println("isPrime4(113) : "+isPrime(113))
println("isPrime4(90) : "+isPrime(90))