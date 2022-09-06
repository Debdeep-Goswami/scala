
//  1. Setup project in intellij, create git repo and push your code to github.
object Main {

  //  2. Write a function that takes two boolean inputs and returns XOR of it.
  //  Xor Function
  def xor(a:Boolean,b:Boolean): Boolean = a ^ b

  //  3. Write a function that takes an int and prints whether the number is even or odd.
  //  IsEven or not
  def isEven(a:Int):Boolean = if(a%2==0) true else false

  //  4. Write a function that takes an int value and returns true if it’s a prime number, otherwise false.
  //  IsPrime or not
  def isPrime(a:Int):Boolean = {
    if(a<2) return false
    else if(a==2) return true
    else for(i <- 2 to a / 2) if(a%i==0) return false
    true
  }

  //  5. Write a function that takes an int value and returns factorial of it using while loop.
  //  Factorial Iterative
  def factorialIterative(a:Int):Any = {
    if(a<0) return null
    var ans :Long = 1
    var i = 2;
    while(i<=a){
      ans = ans * i
      i += 1
    }
    ans
  }

  //  Factorial Recursive
  def factorialRecursive(a:Int,ans:Long):Long = {
    if(a==1) return ans
    var temp = ans * a
    factorialRecursive(a-1,temp)
  }

  //  7. Write a function that takes a number N and returns the Nth value of the fibonacci number.
  //  Fibonacci of Nth number
  def fibonacci(n:Int): Int = {
    var first:Int = 0
    var second:Int = 1
    var sum:Int = 0

    if(n==0) return first
    if(n==1) return second
    else for(i<- 2 to n) {
      sum = first+second
      first = second;
      second = sum
    }
    sum
  }

  //  Fibonacci Recursive
  def fibonacciRecursive(n:Int,first:Int,second:Int,ans:Int):Int = {
    if(n==0) return first
    if(n==1) return second
    else {
      var temp = first+second;
      fibonacciRecursive(n-1,second,temp,temp)
    }
  }

  //  8. Perform prime non prime using tail recursion.
  //  Recursive prime
  def isPrimeRecursive(n:Int,divisor:Int):Boolean = {
    if(n<=1) return false
    else if(n==2) return true
    else if(n%divisor==0) return false
    else if(divisor*divisor>n) return true
    isPrimeRecursive(n,divisor+2)
  }

  def main(args: Array[String]): Unit = {
    println(xor(true,true))
    println(xor(a = true, b = false))

    println(isEven(5))
    println(isEven(6))

    println(isPrime(3))
    println(isPrime(20))

    println(factorialIterative(4))
    println(factorialRecursive(4,1))

    println(fibonacci(5))
    println(fibonacciRecursive(5, 0, 1, 0))

    println(isPrimeRecursive(10,2))
  }
}