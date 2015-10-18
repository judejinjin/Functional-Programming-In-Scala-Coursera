package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  
  val s = union(s1, s2)
  val t = union(s, s3)
  
  var x = filter(t, s=> s < 2)
  
  printSet(x)
  
  
  def intervalWithZero(x: Int) = x >= -10 && x <= 10
    val zero = singletonSet(0)
    val intervalWithoutZero = diff(intervalWithZero, zero)
    def nonZeroPredicate(el: Int) = el != 0
    def zeroPredicate(el: Int) = el == 0
    
  printSet(intervalWithoutZero)
  printSet(intervalWithZero)
  println(forall(intervalWithoutZero, nonZeroPredicate))
  println(exists(intervalWithoutZero, zeroPredicate))
  
  printSet(map(intervalWithZero, x => x*x))
  
}
