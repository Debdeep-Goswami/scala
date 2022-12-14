import scala.annotation.tailrec

object Part1 extends App {

  //  Sum of all numbers in a list
  val l1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  //  println(l1.sum)
  //  println(l1.reduce(_ + _))

  //  Text Processing
  val strList: List[String] = List("Lorem Ipsum is simply dummy text of the printing and typesetting industry.", "Lorem Ipsum has been the industry's standard dummy text.", "Ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.", "Here are some special char &$ *.")
  //  println(strList)
  val finalString = strList.mkString(" ")
  //  println(finalString)

  //  strList.foreach(println)

  //  Tokenization based on white space
  val newString: List[String] = strList.flatMap(s => s.split("\\s").toList)
  //  println(newString)

  //  Filter out tokens containing special symbols
  val newString2: List[String] = newString.filter(a => a.contains("&") || a.contains("*"))
  //  println(newString2)

  //  println(newString.filterNot(a=>a.contains("&") || a.contains("*")))

  //  println(newString.filter(a => a.matches("[&\\*]+")))

  //  Lowercase
  val lower = newString.map(_.toLowerCase())
  //  println(lower)

  //  Frequency of every token
  val count: Map[String, Int] = lower.groupBy(k => k).view.mapValues(_.length).toMap
  //  println(count)

  //  Max Frequency token
  val n = count.maxBy {
    case (token, freq) => freq
  }
  println(n)

  //  println(count.maxBy(_._2))


  //  Second Minimum element from a list
//  val element = List(1, 4, 3, 2, 5, 6, 10, 9, 8)
  //  @tailrec
  //  def findSecondMin(element:List[Int],min:Int): Int ={
  //    if(element.length==1) {
  //      Math.max(min, element(0))
  //    }else{
  //      findSecondMin(element.slice(0,element.size-1), )
  //    }
  //  }

  //  With normal variable
  @tailrec
  def findMin(element: List[Int], min: Int): Int = {
    if (element.isEmpty) min
    else {
      findMin(element.tail, Math.min(min, element.head))
    }
  }

  //  With Option
  @tailrec
  def findMin(element: List[Int], min: Option[Int]): Option[Int] = {
    if (element.isEmpty) min
    else {
      findMin(element.tail, min.map(a => Math.min(a, element.head)))
    }
  }

  //  With match
  def findMinV2(element: List[Int], min: Option[Int]): Option[Int] = {
    element.headOption match {
      case Some(value) => findMin(element.tail, min.map(Math.min(_, value)))
      case None => min
    }
  }

  val element = List(11, 4, 3, 2, 5, 6, 10, 9, 8)
  @tailrec
  def findSecondMin(element: List[Int], min: Option[Int], secMin: Option[Int]): Option[Int] = {
    element.headOption match {
      case None => secMin
      case Some(x) => (min, secMin) match {
        case (None, None) => findSecondMin(element.tail, Some(x), None)
        case (None, Some(_)) => None
        case (Some(m), None) => findSecondMin(element.tail, Some(Math.min(x, m)), Some(Math.max(x, m)))
        case (Some(m), Some(s)) =>
          if (x < m) {
            val newM = x;
            val newS = m
            findSecondMin(element.tail, Some(newM), Some(newS))
          } else if (x < s) {
            val newM = m;
            val newS = x
            findSecondMin(element.tail, Some(newM), Some(newS))
          }else findSecondMin(element.tail,Some(m),Some(s))
      }
    }
  }

  //  println(findMin(element, None))
  //  println(findMinV2(element, None))
  //  println(findSecondMin(element, Integer.MAX_VALUE, Integer.MAX_VALUE))
  //  println(find(element, Integer.MAX_VALUE, Integer.MAX_VALUE))
//  var tpl = ("Sugar", 25)
//  println(tpl._1)
  //  println(tpl(1))
  println(findSecondMin(element,None,None))
}

