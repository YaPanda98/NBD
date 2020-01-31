package assignement

object Assignement1 extends App {
  //Task 1
  //For loop:
  def forLoop(days: List[String]): String = {
    var s: String = ""
    for (i <- days)
      s += i + ","
    s.slice(0, s.length - 1)
  }

  //For days that start with "S"
  def forLoopWithS(days: List[String]): String = {
    var s: String = ""
    for (i <- days) {
      if (i.head.toString.equals("S")) {
        s += i + ","
      }
    }
    s.slice(0, s.length - 1)
  }

  //While loop
  def whileLoop(days: List[String]): String = {
    var i = 0
    var s = ""
    while (i < days.length) {
      s += days(i) + ","
      i += 1
    }
    s.slice(0, s.length - 1)
  }

  //Task 2
  //Recursive function
  def recursiveDays(days: List[String]): String = days match {
    case Nil => ""
    case x :: Nil => x
    case x :: xs => x + "," + recursiveDays(xs)
  }

  //Reverse recursive function
  def recursiveReverseDays(days: List[String]): String = days.reverse match {
    case Nil => ""
    case x :: Nil => x
    case x :: xs => x + "," + recursiveDays(xs)
  }

  //Task 3
  //Tail-recursive function
  def tailRecursiveDays(days: List[String], acc: String): String = days match {
    case Nil => acc
    case x :: xs => tailRecursiveDays(xs, acc match {
      case "" => x
      case _ => acc + "," + x
    })
  }

  //Task 4
  //Foldl
  def foldlFunction(days: List[String]): String = {
    val s = days.foldLeft("")(_ + _ + ",")
    s.slice(0, s.length - 1)
  }

  //Foldr
  def foldrFunction(days: List[String]): String = {
    val s = days.foldRight("")(_ + "," + _)
    s.slice(0, s.length - 1)
  }

  //Foldl starting from S
  def foldlFunctionWith(days: List[String]): String = {
    val s = days.foldRight("") { (xs, acc) =>
      if (xs.head.equals('S')) xs + "," + acc else acc
    }
    s.slice(0, s.length - 1)
  }

  //Task 5
  //Map 10% discount
  def mapDiscount(products: Map[String, Double]): Map[String, Double] = {
    products.mapValues(_ * 0.9)
  }

  //Task 6
  //Map +1
  def mapPlusOne(numbers: List[Int]): List[Int] = {
    numbers.map(_ + 1)
  }

  //Task 7
  //Absolute values between -5 and 12
  def absInRange(numbers: List[Double]): List[Double] = {
    numbers.filter(-5 < _).filter(_ < 12).map(_.abs)

  }

  //Task 8
  //Print Tuple3
  def printTuple3(tuple: Tuple3[Any, Any, Any]): Unit = println(tuple)

  //Task 9
  //List without 0, recursively
  def recursiveFilter(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case 0 :: xs => recursiveFilter(xs)
    case x :: xs => List(x) ::: recursiveFilter(xs)
  }

  //Task 10
  //Option method 1
  def option1(x: Int, y: Int): Option[Int] = y match {
    case 0 => None
    case _ => Some(x / y)
  }

  //Option method 2
  def option2(s: Option[String]): String = s match {
    case None => "The box is empty!"
    case Some(s) => "The string is: " + s
  }

  val test: List[String] = List()
  val days: List[String] = List("Monday", "Tuesday", "Wednesday", "Tuesday",
    "Friday", "Saturday", "Sunday");

  val products = Map("Banana bread" -> 4.5, "Candies" -> 8.2, "Bannana" -> 5.5,
    "Donut" -> 3.3)

  val numbers = List.range(0, 10)
  val n = List(0, 0, 0, 1, 4, 0, 5, 10)

  val realNumbers = List.tabulate(100)(n => n / 3.0)

  val some_tuple = Tuple3(4, "53", 18.3)


}


