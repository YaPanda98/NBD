package assignement

object Assignement2 extends App {

  //Task 1
  def typeOfTheDay(string: String): String = string.toLowerCase match {
    case "monday" => "work"
    case "tuesday" => "work"
    case "wednesday" => "work"
    case "thursday" => "work"
    case "friday" => "work"
    case "saturday" => "weekend"
    case "sunday" => "weekend"
    case _ => "no such day"
  }

  //Task 2
  class BankAccount(private var currentBalance: Double) {
    def this() {
      this(0)
    }

    def deposits(deposit: Double): Double = {
      if (deposit > 0) currentBalance = currentBalance + deposit
      currentBalance
    }

    def withdraw(amount: Double): Double = {
      if (amount <= currentBalance && amount > 0)
        currentBalance = currentBalance - amount
      currentBalance
    }
  }

  //Task 3
  case class Person1(var firstName: String, var lastName: String) {
    def greeting(person: Person1): String = person match {
      case Person1("Yulia", _) => "Hi, Yulia, nice to see you!"
      case Person1(fN, "Suprun") => "Hi," + " " + fN + "," + " wow, you have the same last name as one of my friends!"
      case Person1(fN, lN) => "Hi there, " + fN + " " + lN
    }

  }

  //Task 4
  def higherOrderFunction(f: Int => Int, x: Int): Int = (f andThen f andThen f) (x)

  //Task 5
  abstract class Person2(protected var firstName: String, protected var lastName: String) {
    def taxToPay: Double
  }

  trait Employee extends Person2 {

    private var _salary: Double = 0

    override def taxToPay: Double = salary * 0.2

    def salary: Double = _salary

    def salary_(salary: Double): Unit = _salary = salary

  }

  trait Student extends Person2 {
    override def taxToPay: Double = 0
  }

  trait Teacher extends Employee {
    override def taxToPay: Double = salary * 0.1
  }


  println("Task1:")
  println(typeOfTheDay("Monday"))
  println(typeOfTheDay("Saturday"))
  println(typeOfTheDay("banana"))

  println("Task2:")
  val x: BankAccount = new BankAccount()
  println(x.deposits(5))
  val y: BankAccount = new BankAccount(100)
  println(y.withdraw(50))

  println("Task3:")
  val person: Person1 = new Person1("Some name", "Some surname")
  println(person.greeting(new Person1("Yulia","Sauliak")))
  println(person.greeting(new Person1("Coolguy","Suprun")))
  println(person.greeting(new Person1("Andrzej","Korzov")))

  println("Task4:")
  println(higherOrderFunction(x => x * x, 2))

  println("Task5:")
  object employee1 extends Person2("Mykola", "Suprun") with Employee
  employee1.salary_(10000)
  println(employee1.taxToPay)
  object employee2 extends Person2("Mykola", "Suprun") with Employee with Student
  employee2.salary_(10000)
  println(employee2.taxToPay)
  object employee3 extends Person2("Mykola", "Suprun") with Student with Employee
  employee3.salary_(10000)
  println(employee3.taxToPay)
  object student extends Person2("Mykola", "Suprun") with Student
  println(student.taxToPay)
  object teacher extends Person2("Mykola", "Suprun") with Teacher
  teacher.salary_(10000)
  println(teacher.taxToPay)


}
