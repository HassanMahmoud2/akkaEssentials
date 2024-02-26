package part2

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2.ActorsCapabilities.BankAccount.{Deposit, Statement, TransactionFailure, TransactionSuccess, Withdraw}
import part2.ActorsCapabilities.Counter.{Decrement, Increment, Print}
import part2.ActorsCapabilities.Person.LiveTheLife

import scala.Console.println

object ActorsCapabilities extends App {
  object Counter {
    object Increment
    object Decrement
    object Print
  }
  class Counter extends Actor {
    var counter = 0
    override def receive: Receive = {
      case Increment => counter += 1
      case Decrement => counter -= 1
      case Print => println(counter)
    }
  }
  val system = ActorSystem("actorSystem")
  val counter = system.actorOf(Props[Counter], "myCounter")
  (1 to 3).foreach(_ => counter ! Increment)
  (1 to 4).foreach(_ => counter ! Decrement)
  counter ! Print

  object BankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement
    case class TransactionSuccess(message: String)
    case class TransactionFailure(reason: String)
  }
  class BankAccount extends Actor {
    var funds = 0
    override def receive: Receive = {
      case Deposit(amount) =>
        if(amount < 0) sender() ! TransactionFailure("invalid deposited amount")
        else {
          funds += amount
          sender() ! TransactionSuccess(s"successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if(amount < 0 || amount > funds) sender() ! TransactionFailure("invalid withdraw amount")
        else {
          funds -= amount
          sender() ! TransactionSuccess(s"successfully withdraw $amount")
        }
      case Statement => sender() ! s"Your Balance is $funds"
    }
  }
  object Person {
    case class LiveTheLife(account: ActorRef)
  }
  class Person extends Actor {
    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }
  val account = system.actorOf(Props[BankAccount], "bankAccount")
  val person = system.actorOf(Props[Person], "billionaire")
  person ! LiveTheLife(account)
}
