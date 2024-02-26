package part1

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App{
  private val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  private class WordCountActor extends Actor {
    private var totalWords = 0
    override def receive: Receive = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }
  private val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")
  wordCounter ! "i'm learning akka"
  anotherWordCounter ! "i'm learning akka again"

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hello, my name is $name")
    }
  }
  object Person {
    def props(name: String) = {
      Props(new Person(name))
    }
  }
  val person = actorSystem.actorOf(Props(new Person("Bob")))
  person ! "hi"

  //best practice is as follows:
  val hassan = actorSystem.actorOf(Person.props("Hassan"))
  hassan ! "hi"
}
