package me.micseydel.app

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import cats.data.NonEmptyList

object SelfSortingArrays {
  // https://github.com/Zhangtaining/cell_research/blob/1fd2bd5921c1f6b423a71f691d5189106a8a1020/sorting_cells.py#L4
  private val VALUE_LIST = NonEmptyList.of(28, 34, 6, 20, 7, 89, 34, 18, 29, 51)

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem(Environment(VALUE_LIST), "SelfSortingArrays")
  }
}

object Environment {
  sealed trait Message

  private final case class ReceiveListContents(listContents: List[Int]) extends Message

  def apply(valueList: NonEmptyList[Int]): Behavior[Message] = Behaviors.setup { context =>
    val listHead: ActorRef[Cell.Message] = context.spawnAnonymous(Cell(valueList.head, valueList.tail))

    listHead ! Cell.GetDownstreamListContents(context.messageAdapter(ReceiveListContents))

    Behaviors.receiveMessage {
      case ReceiveListContents(listContents) =>
        val msg = s"Received $listContents"
//        context.log.info(s"Received $listContents")
        println(msg)
        Behaviors.same
    }
  }
}

object Cell {
  sealed trait Message

  final case class GetDownstreamListContents(replyTo: ActorRef[List[Int]]) extends Message

  private case class Aggregating(soFar: List[Int], replyTo: ActorRef[List[Int]]) extends Message

  def apply(value: Int, remaining: List[Int]): Behavior[Message] = Behaviors.setup { context =>
    setup(value, remaining, None)
  }

  private def setup(value: Int, remaining: List[Int], leftNeighbor: Option[ActorRef[Cell.Message]]): Behavior[Message] = Behaviors.setup { context =>
    remaining match {
      case Nil =>
        behavior(value, leftNeighbor, None)
      case head :: tail =>
        val rightNeighbor = context.spawnAnonymous(Cell.setup(head, tail, Some(context.self)))
        behavior(value, leftNeighbor, Some(rightNeighbor))
    }
  }

  private def behavior(value: Int, maybeLeftNeighbor: Option[ActorRef[Cell.Message]], maybeRightNeighbor: Option[ActorRef[Cell.Message]]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case message@GetDownstreamListContents(replyTo) =>
        maybeRightNeighbor match {
          case None =>
            maybeLeftNeighbor match {
              case None => replyTo ! List(value)
              case Some(leftNeighbor) =>
                leftNeighbor ! Aggregating(List(value), replyTo)
            }
          case Some(rightNeighbor) =>
            rightNeighbor ! message
        }
        Behaviors.same

      case Aggregating(soFar, replyTo) =>
        maybeLeftNeighbor match {
          case None => replyTo ! value :: soFar
          case Some(leftNeighbor) =>
            leftNeighbor ! Aggregating(value :: soFar, replyTo)
        }
        Behaviors.same
    }
  }
}
