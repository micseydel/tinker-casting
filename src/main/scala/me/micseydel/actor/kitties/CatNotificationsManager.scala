package me.micseydel.actor.kitties

import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{HueCommand, Notification, NotificationId}
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerContext}
import me.micseydel.model.{Butter, CatOfMine, LitterBoxChoice, Peanut}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId
import net.jcazevedo.moultingyaml.*

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success, Try}

object CatNotificationsManager {
  sealed trait Message

  final case class LitterUsed(when: ZonedDateTime, litterBoxChoice: LitterBoxChoice, maybeCat: Option[CatOfMine], ref: NoteId) extends Message
  final case class LitterClean(litterBoxChoice: LitterBoxChoice, ref: NoteId) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("CatNotificationsManager", CatBrown, "🐱") { (context,  noteRef) =>
    context.actorContext.log.info("Starting CatNotificationsManager")

    val config = noteRef.readNote().map(_.maybeFrontmatter) match {
      case Failure(_: FileNotFoundException) => DefaultConfig
      case Failure(exception) =>
        context.actorContext.log.warn(s"Unexpected failure accessing ${noteRef}", exception)
        DefaultConfig
      case Success(None) =>
        DefaultConfig
      case Success(Some(frontmatter)) =>
        import YamlProtocol.hackyConfigYamlFormat
        Try(frontmatter.parseYaml.convertTo[HackyConfig]) match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Failed to deserialize yaml", exception)
            DefaultConfig
          case Success(value) =>
            value
        }
    }

    behavior(config)
  }

  private def behavior(config: HackyConfig)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context

    message match {
      case LitterUsed(when, choice, maybeCat, noteId) =>
        val now = context.system.clock.now()
        val timeSinceEvent = TimeUtil.between(when, now)

        val timeToWait = maybeCat match {
          case Some(Peanut) | None => config.peanut.minutes
          case Some(Butter) => config.butter.minutes
        }

        val delay: FiniteDuration = timeToWait - timeSinceEvent

        context.actorContext.log.info(s"Using delay ${delay.toMinutes} minutes for cat $maybeCat ($timeToWait - $timeSinceEvent)")

        val notification = LitterNotification(when.plusMinutes(delay.toMinutes), s"$choice needs sifting", noteId, NotificationId(choice.toString))
        context.system.notifier !! NotificationCenterManager.NewNotification(notification)

        Tinker.steadily

      case LitterClean(litterBoxChoice, ref) =>
        context.actorContext.log.info(s"Marking $litterBoxChoice as done and canceling the timer ($ref)")
        context.system.notifier !! NotificationCenterManager.CompleteNotification(litterBoxChoice.toString)
        Tinker.steadily
    }
  }

  def LitterNotification(time: ZonedDateTime, string: String, noteId: NoteId, notificationId: NotificationId): Notification = {
    Notification(time, string, Some(noteId), notificationId, List(HueCommand(HueControl.DoALightShow())))
  }

  case class HackyConfig(peanut: Int, butter: Int)

  private val DefaultConfig: HackyConfig = HackyConfig(20, 15)

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val hackyConfigYamlFormat: YamlFormat[HackyConfig] = yamlFormat2(HackyConfig)
  }
}
