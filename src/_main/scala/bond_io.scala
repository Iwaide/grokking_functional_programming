import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
case class MeetingTime(startHour: Int, endHour: Int)

object ch08_SchedulingMeetings {

  def calendarEntriesApiCall(name: String): List[MeetingTime] = {
    import scala.jdk.CollectionConverters._
    ch08_SchedulingMeetingsAPI.calendarEntriesApiCall(name).asScala.toList
  }

  def calendarEntries(person: String): IO[List[MeetingTime]] = {
    IO.delay(calendarEntriesApiCall(person))
  }

  def createMeetingApiCall(names: List[String], meetingTime: MeetingTime): Unit = {
    import scala.jdk.CollectionConverters._
    ch08_SchedulingMeetingsAPI.createMeetingApiCall(names.asJava, meetingTime)
  }

  def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] = {
    IO.delay(createMeeting(names, meetingTime))
  }

  def scheduledMeetings(attendees: List[String]): IO[List[MeetingTime]] = {
    attendees.
      // List[IO[List[MeetingTime]]]ができる
      map(attendee => {
        calendarEntries(attendee)
      // IO[List[List[MeetingTime]]]ができる
      }).sequence.
      // IO[List[MeetingTime]]になる
      map(_.flatten)
  }
  
  def possibleMeetings(
    existingMeetings: List[MeetingTime],
    startHour: Int, endHour: Int,
    lengthHours: Int): List[MeetingTime] = {
      val diff = endHour - startHour - lengthHours
      val tmpMeetings = (0 to diff).map(i => {
        MeetingTime(startHour + i, startHour + lengthHours + i)
      }).toList
      tmpMeetings.filter(tmpMeeting => {
        existingMeetings.forall(existing => {
          tmpMeeting.startHour >= existing.endHour || 
            tmpMeeting.endHour <= existing.startHour
        })
      })
    }

  // def schedule(
  //   person1: String,
  //   person2: String,
  //   lengthHours: Int
  // ): IO[Option[MeetingTime]] = {
  //   val existingEntries: IO[List[MeetingTime]] = for {
  //     person1Entries <- calendarEntries(person1)
  //     person2Entries <- calendarEntries(person2)
  //   } yield person1Entries.appendedAll(person2Entries)
  //   for {
  //     entries <- existingEntries
  //   } yield possibleMeetings(entries, 8, 16, lengthHours).headOption
  // }

  def schedule(
    attendees: List[String],
    lengthHours: Int
  ): IO[Option[MeetingTime]] = {
    for {
      // この行でIO型のコンテキストが決定する
      existingEntries <- scheduledMeetings(attendees).
                          orElse(IO.pure(List.empty))
      // これは代入なのでOptionを返してもいい？
      possbileMeeting = possibleMeetings(existingEntries, 8, 16, lengthHours).headOption
      // IO[Unit]型を返しているが副作用が目的なので捨ててる
      _ <- possbileMeeting match {
        case Some(meeting) => createMeeting(attendees, meeting).
                                orElse(createMeeting(attendees, meeting)).
                                orElse(IO.unit)
        case None => IO.unit
      }
      // Option[MeetingTime]がIOに包まれて返される
    } yield possbileMeeting
  }
}

