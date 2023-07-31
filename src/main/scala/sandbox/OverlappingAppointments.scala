package sandbox

object OverlappingAppointments extends App {

  sealed abstract class Status
  case object Before extends Status
  case object During extends Status
  case object After extends Status


  def hasConflicts(appointments: List[(Int, Int)]): Map[(Int, Int), Boolean] = {

    val initMap = appointments.map(a => (a, false)).toMap

    def compareTimeToAppt(time: Int, appt: (Int, Int)): Status = {
      if (time < appt._1) Before
      else if (time < appt._2) During
      else After
    }

    appointments.foldLeft(initMap){ case (acc, (start, end)) =>
      val result = appointments
        .filter(_ != (start, end))
        .exists { appt =>
          val startStatus = compareTimeToAppt(start, appt)
          val endStatus = compareTimeToAppt(end, appt)

          println(s"${(start, end)} -> ${(startStatus, endStatus)}")

          (startStatus, endStatus) match {
            case (Before, Before) => false
            case (Before, During) => true
            case (Before, After) => true
            case (During, _) => true
            case (After, _) => false
          }
        }

      acc + ((start, end) -> result)
    }
  }

  println(hasConflicts(List((1,5),(3,7),(2,6),(10,15),(5,6),(4,100))))
  println(hasConflicts(List((1,5),(6,7),(10,15),(3,6))))

}
