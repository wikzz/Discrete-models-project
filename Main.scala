sealed trait Controller
case class WindTurbineController(speedSensor: WindSpeedSensor,
                                 dirSensor: WindDirSensor,
                                 tempSensor: TemperatureSensor,
                                 powerCurve: Map[Int, Int],
                                 orientation: Int) extends Controller {
  // the power curve is a list of integers representing
  // the power generation at different wind speeds
  // the orientation is given in degrees where
  // north is 0, east is 90, south is 180 and west is 270
  def command(date: String): String = {
    val speed = speedSensor.getWindSpeed(date)
    val dir = dirSensor.getWindDirection(date)
    val temp = tempSensor.getTemperature(date)
    if(speed < powerCurve.keysIterator.min || speed > powerCurve.keysIterator.max) {
      // outside of wind speed operating range
      return "Off"
    }
    if(dir < orientation - 90 || dir > orientation + 90) {
      // outside of wind direction operating range
      return "Off"
    }
    if(temp < -10 || temp > 40) {
      // outside of temperature operating range
      return "Off"
    }
    return "On"
  }
  def getDayProduction(date: String): Int = {
    val speed = speedSensor.listDay(date)
    val prod = speed.map(powerCurve.getOrElse(_, 0))
    prod.sum
  }
  def getTotalProduction: Int = {
    val speed = speedSensor.listAll()
    val prod = speed.map(powerCurve.getOrElse(_, 0))
    prod.sum
  }
}
case class SolarPanelController(
                               lightSensor: BrightnessSensor,
                               dirSensor: LightDirectionSensor,
                               tempSensor: TemperatureSensor,
                               power: Int
                               ) extends Controller {
  def command(date: String): String = {
    val brightness = lightSensor.getBrightness(date)
    val direction = dirSensor.getDirection(date)
    val temperature = tempSensor.getTemperature(date)
    if(temperature < -40 || temperature > 65) {
      return "Off"
    }
    if(brightness < 500) {
      return "Off"
    }
    // if within operating range, turn solar panels to face the sun
    direction.toString
  }
  def getDayProduction(date: String): Double = {
    val light = lightSensor.listDay(date)
    light.count(_ > 18000)*power
  }
  def getTotalProduction: Double = {
    val light = lightSensor.listAll
    light.count(_ > 18000)*power
  }
}
case class HydropowerController(
                               damSensor: DamWaterSensor,
                               flowSensor: WaterFlowSensor,
                               power: Int
                               ) extends Controller {
  def command(date: String): String = {
    val flow = flowSensor.getFlow(date)
    val reserves = damSensor.getReserves(date)
    if(reserves < 0.3 && flow > 14) {
      // if dam has too little water
      return "Close dam"
    }
    if(flow < 5 || reserves > 0.95) {
      // if the flow rate of the river is well below normal
      // or the reserves are too full
      return "Open dam"
    }
    "Close dam"
  }
  def getDayProduction(date: String): Double = {
    val flow = flowSensor.listDay(date)
    val reserves = damSensor.listDay(date)
    flow.sum*power+reserves.filter(_>0.95).sum*power
  }
  def getTotalProduction: Double = {
    val flow = flowSensor.listAll
    val reserves = damSensor.listAll
    (flow.sum+reserves.filter(_>0.95).sum)*power
  }
}




/*
PLACEHOLDER CLASSES BELOW
REMOVE WHEN DELIVERING FINAL PROJECT!!!
 */




case class WindSpeedSensor() {
  def getWindSpeed(date: String): Double = {
    14.5
  }
  def listDay(date: String): List[Int] = {
    // returns wind speeds for the last 24 hours as a list
    List(12, 11, 5, 4, 3, 1, 1, 2,
      2, 0, 0, 0, 1, 2, 4, 2,
      5, 2, 3, 6, 8, 9, 10, 11)
  }
  def listAll(): List[Int] = {
    listDay("")
  }
}

case class WindDirSensor() {
  def getWindDirection(date: String): Int = {
    /*
    0 = N
    90 = E
    180 = S
    270 = W
    */
    310
  }
}

case class TemperatureSensor() {
  def getTemperature(date: String): Double = {
    17.3
  }
}

case class BrightnessSensor() {
  def getBrightness(date: String): Double = {
    51000
  }
  def listDay(date: String): List[Double] = {
    List(0.01, 0.001, 0.001, 0.2, 0.2, 15, 100, 400,
      1800, 5700, 16000, 17555, 19002, 20006, 25043, 23752,
      21655, 20896, 17033, 13893, 6839, 1002, 250, 0.2)
  }
  def listAll: List[Double] = {
    listDay("")
  }
}

case class LightDirectionSensor() {
  def getDirection(date: String): Int = {
    /*
    0 = E
    180 = W
    */
    110
  }
}

case class DamWaterSensor() {
  def getReserves(date: String): Double = {
    0.7
  }
  def listDay(date: String): List[Double] = {
    List(0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.3, 0.3,
      0.15, 0.1, 0.2, 0.6, 0.8, 0.99, 0.7, 0.75,
      0.75, 0.75, 0.75, 0.74, 0.74, 0.74, 0.75, 0.73)
  }
  def listAll: List[Double] = {
    listDay("")
  }
}

case class  WaterFlowSensor() {
  def getFlow(date: String): Int = {
    18
  }
  def listDay(date: String): List[Int] = {
    List(24, 16, 18, 5, 4, 3, 3, 3,
      4, 4, 3, 15, 25, 36, 27, 28,
      25, 22, 15, 16, 18, 13, 15, 19)
  }
  def listAll: List[Int] = {
    listDay("")
  }
}