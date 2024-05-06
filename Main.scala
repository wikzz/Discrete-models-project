import scala.io.Source

sealed trait SensorResult
case class SensorData(data: List[String]) extends SensorResult
case class SensorError(message: String) extends SensorResult

sealed trait Controller {
  def parseToIntList(data: List[String]): List[Int] = {
    // Parse each line of data to integers
    data.flatMap(line => line.split("\\s+").map(_.toIntOption).collect { case Some(value) => value })
  }

  def parseToDoubleList(data: List[String]): List[Double] = {
    // Parse each line of data to doubles
    data.flatMap(line => line.split("\\s+").map(_.toDoubleOption).collect { case Some(value) => value })
  }
}
case class WindTurbineController(
                                  speedSensor: Sensor,
                                  dirSensor: Sensor,
                                  tempSensor: Sensor,
                                  powerCurve: Map[Int, Int],
                                  orientation: Int) extends Controller {
  def command(): String = {
    val speed = parseToIntList(speedSensor.getData).last
    val dir = parseToIntList(dirSensor.getData).last
    val temp = parseToIntList(tempSensor.getData).last
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
}
case class SolarPanelController(
                                 lightSensor: Sensor,
                                 tempSensor: Sensor,
                                 power: Int
                               ) extends Controller {
  def command(): String = {
    val brightness = parseToIntList(lightSensor.getData).last
    val temperature = parseToIntList(tempSensor.getData).last
    if(temperature < -40 || temperature > 65) {
      return "Off"
    }
    if(brightness < 500) {
      return "Off"
    }
    return "On"
  }
}
case class HydropowerController(
                                 damSensor: Sensor,
                                 flowSensor: Sensor,
                                 power: Int
                               ) extends Controller {
  def command(): String = {
    val flow = parseToIntList(flowSensor.getData).last
    val reserves = parseToDoubleList(damSensor.getData).last
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
}

import scala.io.Source

class Sensor(filePath: String) {
  def getData: List[String] = {
    // Read data from the file
    try {
      val source = Source.fromFile(filePath)
      val data = source.getLines().toList
      source.close()
      data
    } catch {
      case e: Exception =>
        // Return an empty list if an exception occurs
        List.empty[String]
    }
  }
}

//Modifier
case class Modifier(windTurbineController: WindTurbineController,
                    solarPanelController: SolarPanelController,
                    hydropowerController: HydropowerController) {

  def adjustControllers(): Unit = {
    val windTurbineCommand = windTurbineController.command()
    val solarPanelCommand = solarPanelController.command()
    val hydropowerCommand = hydropowerController.command()

    // Based on the commands from each controller, perform adjustments or actions
    // For example:
    if (windTurbineCommand == "On") {
      println("Wind turbine is turned on.") // side effect, printing to console
      // Perform actions for wind turbine operation
      val windSpeed = windTurbineController.speedSensor.getData.last
      println(s"Current wind speed: $windSpeed")
    } else {
      println("Wind turbine is turned off.")
      // Perform actions when wind turbine is off
    }

    if (solarPanelCommand == "On") {
      println("Solar panels are facing the sun.")
      // Perform actions for solar panel operation
      val brightness = solarPanelController.lightSensor.getData.last
      println(s"Current brightness: $brightness")
    } else {
      println("Solar panels are not facing the sun.")
      // Perform actions when solar panels are off
    }

    if (hydropowerCommand == "Open dam") {
      println("Dam is open for hydropower generation.")
      // Perform actions for hydropower operation when dam is open
      val flow = hydropowerController.flowSensor.getData.last
      val reserves = hydropowerController.damSensor.getData.last
      println(s"Current flow rate: $flow, Reserves: $reserves")
    } else {
      println("Dam is closed.")
      // Perform actions for hydropower operation when dam is closed
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Initializing controllers...")

    // Create instances of the sensors
    println("Please enter the file path of the speed sensor data: (e.g. /Users/david/Downloads/Wind_speed.txt)")
    val speedFilePath = scala.io.StdIn.readLine()
    println("Please enter the file path of the wind direction sensor data: (e.g. /Users/david/Downloads/Wind_direction.txt)")
    val dirFilePath = scala.io.StdIn.readLine()
    println("Please enter the file path of the temperature sensor data: (e.g. /Users/david/Downloads/Temperature.txt)")
    val temperatureFilePath = scala.io.StdIn.readLine()
    val speedSensor = new Sensor(speedFilePath)
    val dirSensor = new Sensor(dirFilePath)
    val tempSensor = new Sensor(temperatureFilePath)

    // Create instance of WindTurbineController
    val windTurbineController = WindTurbineController(
      speedSensor = speedSensor,
      dirSensor = dirSensor,
      tempSensor = tempSensor,
      powerCurve = Map(
        1 -> 100,
        2 -> 200,
        3 -> 300,
        4 -> 400,
        5 -> 500,
        6 -> 600
      ),
      orientation = 180
    )
    val solarPanelController = new SolarPanelController(
      lightSensor = dirSensor,
      tempSensor = tempSensor,
      power = 9000
    )
    val hydropowerController = new HydropowerController(
      damSensor = dirSensor,
      flowSensor = speedSensor,
      power = 20000
    )
    val modifier = new Modifier(
      windTurbineController = windTurbineController,
      solarPanelController = solarPanelController,
      hydropowerController = hydropowerController
    )
    // Print information about the wind turbine controller
    println("Wind Turbine Controller Information:")
    modifier.adjustControllers()
  }
}