package verycoolrobotstuff

import model.Repository
import verycoolrobotstuff.robot.{Base, BaseCompatible, HighTorqueNone, IndustrialBallBearing, OutputMount, OutputMountNoBeam, RotaryMountingPlateNoBeam, xl430w250t}

import scala.reflect.runtime.universe

package robot {
  sealed trait Robot

  sealed trait Bearing

  sealed trait OutputMount extends Drivable

  sealed trait Effector extends Drivable

  sealed trait Servo {
    val drivable: Drivable
  }

  sealed trait MountingPlate {
    val servo: Servo
  }

  sealed trait BaseCompatible

  sealed trait Drivable

  sealed trait Double

  sealed trait NoConnector

  sealed trait Connector

  sealed trait Quad

  sealed trait Single

  sealed trait HighTorqueJointsSpecifier

  sealed trait Rotary

  sealed trait NotRotary

  case class HighTorqueShoulder() extends HighTorqueJointsSpecifier

  case class HighTorqueShoulderAndElbow() extends HighTorqueJointsSpecifier

  case class HighTorqueNone() extends HighTorqueJointsSpecifier

  case class CrossRollerBearing() extends Bearing

  case class IndustrialBallBearing() extends Bearing

  case class MountingPlateNoBeam(servo: Servo with NotRotary) extends MountingPlate with BaseCompatible with NoConnector with NotRotary

  case class MountingPlateSingleBeam(servo: Servo with NotRotary) extends MountingPlate with Single with NotRotary

  case class MountingPlateDoubleBeam(servo: Servo with NotRotary) extends MountingPlate with Double with NotRotary

  case class MountingPlateQuadBeam(servo: Servo with NotRotary) extends MountingPlate with Quad with NotRotary

  case class RotaryMountingPlateNoBeam(servo: Servo with Rotary) extends MountingPlate with NoConnector with Rotary

  case class RotaryMountingPlateSingleBeam(servo: Servo with Rotary) extends MountingPlate with Single with Rotary

  case class RotaryMountingPlateDoubleBeam(servo: Servo with Rotary) extends MountingPlate with Double with Rotary

  case class RotaryMountingPlateQuadBeam(servo: Servo with Rotary) extends MountingPlate with Quad with Rotary

  case class OutputMountNoBeam(mountingPlate: MountingPlate with NoConnector) extends NoConnector with OutputMount with NotRotary

  case class OutputMountSingleBeam(singleConnector: Connector with Single) extends Single with OutputMount with NotRotary

  case class OutputMountDoubleBeam(doubleConnector: Connector with Double) extends Double with OutputMount with NotRotary

  case class OutputMountQuadBeam(quadConnector: Connector with Quad) extends Quad with OutputMount with NotRotary

  case class RotaryOutput(servo: xl430w250t) extends OutputMount with Rotary

  case class xl430w250t(drivable: Drivable with NotRotary) extends Servo with NotRotary

  case class xl430w250trotary(drivable: Drivable with Rotary) extends Servo with Rotary

  case class SingleBeam(mountingPlate: MountingPlate with Single) extends Single with Connector

  case class DoubleBeam(mountingPlate: MountingPlate with Double) extends Double with Connector

  case class QuadBeam(mountingPlate: MountingPlate with Quad) extends Quad with Connector

  //think if propped up allowed
  case class Base(bearing: Bearing, mountingPlate: MountingPlate with BaseCompatible with NotRotary, highTorqueJointsSpecifier: HighTorqueJointsSpecifier)

  case class Gripper() extends Effector with NotRotary

  //Type Class containing all Physical, ergo non structural information

  object Physical{
    trait Dimensions[A]{
      val translateX: scala.Double
      val translateY: scala.Double
      val translateZ: scala.Double
      val weight: scala.Double
    }

    object Dimensions {
      implicit val baseDimensions: Dimensions[Base] = new Dimensions[Base] {
        override val translateX: scala.Double = 0.0
        override val translateY: scala.Double = 0.0
        override val translateZ: scala.Double = 0.0
        override val weight: scala.Double = 0.0
      }

      implicit val outputMountDimensions: Dimensions[OutputMountNoBeam] = new Dimensions[OutputMountNoBeam] {
        override val translateX: scala.Double = 0.0
        override val translateY: scala.Double = 0.0
        override val translateZ: scala.Double = 0.0
        override val weight: scala.Double = 0.0
      }
    }

    def getDimensions[A](a: A)(implicit dimensions: Dimensions[A]): Dimensions[A] = dimensions

    getDimensions(Base(null,null,null)).translateX
    getDimensions(OutputMountNoBeam(null)).translateX
  }

}



import scala.reflect.runtime.universe._
trait Tagged {
  type C
  val c: C
  val weakTypeTag: WeakTypeTag[C]
}

object Tagged {
  type Aux[CC] = Tagged { type C = CC }
  def apply[CC](_c: CC)(implicit tag: WeakTypeTag[CC]): Aux[CC] = new Tagged {
    override type C = CC
    override val c: C = _c
    override val weakTypeTag: WeakTypeTag[C] = tag
  }
}


object RobotCalculus extends App {

  import org.combinators.cls.interpreter._


  val instance = new Repository()
  val repository = ReflectedRepository[Repository](instance,classLoader = getClass.getClassLoader)
  val available = List[Tagged](Tagged(Base), Tagged(xl430w250t))
  val availRepo = available.foldLeft(repository)((repo, c) => repo.addCombinator(c.c)(c.weakTypeTag))
  val result = availRepo.inhabit[Base]()
  //result.rules.foreach(println(_))
  result.interpretedTerms.values(15)._2.foreach(println(_))

}

