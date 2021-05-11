package model

import model.robot.{Base, Connected, Effector, HundredFiftyMMBase, Servo, xl430w250t}
import shapeless.HList.ListCompat.::
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness}
import shapeless.PolyDefns.{Case, Compose}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FlatMapper, MapFolder}

import scala.language.postfixOps

package robot {



  sealed trait Connected {
    def next: Connected
  }

  sealed trait Bearing

  sealed trait OutputMount extends Drivable

  sealed trait Effector extends Drivable

  sealed trait Servo extends Connected {
    val drivable: Drivable

    override def next: Connected = drivable
  }

  sealed trait MountingPlate extends Connected {
    val servo: Servo

    override def next: Connected = servo
  }

  sealed trait Base extends Connected {
    val bearing: Bearing
    val servo: Servo with BaseCompatible
    val highTorqueJointsSpecifier: HighTorqueJointsSpecifier

    override def next: Connected = servo
  }

  sealed trait BasePlate extends Drivable {
    val mountingPlate: MountingPlate with BaseCompatible with NotRotary

    override def next: Connected = mountingPlate
  }

  sealed trait BaseCompatible

  sealed trait Drivable extends Connected

  sealed trait Double

  sealed trait NoConnector

  sealed trait Connector extends Connected {
    val mountingPlate: MountingPlate

    override def next: Connected = mountingPlate
  }

  sealed trait Quad

  sealed trait Single

  sealed trait HighTorqueJointsSpecifier

  sealed trait Rotary

  sealed trait NotRotary

  case class HighTorqueShoulder() extends HighTorqueJointsSpecifier
  case class HighTorqueShoulderAndElbow() extends HighTorqueJointsSpecifier

  //Nonsensical, and no mount exists for this use case
  //case class HighTorqueNone() extends HighTorqueJointsSpecifier

  case class CrossRollerBearing() extends Bearing
  case class IndustrialBallBearing() extends Bearing


  case class MountingPlateNoBeam(servo: Servo with NotRotary) extends MountingPlate  with NoConnector with NotRotary
  case class MountingPlateBase(servo: Servo with NotRotary) extends MountingPlate with BaseCompatible with NotRotary //Not compatible with anything but base
  case class MountingPlateSingleBeam(servo: Servo with NotRotary) extends MountingPlate with Single with NotRotary
  case class MountingPlateDoubleBeam(servo: Servo with NotRotary) extends MountingPlate with Double with NotRotary
  case class MountingPlateQuadBeam(servo: Servo with NotRotary) extends MountingPlate with Quad with NotRotary


  case class RotaryMountingPlateNoBeam(servo: Servo with Rotary) extends MountingPlate with NoConnector with Rotary
  case class RotaryMountingPlateSingleBeam(servo: Servo with Rotary) extends MountingPlate with Single with Rotary
  case class RotaryMountingPlateDoubleBeam(servo: Servo with Rotary) extends MountingPlate with Double with Rotary
  case class RotaryMountingPlateQuadBeam(servo: Servo with Rotary) extends MountingPlate with Quad with Rotary


  case class OutputMountNoBeam(mountingPlate: MountingPlate with NoConnector) extends NoConnector with OutputMount with NotRotary {
    override def next: Connected = mountingPlate
  }
  case class OutputMountSingleBeam(singleConnector: Connector with Single) extends Single with OutputMount with NotRotary {
    override def next: Connected = singleConnector
  }
  case class OutputMountDoubleBeam(doubleConnector: Connector with Double) extends Double with OutputMount with NotRotary {
    override def next: Connected = doubleConnector
  }
  case class OutputMountQuadBeam(quadConnector: Connector with Quad) extends Quad with OutputMount with NotRotary {
    override def next: Connected = quadConnector
  }

  case class OutputMountNoBeamHorizontal(mountingPlate: MountingPlate with NoConnector) extends NoConnector with OutputMount with NotRotary {
    override def next: Connected = mountingPlate
  }
  case class OutputMountSingleBeamHorizontal(singleConnector: Connector with Single) extends Single with OutputMount with NotRotary {
    override def next: Connected = singleConnector
  }
  case class OutputMountDoubleBeamHorizontal(doubleConnector: Connector with Double) extends Double with OutputMount with NotRotary {
    override def next: Connected = doubleConnector
  }
  case class OutputMountQuadBeamHorizontal(quadConnector: Connector with Quad) extends Quad with OutputMount with NotRotary {
    override def next: Connected = quadConnector
  }

  case class RotaryOutput(servo: xl430w250t) extends OutputMount with Rotary {
    override def next: Connected = servo
  }


  case class xl430w250t(drivable: Drivable with NotRotary) extends Servo with NotRotary
  case class xl430w250trotary(drivable: Drivable with Rotary) extends Servo with Rotary
  case class xl430w250tbase(drivable: BasePlate) extends Servo with BaseCompatible


  case class SingleBeam(mountingPlate: MountingPlate with Single) extends Single with Connector
  case class DoubleBeam(mountingPlate: MountingPlate with Double) extends Double with Connector
  case class QuadBeam(mountingPlate: MountingPlate with Quad) extends Quad with Connector


  //think if propped up allowed
  case class HundredFiftyMMBase(bearing: Bearing, servo: Servo with BaseCompatible, highTorqueJointsSpecifier: HighTorqueJointsSpecifier) extends Base
  case class HundredMMBasePlate(mountingPlate: MountingPlate with BaseCompatible with NotRotary) extends BasePlate

  case class Gripper() extends Effector with NotRotary {
    override def next: Connected = null
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
  val available = List[Tagged](Tagged(HundredFiftyMMBase), Tagged(xl430w250t))
  val availRepo = available.foldLeft(repository)((repo, c) => repo.addCombinator(c.c)(c.weakTypeTag))
  val result = availRepo.inhabit[Base]()

  //result.interpretedTerms.values(15)._2.foreach(println)
  println(result.interpretedTerms.values(26)._2(1))
  val robot = new Robot(result.interpretedTerms.values(26)._2(1))
  robot.assembleRobot
  //termToLinks(result.interpretedTerms.values(15)._2(1),Vector[Link](),Vector[Connected]()).foreach(println)

}



