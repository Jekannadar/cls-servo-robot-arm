package model

import model.robot.{Base, Connected, Effector, HundredFiftyMMBase, Joint, Link, Physical, Robot, Servo, xl430w250t}
import shapeless.HList.ListCompat.::
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness}
import shapeless.PolyDefns.{Case, Compose}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FlatMapper, MapFolder}

import scala.language.postfixOps

package robot {

  class Link(val parts: Vector[Connected]){
  }

  class Joint{
    val id = 0
  }

  class Robot{
    val links: List[Link] = List()
    val joints: List[Link] = List()
  }

  sealed trait Connected{
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

  sealed trait Base extends Connected{
    val bearing: Bearing
    val servo: Servo with BaseCompatible with Rotary
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

  sealed trait Connector extends Connected{
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

  case class RotaryOutput(servo: xl430w250t) extends OutputMount with Rotary {
    override def next: Connected = servo
  }

  case class xl430w250t(drivable: Drivable with NotRotary) extends Servo with NotRotary

  case class xl430w250trotary(drivable: Drivable with Rotary) extends Servo with Rotary

  case class xl430w250tbase(drivable: BasePlate) extends Servo with Rotary with BaseCompatible

  case class SingleBeam(mountingPlate: MountingPlate with Single) extends Single with Connector 

  case class DoubleBeam(mountingPlate: MountingPlate with Double) extends Double with Connector 

  case class QuadBeam(mountingPlate: MountingPlate with Quad) extends Quad with Connector 

  //think if propped up allowed
  case class HundredFiftyMMBase(bearing: Bearing, servo: Servo with BaseCompatible with Rotary, highTorqueJointsSpecifier: HighTorqueJointsSpecifier) extends  Base

  case class HundredMMBasePlate(mountingPlate: MountingPlate with BaseCompatible with NotRotary) extends BasePlate

  case class Gripper() extends Effector with NotRotary {
    override def next: Connected = null
  }

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


  def termToLinks(part: Connected, links: Vector[Link], current : Vector[Connected]) : Vector[Link] = {
    part match {
      case _: Servo => termToLinks(part.next,links :+ new Link(current :+ part),Vector[Connected]())
      case _: Effector => links :+ new Link(Vector[Connected](part))
      case _: Connected => termToLinks(part.next,links,current :+ part)
    }
  }


  val instance = new Repository()
  val repository = ReflectedRepository[Repository](instance,classLoader = getClass.getClassLoader)
  val available = List[Tagged](Tagged(HundredFiftyMMBase), Tagged(xl430w250t))
  val availRepo = available.foldLeft(repository)((repo, c) => repo.addCombinator(c.c)(c.weakTypeTag))
  val result = availRepo.inhabit[Base]()

  println(result.interpretedTerms.values(10)._2(1))
  //termToLinks(result.interpretedTerms.values(15)._2(1),Vector[Link](),Vector[Connected]()).foreach(println)
  termToLinks(result.interpretedTerms.values(15)._2(1),Vector[Link](),Vector[Connected]()).foreach(link => {
    link.parts.foreach(part => print(s"${part.getClass.getName},"))
    println(" ")
  })

}



