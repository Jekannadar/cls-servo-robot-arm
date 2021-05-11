package model

import model.Robot.{Dimensions}
import model.robot._

class Link(robot: Robot,val parts: Vector[Connected], index: Int) {
  def translateX = parts.map(robot.getDimensions(_,index).translateX).reduce(_+_)
  def translateY = parts.map(robot.getDimensions(_,index).translateY).reduce(_+_)
  def translateZ = parts.map(robot.getDimensions(_,index).translateZ).reduce(_+_)
  def rotate = parts.map(robot.getDimensions(_,index).rotate).reduce(_+_)
  def assembleStl : Stl = {
    var currentX=robot.getDimensions(parts(0),index).translateX
    var currentY=robot.getDimensions(parts(0),index).translateY
    var currentZ=robot.getDimensions(parts(0),index).translateZ
    var currentRot=robot.getDimensions(parts(0),index).rotate
    //First part may not be transformed
    val linkStl = robot.getDimensions(parts(0),index).mesh
    parts.drop(1).foreach(part => {
      val curr = robot.getDimensions(part,index).mesh
      curr.zRotate(currentRot)
      curr.translate(currentX,currentY,currentZ)
      Stl.mergeStls(linkStl,curr)
      currentX += robot.getDimensions(part,index).translateX
      currentY += robot.getDimensions(part,index).translateY
      currentZ += robot.getDimensions(part,index).translateZ
      currentRot += robot.getDimensions(part,index).rotate
    })
    linkStl.writeStl(s"generated/link$index.stl")
    linkStl
  }
}

class Joint {
  val id = 0
}

class Robot(base: Base) {
  val links: Vector[Link] = termToLinks(base,Vector[Link](),Vector[Connected](),0)
  val joints: Vector[Joint] = Vector()

  def assembleRobot : Stl = {
    var currentX=links(0).translateX
    var currentY=links(0).translateY
    var currentZ=links(0).translateZ
    var currentRot=links(0).rotate

    //Base is not displaced at all, fixed world frame
    val robotStl = links(0).assembleStl
    links.drop(1).foreach(link => {
      val curr = link.assembleStl
      curr.zRotate(currentRot)
      curr.translate(currentX,currentY,currentZ)
      Stl.mergeStls(robotStl,curr)
      currentX += link.translateX
      currentY += link.translateY
      currentZ += link.translateZ
      currentRot += link.rotate
      //println(s"X: $currentX Y: $currentY Z: $currentZ Rot: $currentRot")
    })
    robotStl.writeStl("generated/robot.stl")
    robotStl
  }

  def termToLinks(part: Connected, links: Vector[Link], current : Vector[Connected], index: Int) : Vector[Link] = {
    part match {
      case _: Servo => termToLinks(part.next,links :+ new Link(this,current :+ part,index),Vector[Connected](),index + 1)
      case _: Effector => links :+ new Link(this,Vector[Connected](part),index)
      case _: Connected => termToLinks(part.next,links,current :+ part, index)
    }
  }

  //Based on link index, some parts can differ to use the HighTorqueJointSpecifier
  //Can this be optimised to not reread in the stl every time? Probably
  def getDimensions[A](a: A,index: Int): Dimensions = {
    a match {
      case _:HundredFiftyMMBase => Dimensions(0,0,5,0,0,new Stl("resources/Base.stl"))
      case _:HundredMMBasePlate => Dimensions(0,0,9.232f,0,0,new Stl("resources/BasePlate.stl"))
      case _:MountingPlateBase => Dimensions(0,0,6.5f,0,0,new Stl("resources/MountingPlateBase.stl"))
        //OutputMounts
      case _:OutputMountNoBeam =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountNoBeamWide.stl"))
        else
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountNoBeam.stl"))
      case _:OutputMountSingleBeam =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountSingleBeamWide.stl"))
        else
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountSingleBeam.stl"))
      case _:OutputMountDoubleBeam =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountDoubleBeamWide.stl"))
        else
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountDoubleBeam.stl"))
      case _:OutputMountQuadBeam =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountQuadBeamWide.stl"))
        else
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountQuadBeam.stl"))
      case _:OutputMountNoBeamHorizontal =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountNoBeamHorizontalWide.stl"))
        else
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountNoBeamHorizontal.stl"))
      case _:OutputMountSingleBeamHorizontal =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountSingleBeamWide.stl"))
        else
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountSingleBeam.stl"))
      case _:OutputMountDoubleBeamHorizontal =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountDoubleBeamHorizontalWide.stl"))
        else
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountDoubleBeamHorizontal.stl"))
      case _:OutputMountQuadBeamHorizontal =>
        if(index==2 || (index==3 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,24,0,0,new Stl("resources/OutputMountQuadBeamWide.stl"))
        else
          Dimensions(0,0,24,90,0,new Stl("resources/OutputMountQuadBeam.stl"))
      case _:RotaryOutput => Dimensions(0,0,6.5f,0,0,new Stl("resources/OutputMountRotary.stl"))


      case _:MountingPlateNoBeam =>
        if( index==2 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow])
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateNoBeamWide.stl"))
        else
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateNoBeam.stl"))
      case _:MountingPlateSingleBeam =>
        if( index==2 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow])
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateSingleBeamWide.stl"))
        else
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateSingleBeam.stl"))
      case _:MountingPlateDoubleBeam =>
        if( index==2 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow])
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateDoubleBeamWide.stl"))
        else
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateDoubleBeam.stl"))
      case _:MountingPlateQuadBeam =>
        if( index==2 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow])
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateQuadBeamWide.stl"))
        else
          Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateQuadBeam.stl"))
      case _:RotaryMountingPlateNoBeam => Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateNoBeamRotary.stl"))
      case _:RotaryMountingPlateSingleBeam => Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateSingleBeamRotary.stl"))
      case _:RotaryMountingPlateDoubleBeam => Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateDoubleBeamRotary.stl"))
      case _:RotaryMountingPlateQuadBeam => Dimensions(0,0,5.5f,0,0,new Stl("resources/MountingPlateQuadBeamRotary.stl"))

        
      case _:xl430w250tbase => Dimensions(0,0,36,0,0,new Stl("resources/ServoBase.stl"))
      case _:xl430w250trotary => Dimensions(-12,0,36,0,0,new Stl("resources/ServoRotary.stl"))
      case _:xl430w250t =>
        if(index==1 || (index==2 & base.highTorqueJointsSpecifier.isInstanceOf[HighTorqueShoulderAndElbow]))
          Dimensions(0,0,35.25f,0,0,new Stl("resources/ServoDouble.stl"))
        else
          Dimensions(0,0,35.25f,0,0,new Stl("resources/Servo.stl"))
      case _:SingleBeam => Dimensions(0,0,40,0,0,new Stl("resources/SingleBeam.stl"))
      case _:DoubleBeam => Dimensions(0,0,40,0,0,new Stl("resources/DoubleBeam.stl"))
      case _:QuadBeam => Dimensions(0,0,40,0,0,new Stl("resources/QuadBeam.stl"))
      case _: Effector => Dimensions(0,0,24,0,0,new Stl("resources/OutputMountNoBeam.stl"))

    }
  }
}

object Robot{
  case class Dimensions(
     translateX: Float,
    translateY: Float,
    translateZ: Float,
     rotate: Float,
    weight: Float,
    mesh: Stl
  )


}

