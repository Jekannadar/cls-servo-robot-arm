package model

import org.combinators.cls.interpreter.combinator
import model.robot._

class Repository {

  @combinator object highTorqueShoulder {
    def apply: HighTorqueShoulder = HighTorqueShoulder()
  }

  @combinator object highTorqueShoulderAndElbow {
    def apply(): HighTorqueShoulderAndElbow = HighTorqueShoulderAndElbow()
  }

  @combinator object highTorqueNone {
    def apply(): HighTorqueNone = HighTorqueNone()
  }

  @combinator object crossRollerBearing {
    def apply(): CrossRollerBearing = CrossRollerBearing()
  }

  @combinator object industrialBallBearing {
    def apply(): IndustrialBallBearing = IndustrialBallBearing()
  }

  @combinator object mountingPlateNoBeam {
    def apply(servo: Servo with NotRotary): MountingPlateNoBeam = MountingPlateNoBeam(servo)
  }

  @combinator object mountingPlateSingleBeam {
    def apply(servo: Servo with NotRotary): MountingPlateSingleBeam =
      MountingPlateSingleBeam(servo)
  }

  @combinator object mountingPlateDoubleBeam {
    def apply(servo: Servo with NotRotary): MountingPlateDoubleBeam =
      MountingPlateDoubleBeam(servo)
  }

  @combinator object mountingPlateQuadBeam {
    def apply(servo: Servo with NotRotary): MountingPlateQuadBeam =
      MountingPlateQuadBeam(servo)
  }

  @combinator object rotaryMountingPlateNoBeam {
    def apply(servo: Servo with Rotary): RotaryMountingPlateNoBeam =
      RotaryMountingPlateNoBeam(servo)
  }

  @combinator object rotaryMountingPlateSingleBeam {
    def apply(servo: Servo with Rotary): RotaryMountingPlateSingleBeam =
      RotaryMountingPlateSingleBeam(servo)
  }

  @combinator object rotaryMountingPlateDoubleBeam {
    def apply(servo: Servo with Rotary): RotaryMountingPlateDoubleBeam =
      RotaryMountingPlateDoubleBeam(servo)
  }

  @combinator object rotaryMountingPlateQuadBeam {
    def apply(servo: Servo with Rotary): RotaryMountingPlateQuadBeam =
      RotaryMountingPlateQuadBeam(servo)
  }

  @combinator object outputMountNoBeam {
    def apply(
        mountingPlate: MountingPlate with NoConnector
    ): OutputMountNoBeam = OutputMountNoBeam(mountingPlate)
  }

  @combinator object outputMountSingleBeam {
    def apply(singleConnector: Connector with Single): OutputMountSingleBeam =
      OutputMountSingleBeam(singleConnector)
  }

  @combinator object outputMountDoubleBeam {
    def apply(doubleConnector: Connector with Double): OutputMountDoubleBeam =
      OutputMountDoubleBeam(doubleConnector)
  }

  @combinator object outputMountQuadBeam {
    def apply(quadConnector: Connector with Quad): OutputMountQuadBeam =
      OutputMountQuadBeam(quadConnector)
  }

  @combinator object rotaryOutput {
    def apply(servo: xl430w250t): RotaryOutput = RotaryOutput(servo)
  }

  /*@combinator object cxl430w250t {
    def apply(drivable: Drivable with NotRotary): xl430w250t =
      xl430w250t(drivable)
  }*/

  @combinator object cxl430w250trotary {
    def apply(drivable: Drivable with Rotary): xl430w250trotary =
      xl430w250trotary(drivable)
  }

  @combinator object cxl430w250tbase {
    def apply(drivable: BasePlate): xl430w250tbase =
      xl430w250tbase(drivable)
  }

  @combinator object singleBeam {
    def apply(mountingPlate: MountingPlate with Single): SingleBeam =
      SingleBeam(mountingPlate)
  }

  @combinator object doubleBeam {
    def apply(mountingPlate: MountingPlate with Double): DoubleBeam =
      DoubleBeam(mountingPlate)
  }

  @combinator object quadBeam {
    def apply(mountingPlate: MountingPlate with Quad): QuadBeam =
      QuadBeam(mountingPlate)
  }

  //think if propped up allowed
  /*@combinator object base {
    def apply(
        bearing: Bearing,
        mountingPlate: MountingPlate with BaseCompatible with NotRotary,
        highTorqueJointsSpecifier: HighTorqueJointsSpecifier
    ): Base = Base(bearing, mountingPlate, highTorqueJointsSpecifier)
  }*/


  @combinator object hundredMMBasePlate {
    def apply(mountingPlate: MountingPlate with BaseCompatible with NotRotary): HundredMMBasePlate = HundredMMBasePlate(mountingPlate)
  }


  @combinator object gripper {
    def apply(): Gripper = Gripper()
  }
}
