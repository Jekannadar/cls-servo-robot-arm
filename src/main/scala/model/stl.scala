package model

import io.netty.buffer.{ByteBuf, Unpooled}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}
import scala.math.{cos, sin}

case class Point(var x: Float, var y: Float,var z: Float)

case class Triangle(normal: Point, v1: Point,v2: Point,v3: Point,color: Short)

class stl(path:String) {

  var triangles : Vector[Triangle] = Vector()
  var header: ByteBuf = Unpooled.EMPTY_BUFFER
  var count: Int = 0

  val bytes = Unpooled.copiedBuffer(Files.readAllBytes(Paths.get(path)))
  //preserve header for later
  header = bytes.readBytes(80)
  count = bytes.readIntLE()

  while (bytes.isReadable) {
    triangles :+ Triangle(
      Point(bytes.readFloatLE(),bytes.readFloatLE(),bytes.readFloatLE()),
      Point(bytes.readFloatLE(),bytes.readFloatLE(),bytes.readFloatLE()),
      Point(bytes.readFloatLE(),bytes.readFloatLE(),bytes.readFloatLE()),
      Point(bytes.readFloatLE(),bytes.readFloatLE(),bytes.readFloatLE()),
      bytes.readShortLE())
  }


  //radians
  def zRotate(angle:Double) = {
    triangles.foreach(triangle =>{
      var x = triangle.normal.x
      var y = triangle.normal.y
      triangle.normal.x = (x * cos(angle.toRadians) - y * sin(angle.toRadians)).toFloat
      triangle.normal.y = (x * sin(angle.toRadians) + y * cos(angle.toRadians)).toFloat
      x = triangle.v1.x
      y = triangle.v1.y
      triangle.v1.x = (x * cos(angle.toRadians) - y * sin(angle.toRadians)).toFloat
      triangle.v1.y = (x * sin(angle.toRadians) + y * cos(angle.toRadians)).toFloat
      x = triangle.v2.x
      y = triangle.v2.y
      triangle.v2.x = (x * cos(angle.toRadians) - y * sin(angle.toRadians)).toFloat
      triangle.v2.y = (x * sin(angle.toRadians) + y * cos(angle.toRadians)).toFloat
      x = triangle.v3.x
      y = triangle.v3.y
      triangle.v3.x = (x * cos(angle.toRadians) - y * sin(angle.toRadians)).toFloat
      triangle.v3.y = (x * sin(angle.toRadians) + y * cos(angle.toRadians)).toFloat
    })
  }

  def translate(x:Float,y:Float,z:Float) = {
    triangles.foreach(triangle =>{
      triangle.normal.x += x
      triangle.normal.y += y
      triangle.normal.z += z
      triangle.v1.x += x
      triangle.v1.y += y
      triangle.v1.z += z
      triangle.v2.x += x
      triangle.v2.y += y
      triangle.v2.z += z
      triangle.v3.x += x
      triangle.v3.y += y
      triangle.v3.z += z
    })
  }

}
