package snapshot

import java.io._
import java.util.zip._

trait ObjectSerializer {

  @throws(classOf[java.io.IOException])
  def serialize[A](o: A): Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream(512)
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray
  }

  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  def deserialize[A](buffer: Array[Byte]): A = {
    val in =
      new ObjectInputStream(new ByteArrayInputStream(buffer)) {
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
          try { Class.forName(desc.getName, false, getClass.getClassLoader) }
          catch { case ex: ClassNotFoundException ⇒ super.resolveClass(desc) }
        }
      }
    in.readObject().asInstanceOf[A]
  }

  @throws(classOf[java.io.IOException])
  def byteArrayToGZipFile(byteArray: Array[Byte], file: File): Unit = {
    val fis = new ByteArrayInputStream(byteArray)
    val fos = new FileOutputStream(file)
    val gos = new GZIPOutputStream(fos)
    doCopy(fis, gos)
  }

  @throws(classOf[java.io.IOException])
  def gzipToByteArray(file: File): Array[Byte] = {
    val fis2 = new FileInputStream(file)
    val gis = new GZIPInputStream(fis2)
    val fos2 = new ByteArrayOutputStream(512)
    doCopy(gis, fos2)
    fos2.toByteArray
  }

  private def doCopy(is: InputStream, os: OutputStream) = {
    var oneByte = is.read()

    while (oneByte != -1) {
      os.write(oneByte)
      oneByte = is.read()
    }

    os.close()
    is.close()
  }
}

object ObjectSerializer extends ObjectSerializer
