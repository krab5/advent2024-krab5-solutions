package problems

import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileReader
import java.io.FileNotFoundException

import annotation.tailrec

/**
 * A content provider based on a file that slurps the entire content of the file and
 * provides it as a string.
 *
 * This might not be ideal for very large files, but is convenient for most cases.
 */
class FileFullInputProvider extends InputProvider[String,String]:
  private var content : String = ""

  /**
   * Set up the provider with the given file name; this reads the file entirely
   * and put the content in the internal state of the provider.
   *
   * @param input file name to be slurped
   */
  override def setup(input: String): Unit = {
    val f: File = new File(input)
    if (!f.exists() || !f.canRead()) then {
      throw new FileNotFoundException(s"File $input could not be found or is not readable")
    }
    var reader = new BufferedReader(new FileReader(f))
    var buff: Array[Char] = Array.ofDim(FileFullInputProvider.BUFF_SIZE)
    var nr = 0
    this.content = ""

    @tailrec
    def readall(acc: String): String = {
      val nr = reader.read(buff, 0, FileFullInputProvider.BUFF_SIZE)
      if (nr > 0) then {
        readall(acc ++ Array.copyAs[Char](buff, nr))
      } else {
        acc
      }
    }

    this.content = readall("")
    reader.close()
  }

  /**
   * Get the content retrieved from the file.
   *
   * @return the content
   */
  override def retrieve(): String = this.content

object FileFullInputProvider {
  val BUFF_SIZE : Int = 256
}


