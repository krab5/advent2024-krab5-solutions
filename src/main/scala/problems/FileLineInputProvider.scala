package problems

import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileReader
import java.io.FileNotFoundException

import scala.annotation.tailrec

/**
 * Convenient input provider that extracts a list of lines from a given file.
 * The input for this provider is the path to the file to be loaded.
 */
class FileLineInputProvider extends InputProvider[String,List[String]]:
  /** The content. */
  private var content: List[String] = List()

  /**
   * Sets up the provider with the given file name. This will load the file and read it
   * till the end, recording every line in the internal state, in order.
   *
   * @param input file name to load
   * @throws java.io.FileNotFoundException if the file has not been found or cannot be read
   */
  override def setup(input: String): Unit = {
    val file = new File(input)
    if (!file.exists() || !file.canRead()) {
      throw new FileNotFoundException(s"File $input could not be found or is not readable")
    }
    var reader = new BufferedReader(new FileReader(file))
    @tailrec
    def line(acc : List[String]): List[String] = Option[String](reader.readLine()) match {
      case None => acc.reverse
      case Some(l) => line(l :: acc)
    }

    content = line(List())
    reader.close()
  }

  /**
   * Get the lines read from the file during setup.
   *
   * @return list of lines from the file
   */
  override def retrieve(): List[String] = this.content


