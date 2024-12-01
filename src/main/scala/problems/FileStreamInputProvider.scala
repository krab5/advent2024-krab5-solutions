package problems

import parser.stream.Stream
import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileReader

/**
 * An implementation of [[problems.InputProvider]] that transforms a file into
 * a [[parser.stream.Stream]]. This is usually better than using [[problems.FileLineInputProvider]]
 * as it streams the file as needed ("lazily", one could say).
 *
 * The input for this input provider is the file name.
 */
class FileStreamInputProvider extends InputProvider[String,Stream[Char]]:
  /**
   * Inner implementation of [[parser.stream.Stream]] that is returned by the
   * provider (and is not intended to be used directly by the user).
   *
   * This reads a character whenever needed (i.e. whenever [[parser.stream.Stream.eat]] is
   * called). It uses a [[java.io.BufferedReader]] internally to improve reading performances.
   *
   * @constructor builds the reader stream from the given file
   * @param file file to use as streaming source
   */
  class ReaderStream(val file: File) extends Stream[Char]:
    /** The internal reader */
    private var reader: Reader = new BufferedReader(new FileReader(file))
    /** This keeps track of the number of char read for `fork`. */
    private var nread: Int = 0
    /** Boolean to store if we have reached the end of the file */
    private var eosReached: Boolean = false

    /**
     * Test if the stream has reached the end of the file.
     *
     * @return true if the stream is done
     */
    override def eos: Boolean = eosReached

    /**
     * Read a character from the file, or return [[None]] if the end of file was reached.
     * Update the inner state of the stream.
     *
     * @return the character read, or [[None]] if none
     */
    override def eat: Option[Char] = {
      val x = reader.read()
      if (x < 0) {
        this.eosReached = true
        this.reader.close()
        None
      } else {
        nread = nread + 1
        Some(x.toChar)
      }
    }

    /**
     * Split the stream into to new streams.
     *
     * **NOTE:** this implementation if VERY LOUSY. If it is ever required that I have
     * to split streams I will provide a better mechanism (with ideally one shared reader
     * and multiple local buffers or something like that).
     *
     * @return the two stream duplicata
     */
    override def fork: (Stream[Char],Stream[Char]) = {
      def mkReader: ReaderStream = {
        val rd = new ReaderStream(this.file)
        rd.reader.skip(this.nread)
        rd.nread = this.nread
        rd.eosReached = this.eosReached
        return rd
      }
      val one = mkReader
      val two = mkReader
      this.reader.close()
      return (one,two)
    }

  /** Inner file variable */
  private var stream : ReaderStream = null

  /** 
   * Set up the input provider with the given file name.
   *
   * @param input file path to be loaded
   */
  override def setup(input: String): Unit = { 
    this.stream = new ReaderStream(new File(input))
  }

  /**
   * Retrieve the data from the input provider (the stream associated to the
   * specified file).
   *
   * @return the stream associated to the file
   */
  override def retrieve(): Stream[Char] = this.stream


