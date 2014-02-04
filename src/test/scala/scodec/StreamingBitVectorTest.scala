package scodec

object StreamingBitVectorTest extends App {

  println { """
  |Simple integration test for streaming, lazy-I/O-backed `BitVector`.
  |When viewing output, we are looking for two things:
  |
  | * Speed should be "reasonable", between 10-100 MB/s if touching
  |   each byte, and up to 4 GB/s or so if we are just measuring time
  |   to load the series of chunks.
  | * Memory usage should be bounded by the chunk size, not by overall
  |   file size. That is, the garbage collector should be able to
  |   reclaim previous chunks when the stream is traversed in a linear
  |   fashion. Try running with -Xmx100m to verify this.
  """.stripMargin
  }

  def time[A](label: String, mb: Double)(a: => A): A = {
    val start = System.currentTimeMillis
    val result = a
    val t = (System.currentTimeMillis.toDouble - start) / 1000.0
    println(s"$label took $t seconds, ${math.round(mb/t)} MB/s")
    result
  }

  def printMemoryStats(): Unit = {
    val R = java.lang.Runtime.getRuntime
    println(s"Max memory:   ${R.maxMemory.toDouble / 1e6} MB")
    println(s"Total memory: ${R.totalMemory.toDouble / 1e6} MB")
    println(s"Free memory:  ${R.freeMemory.toDouble / 1e6} MB")
  }

  val Stride = 4096
  @annotation.tailrec
  def countBits(b: BitVector, acc: Long, touchBytes: Boolean): Long = {
    if (b.isEmpty) acc
    else {
      val (h, t) = (b.take(Stride), b.drop(Stride))
      var i = 0
      if (touchBytes) {
        val bytes = h.toByteVector
        while (i < bytes.size) {
          bytes(i)
          i += 1
        }
      }
      countBits(t, acc + h.size, touchBytes)
    }
  }
  import java.io._
  val file = new File("test.largefile")
  if (!file.exists) {
    println("To test large file-backed streaming BitVector support,")
    println("place a large file at ./test.largefile")
  }
  else {
    def go(touchBytes: Boolean): Unit = {
      val in = new FileInputStream(file)
      val nioIn1 = (new FileInputStream(file)).getChannel
      val nioIn2 = (new FileInputStream(file)).getChannel
      val size = nioIn1.size.toDouble / 1e6
      println("Processing file of size: " + size + " MB")
      println("Touching each byte read: " + touchBytes)
      try {
        time("BitVector.fromInputStream", size) {
          // NB: if we declare `val b1 = BitVector.fromInputStream(..)`, this
          // will run out of memory, since head of stream makes entire stream
          // reachable!
          countBits(BitVector.fromInputStream(in), 0, touchBytes)
          printMemoryStats
        }
        time("BitVector.fromChannel", size) {
          countBits(BitVector.fromChannel(nioIn1), 0, touchBytes)
          printMemoryStats
        }
        time("BitVector.fromMmap", size) {
          countBits(BitVector.fromMmap(nioIn2), 0, touchBytes)
          printMemoryStats
        }
      }
      finally {
        print("closing files... ")
        in.close
        nioIn1.close
        nioIn2.close
        println("done")
      }
    }
    go(false)
    go(true)
  }
}
