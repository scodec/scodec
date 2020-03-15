package scodec

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import java.util.UUID
import scodec.bits.BitVector

// @BenchmarkMode(Array(Mode.AverageTime)) TODO
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class UuidBenchmark {
  val uuid = UUID.fromString("b0739ffd-d1f9-47b8-aa2e-b2be69733def")
  val codec = codecs.uuid

  val encoded: BitVector = encode.toOption.get // YOLO

  assert(decode.isSuccessful)

  @Benchmark def encode: Attempt[BitVector] =
    codec.encode(uuid.nn)

  @Benchmark def decode: Attempt[DecodeResult[UUID]] =
    codec.decode(encoded)
}
