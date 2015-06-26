package scodec.codecs

import scodec.bits.{ BitVector, ByteOrdering }

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }

@State(Scope.Benchmark)
class IntCodecBenchmark {

  private val bits64high = BitVector.high(64)
  private val int32generic = new IntCodec(32, true, ByteOrdering.BigEndian)
  private val int32specialized = new Int32Codec(ByteOrdering.BigEndian)

  @Benchmark def encode_int32_generic = int32generic.encode(-1)
  @Benchmark def decode_int32_generic = int32generic.decode(bits64high)
  @Benchmark def encode_int32_specialized = int32specialized.encode(-1)
  @Benchmark def decode_int32_specialized = int32specialized.decode(bits64high)
  @Benchmark def roundtrip_int32_generic = int32generic.decode(int32generic.encode(-1).require)
  @Benchmark def roundtrip_int32_specialized = int32specialized.decode(int32specialized.encode(-1).require)
}
