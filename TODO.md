Current Work
============
 - Basic combinators
   - integers, longs, strings
   - fixed size
   - variable size
   - tuples via ~ (ala parser combinators)
   - hlists w/ case class bindings

Future Improvements
===================
 - Implementation of ByteVector that minimizes boxing/unboxing
   - Need fast ++ and indexed access
   - Slow insertion is okay
   - Integrate Akka's ByteString?
 - Implementation of BitVector backed by Vector[Long]
   - A non-specialized Vector[Long] should result in 1/8th of total operations on large vectors (and 1/8th boxing/unboxing)
   - Test if this is faster than current Vector[Byte]
