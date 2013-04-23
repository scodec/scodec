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
   - Consider something like Akka's ByteString
 - Implementation of BitVector backed by Vector[Long]
   - Test if this is faster than current Vector[Byte]
