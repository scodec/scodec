Current Work
============
 - Basic combinators
   - integers, longs, strings
   - fixed size
   - variable size
   - tuples via ~ (ala parser combinators)
   - hlists w/ case class bindings
 - Allow flatPrepend (and flatZip) to return a non-HList (or auto-lift to HList?)
   - Does not auto-lift but this is possible via .hlist now

Future Improvements
===================
 - Implementation of BitVector backed by a single int or long
   - Any operations that grow size past 4/8 bytes results in falling back to SimpleBitVector
 - Implementation of ByteVector that minimizes boxing/unboxing
   - Need fast ++ and indexed access
   - Slow insertion is okay
   - Integrate Akka's ByteString?
 - Implementation of BitVector backed by Vector[Long]
   - A non-specialized Vector[Long] should result in 1/8th of total operations on large vectors (and 1/8th boxing/unboxing)
   - Test if this is faster than current Vector[Byte]
