Before 1.0
============
 - Better ScalaDoc
 - Implement a choice codec

Future Improvements
===================
 - Implementation of ByteVector that minimizes boxing/unboxing
   - Need fast ++ and indexed access
   - Slow insertion is okay
   - Integrate Akka's ByteString via optional dependency? Don't want to rebuild on each Akka y version change.
