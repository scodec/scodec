package scodec
package codecs

import scodec.bits.BitVector

private[scodec] final class TupleCodec[A, B](A: Codec[A], B: Codec[B]) extends Codec[(A, B)] {

  override def encode(t: (A, B)) =
    Codec.encodeBoth(A, B)(t._1, t._2)

  override def decode(buffer: BitVector) =
    Codec.decodeBoth(A, B)(buffer)

  def ~~[C](C: Codec[C]): Tuple3Codec[A,B,C] = new Tuple3Codec(A,B,C)

  override def toString = s"($A, $B)"
}

private[scodec] class Tuple3Codec[A,B,C](A: Codec[A],
                                         B: Codec[B],
                                         C: Codec[C]) extends Codec[(A,B,C)] {
  def ~~[D](D: Codec[D]) = new Tuple4Codec(A,B,C,D)

  override def decode(bits: BitVector) = {
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
    } yield DecodeResult((a.value,b.value,c.value), c.remainder)
  }

  override def encode(abc: (A,B,C)) =
    for {
      bits <- A.encode(abc._1)
      bits2 <- B.encode(abc._2)
      bits3 <- C.encode(abc._3)
    } yield bits ++ bits2 ++ bits3

  def widenOpt[X](to: (A,B,C) => X, from: X => Option[(A,B,C)]): Codec[X] = 
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C)"
}

private[scodec] class Tuple4Codec[A,B,C,D](A: Codec[A],
                                           B: Codec[B],
                                           C: Codec[C],
                                           D: Codec[D]) extends Codec[(A,B,C,D)] {
  def ~~[E](E: Codec[E]) = new Tuple5Codec(A,B,C,D,E)

  override def decode(bits: BitVector) = 
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value), d.remainder)

  override def encode(abcd: (A,B,C,D)) =
    for {
      bits <- A.encode(abcd._1)
      bits2 <- B.encode(abcd._2)
      bits3 <- C.encode(abcd._3)
      bits4 <- D.encode(abcd._4)
    } yield bits ++ bits2 ++ bits3 ++ bits4


  def widenOpt[X](to: (A,B,C,D) => X, from: X => Option[(A,B,C,D)]): Codec[X] = 
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D)"
}

private[scodec] class Tuple5Codec[A,B,C,D,E](A: Codec[A],
                                             B: Codec[B],
                                             C: Codec[C],
                                             D: Codec[D],
                                             E: Codec[E]) extends Codec[(A,B,C,D,E)] {
  def ~~[F](F: Codec[F]) = new Tuple6Codec(A,B,C,D,E,F)

  override def decode(bits: BitVector) = 
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value),e.remainder)

  override def encode(abcde: (A,B,C,D,E)) =
    for {
      bits <- A.encode(abcde._1)
      bits2 <- B.encode(abcde._2)
      bits3 <- C.encode(abcde._3)
      bits4 <- D.encode(abcde._4)
      bits5 <- E.encode(abcde._5)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5

  def widenOpt[X](to: (A,B,C,D,E) => X, from: X => Option[(A,B,C,D,E)]): Codec[X] = 
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E)"
}

private[scodec] class Tuple6Codec[A,B,C,D,E,F](A: Codec[A],
                                               B: Codec[B],
                                               C: Codec[C],
                                               D: Codec[D],
                                               E: Codec[E],
                                               F: Codec[F]) extends Codec[(A,B,C,D,E,F)] {
  def ~~[G](G: Codec[G]) = new Tuple7Codec(A,B,C,D,E,F,G)

  override def decode(bits: BitVector) = 
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value),f.remainder)

  override def encode(abcdef: (A,B,C,D,E,F)) = 
    for {
      bits <- A.encode(abcdef._1)
      bits2 <- B.encode(abcdef._2)
      bits3 <- C.encode(abcdef._3)
      bits4 <- D.encode(abcdef._4)
      bits5 <- E.encode(abcdef._5)
      bits6 <- F.encode(abcdef._6)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6


  def widenOpt[X](to: (A,B,C,D,E,F) => X, from: X => Option[(A,B,C,D,E,F)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})
    
  override def toString = s"($A, $B, $C, $D, $E, $F)"
}

private[scodec] class Tuple7Codec[A,B,C,D,E,F,G](A: Codec[A],
                                                 B: Codec[B],
                                                 C: Codec[C],
                                                 D: Codec[D],
                                                 E: Codec[E],
                                                 F: Codec[F],
                                                 G: Codec[G]) extends Codec[(A,B,C,D,E,F,G)] {
  def ~~[H](H: Codec[H]) = new Tuple8Codec(A,B,C,D,E,F,G,H)

  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value),g.remainder)

  override def encode(abcdefg: (A,B,C,D,E,F,G)) =
    for {
      bits <- A.encode(abcdefg._1)
      bits2 <- B.encode(abcdefg._2)
      bits3 <- C.encode(abcdefg._3)
      bits4 <- D.encode(abcdefg._4)
      bits5 <- E.encode(abcdefg._5)
      bits6 <- F.encode(abcdefg._6)
      bits7 <- G.encode(abcdefg._7)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7

  def widenOpt[X](to: (A,B,C,D,E,F,G) => X, from: X => Option[(A,B,C,D,E,F,G)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G)"
}

private[scodec] class Tuple8Codec[A,B,C,D,E,F,G,H](A: Codec[A],
                                                   B: Codec[B],
                                                   C: Codec[C],
                                                   D: Codec[D],
                                                   E: Codec[E],
                                                   F: Codec[F],
                                                   G: Codec[G],
                                                   H: Codec[H]) extends Codec[(A,B,C,D,E,F,G,H)] {
  def ~~[I](I: Codec[I]) = new Tuple9Codec(A,B,C,D,E,F,G,H,I)

  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
      h <- H.decode(g.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value,h.value),h.remainder)

  override def encode(abcdefgh: (A,B,C,D,E,F,G,H)) =
    for {
      bits <- A.encode(abcdefgh._1)
      bits2 <- B.encode(abcdefgh._2)
      bits3 <- C.encode(abcdefgh._3)
      bits4 <- D.encode(abcdefgh._4)
      bits5 <- E.encode(abcdefgh._5)
      bits6 <- F.encode(abcdefgh._6)
      bits7 <- G.encode(abcdefgh._7)
      bits8 <- H.encode(abcdefgh._8)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7 ++ bits8

  def widenOpt[X](to: (A,B,C,D,E,F,G,H) => X, from: X => Option[(A,B,C,D,E,F,G,H)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G, $H)"
}

private[scodec] class Tuple9Codec[A,B,C,D,E,F,G,H,I](A: Codec[A],
                                                   B: Codec[B],
                                                   C: Codec[C],
                                                   D: Codec[D],
                                                   E: Codec[E],
                                                   F: Codec[F],
                                                   G: Codec[G],
                                                   H: Codec[H],
                                                   I: Codec[I]) extends Codec[(A,B,C,D,E,F,G,H,I)] {
  def ~~[J](J: Codec[J]) = new Tuple10Codec(A,B,C,D,E,F,G,H,I,J)

  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
      h <- H.decode(g.remainder)
      i <- I.decode(h.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value,h.value,i.value),i.remainder)

  override def encode(abcdefghi: (A,B,C,D,E,F,G,H,I)) =
    for {
      bits <- A.encode(abcdefghi._1)
      bits2 <- B.encode(abcdefghi._2)
      bits3 <- C.encode(abcdefghi._3)
      bits4 <- D.encode(abcdefghi._4)
      bits5 <- E.encode(abcdefghi._5)
      bits6 <- F.encode(abcdefghi._6)
      bits7 <- G.encode(abcdefghi._7)
      bits8 <- H.encode(abcdefghi._8)
      bits9 <- I.encode(abcdefghi._9)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7 ++ bits8 ++ bits9

  def widenOpt[X](to: (A,B,C,D,E,F,G,H,I) => X, from: X => Option[(A,B,C,D,E,F,G,H,I)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G, $H, $I)"
}

private[scodec] class Tuple10Codec[A,B,C,D,E,F,G,H,I,J](A: Codec[A],
                                                        B: Codec[B],
                                                        C: Codec[C],
                                                        D: Codec[D],
                                                        E: Codec[E],
                                                        F: Codec[F],
                                                        G: Codec[G],
                                                        H: Codec[H],
                                                        I: Codec[I],
                                                        J: Codec[J]) extends Codec[(A,B,C,D,E,F,G,H,I,J)] {
  def ~~[K](K: Codec[K]) = new Tuple11Codec(A,B,C,D,E,F,G,H,I,J,K)

  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
      h <- H.decode(g.remainder)
      i <- I.decode(h.remainder)
      j <- J.decode(i.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value,h.value,i.value,j.value),j.remainder)

  override def encode(abcdefghij: (A,B,C,D,E,F,G,H,I,J)) =
    for {
      bits <- A.encode(abcdefghij._1)
      bits2 <- B.encode(abcdefghij._2)
      bits3 <- C.encode(abcdefghij._3)
      bits4 <- D.encode(abcdefghij._4)
      bits5 <- E.encode(abcdefghij._5)
      bits6 <- F.encode(abcdefghij._6)
      bits7 <- G.encode(abcdefghij._7)
      bits8 <- H.encode(abcdefghij._8)
      bits9 <- I.encode(abcdefghij._9)
      bits10 <- J.encode(abcdefghij._10)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7 ++ bits8 ++ bits9 ++ bits10

  def widenOpt[X](to: (A,B,C,D,E,F,G,H,I,J) => X, from: X => Option[(A,B,C,D,E,F,G,H,I,J)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G, $H, $I, $J)"
}

private[scodec] class Tuple11Codec[A,B,C,D,E,F,G,H,I,J,K](A: Codec[A],
                                                          B: Codec[B],
                                                          C: Codec[C],
                                                          D: Codec[D],
                                                          E: Codec[E],
                                                          F: Codec[F],
                                                          G: Codec[G],
                                                          H: Codec[H],
                                                          I: Codec[I],
                                                          J: Codec[J],
                                                          K: Codec[K]) extends Codec[(A,B,C,D,E,F,G,H,I,J,K)] {
  def ~~[L](L: Codec[L]) = new Tuple12Codec(A,B,C,D,E,F,G,H,I,J,K,L)

  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
      h <- H.decode(g.remainder)
      i <- I.decode(h.remainder)
      j <- J.decode(i.remainder)
      k <- K.decode(j.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value,h.value,i.value,j.value,k.value),k.remainder)

  override def encode(abcdefghijk: (A,B,C,D,E,F,G,H,I,J,K)) =
    for {
      bits <- A.encode(abcdefghijk._1)
      bits2 <- B.encode(abcdefghijk._2)
      bits3 <- C.encode(abcdefghijk._3)
      bits4 <- D.encode(abcdefghijk._4)
      bits5 <- E.encode(abcdefghijk._5)
      bits6 <- F.encode(abcdefghijk._6)
      bits7 <- G.encode(abcdefghijk._7)
      bits8 <- H.encode(abcdefghijk._8)
      bits9 <- I.encode(abcdefghijk._9)
      bits10 <- J.encode(abcdefghijk._10)
      bits11 <- K.encode(abcdefghijk._11)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7 ++ bits8 ++ bits9 ++ bits10 ++ bits11

  def widenOpt[X](to: (A,B,C,D,E,F,G,H,I,J,K) => X, from: X => Option[(A,B,C,D,E,F,G,H,I,J,K)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K)"
}

private[scodec] class Tuple12Codec[A,B,C,D,E,F,G,H,I,J,K,L](A: Codec[A],
                                                          B: Codec[B],
                                                          C: Codec[C],
                                                          D: Codec[D],
                                                          E: Codec[E],
                                                          F: Codec[F],
                                                          G: Codec[G],
                                                          H: Codec[H],
                                                          I: Codec[I],
                                                          J: Codec[J],
                                                          K: Codec[K],
                                                          L: Codec[L]) extends Codec[(A,B,C,D,E,F,G,H,I,J,K,L)] {
  override def decode(bits: BitVector) =
    for {
      a <- A.decode(bits)
      b <- B.decode(a.remainder)
      c <- C.decode(b.remainder)
      d <- D.decode(c.remainder)
      e <- E.decode(d.remainder)
      f <- F.decode(e.remainder)
      g <- G.decode(f.remainder)
      h <- H.decode(g.remainder)
      i <- I.decode(h.remainder)
      j <- J.decode(i.remainder)
      k <- K.decode(j.remainder)
      l <- L.decode(k.remainder)
    } yield DecodeResult((a.value,b.value,c.value,d.value,e.value,f.value,g.value,h.value,i.value,j.value,k.value,l.value),l.remainder)

  override def encode(abcdefghijkl: (A,B,C,D,E,F,G,H,I,J,K,L)) =
    for {
      bits <- A.encode(abcdefghijkl._1)
      bits2 <- B.encode(abcdefghijkl._2)
      bits3 <- C.encode(abcdefghijkl._3)
      bits4 <- D.encode(abcdefghijkl._4)
      bits5 <- E.encode(abcdefghijkl._5)
      bits6 <- F.encode(abcdefghijkl._6)
      bits7 <- G.encode(abcdefghijkl._7)
      bits8 <- H.encode(abcdefghijkl._8)
      bits9 <- I.encode(abcdefghijkl._9)
      bits10 <- J.encode(abcdefghijkl._10)
      bits11 <- K.encode(abcdefghijkl._11)
      bits12 <- L.encode(abcdefghijkl._12)
    } yield bits ++ bits2 ++ bits3 ++ bits4 ++ bits5 ++ bits6 ++ bits7 ++ bits8 ++ bits9 ++ bits10 ++ bits11 ++ bits12

  def widenOpt[X](to: (A,B,C,D,E,F,G,H,I,J,K,L) => X, from: X => Option[(A,B,C,D,E,F,G,H,I,J,K,L)]): Codec[X] =
    this.exmap(to.tupled andThen Attempt.successful, {x => Attempt.fromOption(from(x), Err(s"could not extract a tuple from: $x"))})

  override def toString = s"($A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L)"
}
