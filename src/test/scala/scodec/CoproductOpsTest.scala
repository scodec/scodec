package scodec

import shapeless._
import CoproductOps._

import org.scalatest.{ WordSpec, Matchers }

class CoproductOpsTest extends WordSpec with Matchers {

  "coproduct type alignment" should {
    type ISB = Int :+: String :+: Boolean :+: CNil

    "identity" in {
      val align = implicitly[Align[ISB, ISB]]
      def reflect(isb: ISB) = align(isb) shouldBe isb
      reflect(Coproduct[ISB](1))
      reflect(Coproduct[ISB]("Hi"))
      reflect(Coproduct[ISB](true))
    }

    "reorder" in {
      type BSI = Boolean :+: String :+: Int :+: CNil
      val align = implicitly[Align[ISB, BSI]]
      Coproduct[ISB](1).align[BSI] shouldBe Coproduct[BSI](1)
      Coproduct[ISB]("Hi").align[BSI] shouldBe Coproduct[BSI]("Hi")
      Coproduct[ISB](true).align[BSI] shouldBe Coproduct[BSI](true)
    }
  }
}
