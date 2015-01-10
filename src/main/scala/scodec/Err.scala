package scodec

/**
 * Describes an error.
 *
 * An error has a message and a list of context identifiers that provide insight into where an error occurs in a large structure.
 *
 * This type is not sealed so that codecs can return domain specific
 * subtypes and dispatch on those subtypes.
 */
trait Err {

  /** Gets a description of the error. */
  def message: String

  /**
   * Gets a stack of context identifiers.
   *
   * The head of the list is the outermost (i.e., least specific) identifier.
   */
  def context: List[String]

  /** Gets a description of the error with the context identifiers prefixing the message. */
  def messageWithContext: String =
    (if (context.isEmpty) "" else context.mkString("", "/", ": ")) + message

  /** Returns a new error with the specified context identifier pushed in to the context stack. */
  def pushContext(ctx: String): Err

  override def toString = messageWithContext
}

/** Companion for [[Err]]. */
object Err {

  final case class General(message: String, context: List[String]) extends Err {
    def this(message: String) = this(message, Nil)
    def pushContext(ctx: String) = copy(context = ctx :: context)
  }

  final case class InsufficientBits(needed: Long, have: Long, context: List[String]) extends Err {
    def this(needed: Long, have: Long) = this(needed, have, Nil)
    def message = s"cannot acquire $needed bits from a vector that contains $have bits"
    def pushContext(ctx: String) = copy(context = ctx :: context)
  }

  final case class MatchingDiscriminatorNotFound[A](a: A, context: List[String]) extends Err {
    def this(a: A) = this(a, Nil)
    def message = s"could not find matching case for $a"
    def pushContext(ctx: String) = copy(context = ctx :: context)
  }

  def apply(message: String): Err = new General(message)
  def insufficientBits(needed: Long, have: Long): Err = new InsufficientBits(needed, have)
}
