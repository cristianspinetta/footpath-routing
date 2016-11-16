package utils

trait CollectionUtils {

  //  def turnInRing[E, T <: Traversable[E]](elems: T): T = {
  //    if (elems.size <= 1) elems
  //    else                 {
  //      val es : T = elems.companion.apply((elems.head))
  //      es
  //    }
  //  }

  def removeDuplicated[A](elems: List[A], customEquals: (A, A) ⇒ Boolean): List[A] = {
    elems.foldRight(List.empty[A]) {
      (curr, unique) ⇒
        {
          if (!unique.exists(customEquals(curr, _)))
            curr +: unique
          else
            unique
        }
    }
  }

  def fixedGroup[A](list: Seq[A], quantity: Int): Seq[Seq[A]] = {
    list.grouped((list.size + quantity - 1) / quantity).toSeq
  }

}

object CollectionUtils extends CollectionUtils
