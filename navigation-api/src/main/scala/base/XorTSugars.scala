package base

import cats.data.{ Xor, XorT }

import scala.concurrent.Future

object XorTSugars {
  implicit def toXorT[L, R](x: Future[Xor[L, R]]): XorT[Future, L, R] = XorT(x)
  implicit def toFuture[L, R](x: XorT[Future, L, R]): Future[Xor[L, R]] = x.value
}
