package xyz.hyperreal.numbers

import scala.Integral.Implicits._

abstract class AbstractQuaternionRational[T: Numeric, Q <: Quaternion[T, Double, Q, QuaternionDouble]] extends AbstractQuaternion[T, Q] {

  def ^(p: Int): Q = pow(this.asInstanceOf[Q], p)

  def ^(p: BigInt): Q = pow(this.asInstanceOf[Q], p)

  private def pow[P: Integral](b: Q, e: P): Q = {
    val ie = implicitly[Integral[P]]
    val two = ie.fromInt(2)

    def _pow(b: Q, e: P): Q =
      if (e == ie.one)
        b
      else if (ie.rem(e, two) != ie.zero)
        b * _pow(b * b, ie.minus(e, ie.one) / two)
      else
        _pow(b * b, e / two)

    if (b == b.zero)
      if (e == ie.zero)
        sys.error("0^0 is undefined")
      else
        b.zero
    else if (e == ie.zero)
      b.one
    else if (ie.lt(e, ie.zero))
      _pow(b, ie.negate(e)).inverse
    else
      _pow(b, e)
  }

// 	private def pow[N: Numeric, P: Integral]( b: N, e: P, inv: Option[N => N] ) = {
// 		val ie = implicitly[Integral[P]]
// 		val ib = implicitly[Numeric[N]]
// 		val two = ie.fromInt(2)
// 		
// 		def _pow( b: N, e: P ): N =
// 			if (e == ie.one)
// 				b
// 			else
// 				if (ie.rem( e, two ) != ie.zero)
// 					b*_pow( b*b, ie.minus(e, ie.one)/two )
// 				else
// 					_pow( b*b, e/two )
//
// 		if (b == ib.zero)
// 			if (e == ie.zero)
// 				sys.error( "0^0 is undefined" )
// 			else
// 				ib.zero
// 		else if (e == ie.zero)
// 			ib.one
// 		else if (ie.lt( e, ie.zero ))
// 			if (inv == None)
// 				sys.error( "exponent must be non-negative" )
// 			else
// 				inv.get( _pow(b, ie.negate(e)) )
// 		else
// 			_pow( b, e )
// 	}

}
