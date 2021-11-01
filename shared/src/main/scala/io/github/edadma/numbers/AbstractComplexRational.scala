package io.github.edadma.numbers

import scala.Integral.Implicits._

abstract class AbstractComplexRational[T: Numeric, C <: Complex[T, Double, C, ComplexDouble]] extends AbstractComplex[T, C] {

  def ^(p: Int): C = pow(this.asInstanceOf[C], p)

  def ^(p: BigInt): C = pow(this.asInstanceOf[C], p)

  private def pow[P: Integral](b: C, e: P): C = {
    val ie = implicitly[Integral[P]]
    val two = ie.fromInt(2)

    def _pow(b: C, e: P): C =
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
