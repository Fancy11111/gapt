package gapt.logic.fol.arithmetic

import gapt.expr.ty.TBase
import gapt.expr.ty.Ty

object Numeric {
  def isNumeric( ty: Ty ) = ty == TInt || ty == TReal || ty == TRat
}

object TInt extends TBase( "int", List() )
object TRat extends TBase( "rat", List() )
object TReal extends TBase( "real", List() )
