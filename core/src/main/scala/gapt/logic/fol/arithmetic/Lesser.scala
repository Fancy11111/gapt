
package gapt.logic.fol.arithmetic
import gapt.expr.formula.constants.LesserC
import gapt.expr.Apps
import gapt.expr.formula.fol.FOLAtom
import gapt.expr.Expr
import gapt.expr.formula.fol.FOLTerm
import gapt.expr
import gapt.expr.formula.fol.FOLExpression
import gapt.expr.formula.fol.FOLFormula
import gapt.expr.formula._

object Lesser {
  def apply( a: Expr, b: Expr ): Atom = Apps( LesserC( a.ty ), a, b ).asInstanceOf[Atom]
  def apply( a: FOLTerm, b: FOLTerm ): FOLAtom =
    apply( a, b.asInstanceOf[Expr] ).asInstanceOf[FOLAtom]

  def unapply( e: Expr ): Option[( Expr, Expr )] = e match {
    case expr.App( expr.App( LesserC( _ ), a ), b ) => Some( a, b )
    case _ => None
  }
  def unapply( f: FOLFormula ): Option[( FOLTerm, FOLTerm )] = unapply( f.asInstanceOf[FOLExpression] )
  def unapply( f: FOLExpression ): Option[( FOLTerm, FOLTerm )] = f.asInstanceOf[Expr] match {
    case Lesser( a: FOLTerm, b: FOLTerm ) => Some( a, b )
    case _                                => None
  }
}
