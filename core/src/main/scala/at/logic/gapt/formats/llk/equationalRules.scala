/*
 * equationalRules.scala
 *
 */

package at.logic.gapt.formats.llk

import at.logic.gapt.expr.{ Substitution => SubstitutionLambda, rename => renameLambda, _ }

//TODO: perhaps there is a better place for this
object EquationVerifier {
  //results
  abstract class ReplacementResult
  case object Equal extends ReplacementResult
  case object Different extends ReplacementResult
  case class EqualModuloEquality( path: List[Int] ) extends ReplacementResult

  def apply( s: LambdaExpression, t: LambdaExpression, e1: LambdaExpression, e2: LambdaExpression ) = checkReplacement( s, t, e1, e2 )
  //this is a convenience method, apart from that everything is general
  def apply( eq: HOLFormula, e1: HOLFormula, e2: HOLFormula ): Option[List[Int]] = {
    //println("try "+eq+" "+e1+" "+e2)
    eq match {
      case Eq( s, t ) => apply( s, t, e1, e2 ) match {
        case EqualModuloEquality( path ) =>
          //println("result:"+path)
          Some( path )
        case _ =>
          //println("no result")
          None
      }
      case _ => throw new Exception( "Error checking for term replacement in " + e1 + " and " + e2 + ": " + eq + " is not an Eq!" )
    }
  }

  def checkReplacement( s: LambdaExpression, t: LambdaExpression, e1: LambdaExpression, e2: LambdaExpression ): ReplacementResult = {
    //trace("matching "+e1+" against "+e2+" for "+s+" -> "+t)
    ( e1, e2 ) match {
      case _ if e1 == e2                    => Equal
      case _ if ( e1 == s ) && ( e2 == t )  => EqualModuloEquality( Nil )
      case ( Var( _, _ ), Var( _, _ ) )     => Different
      case ( Const( _, _ ), Const( _, _ ) ) => Different
      case ( App( l1, r1 ), App( l2, r2 ) ) =>
        ( checkReplacement( s, t, l1, l2 ), checkReplacement( s, t, r1, r2 ) ) match {
          case ( Equal, Equal )                       => Equal
          case ( EqualModuloEquality( path ), Equal ) => EqualModuloEquality( 1 :: path )
          case ( Equal, EqualModuloEquality( path ) ) => EqualModuloEquality( 2 :: path )
          case _                                      => Different
        }
      case ( Abs( v1 @ Var( name1, expt1 ), t1 ), Abs( v2 @ Var( name2, expt2 ), t2 ) ) =>
        if ( expt1 != expt2 )
          Different
        else {
          val vn = renameLambda( v1, freeVariables( s ).toList ++ freeVariables( t ).toList ++ freeVariables( t1 ).toList ++ freeVariables( t2 ).toList ) //TODO: pass the list on instead of recreating it
          checkReplacement( s, t, SubstitutionLambda( v1, vn )( t1 ), SubstitutionLambda( v2, vn )( t2 ) )
        }
      case _ => Different
    }
  }

}