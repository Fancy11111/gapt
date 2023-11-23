package gapt.formats.tptp

import gapt.expr.ty.Ty
import scala.collection.mutable
import gapt.expr.ty.Ti
import gapt.formats.tptp.AnnotatedFormula
import gapt.formats.tptp.AnnotatedFormula
import gapt.formats.tptp.AnnotatedFormula
import gapt.expr.Expr
import gapt.expr.ty.To
import gapt.expr.formula.fol.FOLAtom

object TptpTypeChecker {
  val knownTypes = mutable.Map[String, Ty]();

  def extractTypes( tf: TptpFile ): Map[String, Ty] = {
    tf match {
      case TptpFile( inputs ) => inputs.map( _ match {
        case Typedef( _, n, tn, ty, _ ) => ( tn, ty )
        case any                        => ( null, Ti )
      } ).filter( _ match {
        case ( null, _ ) => false
        case _           => true
      } ).groupBy( _._1 ).map { case ( k, v ) => ( k, v.map( _._2 ).head ) }
    }
  }

  def checkTypes( tf: TptpFile ) = {
    tf.inputs.map {
      case AnnotatedFormula( _, _, _, f, _ ) => f
      case e                                 => null
    }.filter( _ != null ).forall( f => infertType( f ) == To )
  }

  def topLevelTypes( tf: TptpFile ) = {
    val types = extractTypes( tf )
    tf.inputs.map {
      case AnnotatedFormula( _, n, _, f, _ ) => ( f )
      case _                                 => null
    }.filter( _ != null )
  }

  def infertType( e: Expr ) = {
    e.ty
  }

  def addToContext( name: String, t: Ty ) = {}
}

sealed trait Result[T]
case class Ok[T]( res: T ) extends Result[T]
case class Err[T]() extends Result[T]

class Context() {
  var ctx = mutable.Map[String, Ty]()
  var sublevel: Context = null
  def addToContext( name: String, t: Ty ) = {
    if ( ctx.contains( name ) ) {
      Err()
    } else {
      ctx = ctx.addOne( name, t )
      Ok( null )
    }
  }

  def getDefinedType( name: String ): Option[Ty] = {
    if ( sublevel != null ) {
      sublevel.getDefinedType( name ) match {
        case Some( value ) => Some( value )
        case None          => ctx.get( name )
      }
    }
    ctx.get( name )
  }
}

object Context {
  def apply( ctx: Context ): Context = {
    var c = new Context()
    c.sublevel = ctx;
    return c
  }

  def unapply( ctx: Context ): Context = {
    ctx.sublevel
  }
}

