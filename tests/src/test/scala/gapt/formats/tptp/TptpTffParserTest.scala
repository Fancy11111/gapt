package gapt.formats.tptp

import gapt.formats.ClasspathInputFile
import org.specs2.mutable.Specification
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.util.Success
import scala.util.Failure
import org.parboiled2.ParseError
import org.parboiled2.ErrorFormatter
import gapt.expr.ty.To
import gapt.expr.ty.Ti
import gapt.expr.Var
import org.specs2.specification.core.Fragments
import scala.util.Try
import gapt.expr.formula.Formula

class TptpTffParserTest extends Specification {

  def loadTPTP( fileName: String ): TptpFile =
    resolveIncludes(
      TptpFile( Seq( IncludeDirective( fileName, None ) ) ),
      fileName => TptpImporter.loadWithoutIncludes( ClasspathInputFile( fileName ) ) )

  "A: $i > $o,B:$i,C:$i*$o" in {

    val l = new TptpParser( "A: $i > $o,B:$i,C:$i*$o" ).tff_variable_list.run()
    l match {
      case Success( value )     => println( value.map( _( Ctx() ) ).foldLeft( "" )( _ + ", " + _ ) )
      case Failure( exception ) => failure
    }
    ok
  }

  "typedef formula" in {
    val parser = new TptpParser( "tff(animal_type,type, animal: $tType )." )
    val res = parser.typedef_formula.run()
    res match {
      case Success( value ) => println( value( new Ctx( Map(), Map() ) ) )
      case Failure( e: ParseError ) => {
        println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
        failure
      }
      case Failure( exception ) => {
        println( "cause" )
        failure
      }
    }
    ok
  }

  "tff formula" >> {

    Fragments.foreach( Seq(
      ( "(![X:$i] : p(X)) | (?[X:$o]: q(X))", new Ctx( Map( "p" -> Var( "p", gapt.expr.ty.TArr( Ti, To ) ), "q" -> Var( "q", gapt.expr.ty.TArr( To, To ) ) ), Map() ), ( p: TptpParser ) => ( p.tff_logic_formula.run() ) ),
      ( "![X:$i] : (p(X) & ![X:$o] : q(X))", new Ctx( Map( "p" -> Var( "p", gapt.expr.ty.TArr( Ti, To ) ), "q" -> Var( "q", gapt.expr.ty.TArr( To, To ) ) ), Map() ), ( p: TptpParser ) => ( p.tff_logic_formula.run() ) ),
      ( "![X:$i] : (![X:$o] : q(X)  => p(X) )", new Ctx( Map( "p" -> Var( "p", gapt.expr.ty.TArr( Ti, To ) ), "q" -> Var( "q", gapt.expr.ty.TArr( To, To ) ) ), Map() ), ( p: TptpParser ) => ( p.tff_logic_formula.run() ) ),
      ( "! [A: $i > $o,B:$i,C:$i*$o,D:$o] : a(B)", Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ), ( p: TptpParser ) => ( p.tff_quantified_formula.run() ) ),
      ( "! [A: $int,B:$int] : $less($uminus(A), B)", Ctx(), ( p: TptpParser ) => ( p.tff_quantified_formula.run() ) ) ) ) {
      case ( exp, ctx, pRun ) =>
        exp in {
          val parser = new TptpParser( exp )
          val res = pRun( parser )
          res match {
            case Success( value ) => println( value( ctx ) )
            case Failure( e: ParseError ) => {
              println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
              failure
            }
            case Failure( exception ) => {
              println( "cause" )
              failure
            }
          }
          ok
        }
    }
  }

  "should fail" >> {

    "Should fail type mismatch: ! [A: $i > $o,B:$i,C:$i*$o : a(B)]" in {

      val parser = new TptpParser( "! [A: $i > $o,B:$i,C:$i*$o,D:$o] : a(A)" )
      val l = parser.tff_quantified_formula.run()
      l match {
        case Success( value ) => {
          value( Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ) ) must throwA[IllegalArgumentException]
          ok
        }
        case Failure( e: ParseError ) => {
          println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
          failure
        }
        case Failure( exception ) => {
          println( "cause" )
          failure
        }
      }
      ok
    }

    "Should fail type mismatch: ! [A: $int, B: $real] : $less($difference(A,B), $difference(B,A))" in {

      val parser = new TptpParser( "! [A: $int, B: $real] : $less($difference(A,B), $difference(B,A))" )
      val l = parser.tff_quantified_formula.run()
      l match {
        case Success( value ) => {
          try {
            value( Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ) )
            failure
          } catch {
            case e: IllegalArgumentException => {
              println( e.getMessage() )
              ok
            }
            case e: Exception => {
              println( e )
              failure
            }
          }
        }
        case Failure( e: ParseError ) => {
          println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
          failure
        }
        case Failure( exception ) => {
          println( "cause" )
          failure
        }
      }
      ok
    }

    "Should fail type mismatch: ? [A: $int, B: $real] : $less(A,B)" in {

      val parser = new TptpParser( "? [A: $int, B: $real] : $greatereq(A,B)" )
      val l = parser.tff_quantified_formula.run()
      l match {
        case Success( value ) => {
          try {
            value( Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ) )
            failure
          } catch {
            case e: IllegalArgumentException => {
              println( e.getMessage() )
              ok
            }
            case e: Exception => {
              println( e )
              failure
            }
          }
        }
        case Failure( e: ParseError ) => {
          println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
          failure
        }
        case Failure( exception ) => {
          println( "cause" )
          failure
        }
      }
      ok
    }

  }

  "test files" >> {

    Fragments.foreach( Seq( "TF0.p", "ANA134_tff_fragment.p", "TF0_typed_and_untyped_variables.p", "TF0_typed_and_untyped_variables_shadowing.p" ) ) { file_name =>
      file_name in
        {
          val file = ClasspathInputFile( file_name )
          val parser = new TptpParser( file.read )
          val res = parser.TPTP_file.run()
          res match {
            case Success( value ) => {
              val interpreted = value( new Ctx( Map(), Map() ) )
              ok
            }
            case Failure( e: ParseError ) => {
              println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
              failure
            }

            case Failure( e: IllegalArgumentException ) => {
              println( "cause" )
              e.printStackTrace()
              failure
            }
            case Failure( exception ) => {
              println( "cause" )
              exception.printStackTrace()
              failure
            }

          }
          ok
        }
    }
  }

}
