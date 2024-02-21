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

  "a defined as $i > $o; ! [A: $i > $o,B:$i,C:$i*$o : a(B)]" in {

    val parser = new TptpParser( "! [A: $i > $o,B:$i,C:$i*$o,D:$o] : a(B)" )
    val l = parser.tff_quantified_formula.run()
    l match {
      case Success( value ) => println( value( Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ) ).toString() )
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

  "Should fail type mismatch: ! [A: $i > $o,B:$i,C:$i*$o : a(B)]" in {

    val parser = new TptpParser( "! [A: $i > $o,B:$i,C:$i*$o,D:$o] : a(A)" )
    val l = parser.tff_quantified_formula.run()
    l match {
      case Success( value ) => {
        println( value( Ctx( Ctx(), "a", Var( "a", gapt.expr.ty.TArr( Ti, To ) ) ) ).toString() )
        failure
      }
      case Failure( e: ParseError ) => {
        println( parser.formatError( e, new ErrorFormatter( showTraces = true ) ) )
        failure
      }

      case Failure( e: IllegalArgumentException ) => {
        ok
      }
      case Failure( exception ) => {
        println( "cause" )
        failure
      }
    }
    ok
  }

}
