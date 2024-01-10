package gapt.formats.tptp

import gapt.formats.ClasspathInputFile
import org.specs2.mutable.Specification
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.util.Success
import scala.util.Failure

class TptpTffParserTest extends Specification {

  def loadTPTP( fileName: String ): TptpFile =
    resolveIncludes(
      TptpFile( Seq( IncludeDirective( fileName, None ) ) ),
      fileName => TptpImporter.loadWithoutIncludes( ClasspathInputFile( fileName ) ) )

  "tffBasics" in {

    val l = new TptpParser( "A: $i > $o, B: $i, C: $i * $o" ).tff_variable_list.run()
    l match {
      case Success( value )     => println(value)
      case Failure( exception ) => failure
    }
    ok
  }
}
