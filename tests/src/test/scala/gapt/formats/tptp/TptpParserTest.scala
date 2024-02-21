package gapt.formats.tptp

import gapt.formats.ClasspathInputFile
import org.specs2.mutable.Specification
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class TptpParserTest extends Specification {

  def loadTPTP( fileName: String ): TptpFile =
    resolveIncludes(
      TptpFile( Seq( IncludeDirective( fileName, None ) ) ),
      fileName => TptpImporter.loadWithoutIncludes( ClasspathInputFile( fileName ) ) )

  "gra014p1" in {
    loadTPTP( "GRA014+1.p" )
    ok
  }

  "tautological clauses" in {
    TptpProblemToResolution( loadTPTP( "HWV116-1_excerpt.p" ) )
    ok
  }

  "tffProblems" in {
    loadTPTP( "ANA134_1.002.032.p" ).inputs.foreach( v => print( TptpToString.tptpInput( v ) ) )
    ok
    // loadTPTP( "ANA134_1.002.032.p" )
    // TptpTypeChecker.extractTypes( loadTPTP( "ANA134_1.002.032.p" ) ).foreach( println( _ ) )
    // TptpTypeChecker.topLevelTypes( loadTPTP( "ANA134_1.002.032.p" ) ).foreach( println( _ ) )
  }
  // }.prepend( Files.newDirectoryStream( Paths.get( getClass.getResource( "/tff-problems" ).toURI() ) ).iterator.asScala.map( p => {
  //   "load " + p.getFileName.toString in {
  //     loadTPTP( "tff-problems/" + p.getFileName().toString() )
  //     ok
  //   }
  // } ).toSeq )
}
