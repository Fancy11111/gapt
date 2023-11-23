package gapt.formats.tptp

import gapt.formats.ClasspathInputFile
import org.specs2.mutable.Specification

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

  "tffProblem" in {
    loadTPTP( "ANA134_1.002.032.p" )
    // loadTPTP( "ANA134_1.002.032.p" ).inputs.foreach( v => print( TptpToString.tptpInput( v ) ) )
    // TptpTypeChecker.extractTypes( loadTPTP( "ANA134_1.002.032.p" ) ).foreach( println( _ ) )
    // TptpTypeChecker.topLevelTypes( loadTPTP( "ANA134_1.002.032.p" ) ).foreach( println( _ ) )
    ok
  }
}
