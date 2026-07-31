package gapt.formats.tptp

import gapt.formats.ClasspathInputFile
import org.specs2.mutable.Specification
import scala.util.Success
import scala.util.Failure
import org.parboiled2.ParseError
import org.parboiled2.ErrorFormatter

class TptpTffParserTest extends Specification {

  def loadTPTP(fileName: String) =
    resolveIncludes(
      TptpFile(Seq(IncludeDirective(fileName, None))),
      fileName => TptpTffImporter.loadWithoutIncludes(ClasspathInputFile(fileName))
    )

  "gra014p1" in {
    loadTPTP("GRA014+1.p")
    ok
  }
  //
  // "tautological clauses" in {
  //   TptpProblemToResolution(loadTPTP("HWV116-1_excerpt.p"))
  //   ok
  // }
  // "mapping type" in {
  "(aType * bType * $i) > $o" in {
    val parser = TptpTffParser("(aType * bType * $i) > $o")
    parser.tff_mapping_type.run() match {
      case Failure(error: ParseError) =>
        throw new IllegalArgumentException(s"Parse error:\n" +
          parser.formatError(error, new ErrorFormatter(showTraces = true)))
      case Failure(exception) => throw exception
      case Success(value) => {
        println(value)
        ok
      }
    }
  }
  // }

}
