/**
 * Description:
 */
package at.logic.gapt.integration_tests

import at.logic.gapt.expr.hol.isAtom
import at.logic.gapt.formats.readers.XMLReaders._
import at.logic.gapt.formats.tptp.TPTPFOLExporter
import at.logic.gapt.formats.xml.XMLParser._
import at.logic.gapt.proofs.HOLClause
import at.logic.gapt.proofs.ceres._
import at.logic.gapt.proofs.lk.{ deleteTautologies }
import at.logic.gapt.proofs.lkNew._
import at.logic.gapt.provers.prover9._
import java.io.File.separator
import org.specs2.mutable._

//NOTE: I removed the proof profile from this test

class LatticeTest extends Specification {
  def checkForProverOrSkip = Prover9.isInstalled must beTrue.orSkip

  sequential

  "The system" should {
    "parse, transform to LKsk, and extract the clause set for the lattice proof" in {
      checkForProverOrSkip

      val proofdb = XMLProofDatabaseParser( getClass.getClassLoader.getResourceAsStream( "lattice.xml" ) )
      proofdb.proofs.size must beEqualTo( 1 )
      val proof = DefinitionElimination( proofdb.Definitions )( lkOld2New( proofdb.proofs.head._2 ) )

      val s = extractStruct( proof, CERES.skipEquations )
      val css = CharacteristicClauseSet( s )
      val cs = deleteTautologies( css )
      Prover9.getRobinsonProof( cs ) must beSome
    }

    "parse, skolemize and apply CERES to the lattice proof" in {
      //      skipped( "doesn't work yet" )
      checkForProverOrSkip

      val proofdb = ( new XMLReader( getClass.getClassLoader.getResourceAsStream( "lattice.xml" ) ) with XMLProofDatabaseParser ).getProofDatabase()
      proofdb.proofs.size must beEqualTo( 1 )
      val proof = lkOld2New( proofdb.proofs.head._2 )

      val acnf = CERES( skolemize( proof ), CERES.skipNothing )
      ( acnf.endSequent multiSetEquals proof.endSequent ) must beTrue
      acnf.foreach( {
        case CutRule( p1, a1, p2, a2 ) => isAtom( p1.endSequent( a1 ) ) must beTrue
        case _                         => ()
      } )
      ok
    }

    "parse, skolemize and apply CERES to the lattice proof, skipping equational inferences" in {
      skipped( "doesn't work yet" ) //TODO: apparently there is a bug in projection computation which surfaces in this case
      checkForProverOrSkip

      val proofdb = ( new XMLReader( getClass.getClassLoader.getResourceAsStream( "lattice.xml" ) ) with XMLProofDatabaseParser ).getProofDatabase()
      proofdb.proofs.size must beEqualTo( 1 )
      val proof = lkOld2New( proofdb.proofs.head._2 )

      val acnf = CERES( skolemize( proof ), CERES.skipEquations )
      ( acnf.endSequent multiSetEquals proof.endSequent ) must beTrue
      acnf.foreach( {
        case CutRule( p1, a1, p2, a2 ) => isAtom( p1.endSequent( a1 ) ) must beTrue
        case _                         => ()
      } )
      ok
    }

  }
}
