package at.logic.gapt.integration_tests

import at.logic.gapt.examples.LinearExampleProof
import at.logic.gapt.expr._
import at.logic.gapt.expr.fol.{ Numeral, Utils }
import at.logic.gapt.expr.hol.containsQuantifier
import at.logic.gapt.grammars.DeltaTableMethod
import at.logic.gapt.proofs.{ Ant, Sequent }
import at.logic.gapt.proofs.expansion.{ ETWeakening, ExpansionProof, FOLInstanceTermEncoding }
import at.logic.gapt.cutintro._
import at.logic.gapt.proofs.lk.{ CutRule, ForallLeftRule, quantRulesNumber }
import at.logic.gapt.provers.escargot.Escargot
import org.specs2.mutable._

class CutIntroTest extends Specification {
  private def LinearExampleTermset( n: Int ): List[FOLTerm] =
    if ( n == 0 )
      List[FOLTerm]()
    else
      Utils.numeral( n - 1 ) :: LinearExampleTermset( n - 1 )

  "CutIntroduction" should {
    "extract and decompose the termset of the linear example proof (n = 9)" in {
      val proof = LinearExampleProof( 9 )

      val ( termset, _ ) = FOLInstanceTermEncoding( proof )
      val set = termset collect { case FOLFunction( _, List( arg ) ) => arg }

      CutIntroduction( proof ) must beSome

      set must contain( exactly( LinearExampleTermset( 9 ): _* ) )
    }

    "maxsat method" in {
      CutIntroduction( LinearExampleProof( 9 ), method = MaxSATMethod( 1 ) ) must beSome
    }

    "reforest method" in {
      CutIntroduction( LinearExampleProof( 9 ), method = ReforestMethod ) must beSome
    }

    "linear equality example" in {
      val f = FOLFunctionConst( "f", 1 )
      val g = FOLFunctionConst( "g", 1 )
      val x = FOLVar( "x" )
      val c = FOLConst( "c" )

      val Some( p ) = Escargot getLKProof (
        All( x, f( g( x ) ) === f( x ) ) +:
        Sequent()
        :+ ( f( ( g ^ 9 )( c ) ) === f( c ) )
      )
      val Some( q ) = CutIntroduction( p )
      val cutFormulas = q.subProofs collect { case c: CutRule => c.cutFormula } filter { containsQuantifier( _ ) }
      cutFormulas must contain( atMost(
        All( x, f( ( g ^ 3 )( x ) ) === f( x ) ),
        All( x, f( x ) === f( ( g ^ 3 )( x ) ) )
      ) )
    }

    "introduce two cuts into linear example proof with improveSolutionLK" in {
      def fun( n: Int, t: FOLTerm ): FOLTerm = if ( n == 0 ) t else FOLFunction( "s", fun( n - 1, t ) :: Nil )
      val proof = LinearExampleProof( 8 )
      val f = proof.endSequent( Ant( 0 ) ).asInstanceOf[FOLFormula]
      val a1 = FOLVar( "α_1" )
      val a2 = FOLVar( "α_2" )
      val zero = FOLConst( "0" )

      val u1 = a1
      val u2 = fun( 1, a1 )
      val us = for ( f <- proof.endSequent )
        yield f.asInstanceOf[FOLFormula] -> ( if ( containsQuantifier( f ) ) List( List( u1 ), List( u2 ) ) else List( List() ) )
      val s11 = a2
      val s12 = fun( 2, a2 )
      val s21 = zero
      val s22 = fun( 4, zero )

      val ss = ( a1 :: Nil, ( s11 :: Nil ) :: ( s12 :: Nil ) :: Nil ) :: ( a2 :: Nil, ( s21 :: Nil ) :: ( s22 :: Nil ) :: Nil ) :: Nil
      val grammar = new SchematicExtendedHerbrandSequent( us, ss )
      val ehs = SolutionStructure( grammar, CutIntroduction.computeCanonicalSolution( grammar ) )
      val prover = CutIntroduction.BackgroundTheory.PureFOL.prover
      val result_new = improveSolutionLK( ehs, prover, hasEquality = false )
      val r_proof = CutIntroduction.buildProofWithCut( result_new, prover )

      // expected result
      val cf1 = FOLAtom( "P", a1 ) --> FOLAtom( "P", fun( 2, a1 ) )
      val cf2 = FOLAtom( "P", a2 ) --> FOLAtom( "P", fun( 4, a2 ) )

      result_new.formulas must beEqualTo( cf1 :: cf2 :: Nil )

      quantRulesNumber( r_proof ) must_== grammar.size
    }

    "introduce weak quantifiers as low as possible" in {
      val endSequent = hos"!x q(x), q(c)->p(0), !x (p(x)&q(c)->p(s(x))) :- p(${Numeral( 9 )})"
      val Some( proof ) = Escargot.getLKProof( endSequent )
      val Some( proofWithCut ) = CutIntroduction( proof )

      // !x q(x) must only be instantiated once, even though it is used in both branches of the cut.
      proofWithCut.treeLike.postOrder.filter {
        case p: ForallLeftRule => p.mainFormula == hof"!x q(x)"
        case _                 => false
      } must haveSize( 1 )
    }

    "filter bottom during beautification" in {
      val Some( expansion ) = Escargot.getExpansionProof( hos"!x (p x -> p (s x)) :- p 0 -> p ${Numeral( 9 )}" )
      val weirdExpansion = ExpansionProof(
        ETWeakening( hof"!x (p x & -p x)", Polarity.InAntecedent ) +:
          expansion.expansionSequent
      )
      CutIntroduction.compressToSolutionStructure( weirdExpansion ) must beNone
    }
  }
}

