package at.logic.gapt.proofs.lkNew

import at.logic.gapt.expr._
import at.logic.gapt.proofs.lk.base._
import org.specs2.execute.Success
import org.specs2.mutable._

/**
 * Created by sebastian on 8/6/15.
 */
class LKNewTest extends Specification {
  val s = FOLConst( "s" )

  val A = FOLAtom( "A", Nil )
  val B = FOLAtom( "B", Nil )
  val C = FOLAtom( "C", Nil )
  val D = FOLAtom( "D", Nil )
  val E = FOLAtom( "E", Nil )
  val F = FOLAtom( "F", Nil )

  val Ps = FOLAtom( "P", s )

  private def testParents( o: OccConnector, ruleName: String )( sequent: HOLSequent, parents: Seq[SequentIndex]* ): Success = {
    val ( m, n ) = sequent.sizes
    for ( ( i, ps ) <- sequent.indices zip parents ) {
      o.parents( i ) aka s"$ruleName: Parents of $i in $sequent should be $ps" must beEqualTo( ps )
    }
    o.parents( Ant( m ) ) aka s"Parents of ${Ant( m )} in $sequent" must throwAn[IndexOutOfBoundsException]
    o.parents( Suc( n ) ) aka s"Parents of ${Suc( n )} in $sequent" must throwAn[IndexOutOfBoundsException]
    success
  }

  private def testChildren( o: OccConnector, ruleName: String )( sequent: HOLSequent, children: Seq[SequentIndex]* ): Success = {
    val ( m, n ) = sequent.sizes
    for ( ( i, cs ) <- sequent.indices zip children ) {
      o.children( i ) aka s"$ruleName: Children of $i in $sequent should be $cs" must beEqualTo( cs )
    }

    o.children( Ant( m ) ) aka s"Parents of ${Ant( m )} in $sequent" must throwAn[IndexOutOfBoundsException]
    o.children( Suc( n ) ) aka s"Parents of ${Suc( n )} in $sequent" must throwAn[IndexOutOfBoundsException]
    success
  }

  "LogicalAxiom" should {
    "correctly create an axiom" in {
      LogicalAxiom( A )

      success
    }

    "correctly return its main formula" in {
      val ax = LogicalAxiom( A )

      if ( ax.mainIndices.length != 2 )
        failure

      val ( i1, i2 ) = ( ax.mainIndices.head, ax.mainIndices.tail.head )
      ax.endSequent( i1 ) must beEqualTo( A )
      ax.endSequent( i2 ) must beEqualTo( A )
    }
  }

  "ReflexivityAxiom" should {
    "correctly create an axiom" in {
      ReflexivityAxiom( s )

      success
    }

    "correctly return its main formula" in {
      val ax = ReflexivityAxiom( s )

      if ( ax.mainIndices.length != 1 )
        failure

      val i = ax.mainIndices.head
      ax.endSequent( i ) must beEqualTo( Eq( s, s ) )
    }
  }

  "WeakeningLeftRule" should {
    "correctly create a proof" in {
      WeakeningLeftRule( LogicalAxiom( A ), Ps )

      success
    }

    "correctly return its main formula" in {
      val p = WeakeningLeftRule( LogicalAxiom( A ), Ps )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( Ps )
    }

    "correctly connect occurrences" in {
      //end sequent of p: B, A :- A
      val p = WeakeningLeftRule( LogicalAxiom( A ), B )

      val o = p.getOccConnector

      testChildren( o, "w_l" )(
        p.premise,
        Seq( Ant( 1 ) ),

        Seq( Suc( 0 ) )
      )

      testParents( o, "w_l" )(
        p.endSequent,
        Seq(),
        Seq( Ant( 0 ) ),
        Seq( Suc( 0 ) )
      )
    }
  }

  "WeakeningRightRule" should {
    "correctly create a proof" in {
      WeakeningRightRule( LogicalAxiom( A ), B )

      success
    }

    "correctly return its main formula" in {
      val p = WeakeningRightRule( LogicalAxiom( A ), B )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( B )
    }

    "correctly connect occurrences" in {
      // end sequent of p: A :- A, B
      val p = WeakeningRightRule( LogicalAxiom( A ), B )

      val o = p.getOccConnector

      testChildren( o, "w_r" )(
        p.endSequent,
        Seq( Ant( 0 ) ),
        Seq( Suc( 0 ) )
      )

      testParents( o, "w_r" )(
        p.endSequent,
        Seq( Ant( 0 ) ),
        Seq( Suc( 0 ) ),
        Seq()
      )
    }
  }

  "ContractionLeftRule" should {

    "correctly create a proof" in {
      ContractionLeftRule( WeakeningLeftRule( LogicalAxiom( A ), A ), Ant( 0 ), Ant( 1 ) )
      ContractionLeftRule( WeakeningLeftRule( LogicalAxiom( A ), A ), A )

      success
    }

    "refuse to construct a proof" in {
      ContractionLeftRule( LogicalAxiom( A ), Ant( 0 ), Ant( 1 ) ) must throwAn[LKRuleCreationException]
      ContractionLeftRule( WeakeningLeftRule( LogicalAxiom( A ), Ps ), Ant( 0 ), Ant( 1 ) ) must throwAn[LKRuleCreationException]
      ContractionLeftRule( LogicalAxiom( A ), Ant( 0 ), Ant( 0 ) ) must throwAn[LKRuleCreationException]
      ContractionLeftRule( LogicalAxiom( Ps ), A ) must throwAn[LKRuleCreationException]
      ContractionLeftRule( LogicalAxiom( A ), A ) must throwAn[LKRuleCreationException]
    }

    "correctly return its main formula" in {
      val p = ContractionLeftRule( WeakeningLeftRule( LogicalAxiom( A ), A ), A )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( A )
    }

    "correctly return its aux formulas" in {
      val p = ContractionLeftRule( WeakeningLeftRule( LogicalAxiom( A ), A ), A )

      if ( p.auxIndices.length != 1 )
        failure
      if ( p.auxIndices.head.length != 2 )
        failure

      for ( i <- p.auxIndices.head ) {
        p.premise( i ) must beEqualTo( A )
      }
      success
    }

    "correctly connect occurrences" in {
      // end sequent of p: A, B, C :- A, B
      val p = ContractionLeftRule( ArbitraryAxiom( B +: A +: C +: A +: Sequent() :+ A :+ B ), A )

      val o = p.getOccConnector

      testParents( o, "c_l" )(
        p.endSequent,
        Seq( Ant( 1 ), Ant( 3 ) ),
        Seq( Ant( 0 ) ),
        Seq( Ant( 2 ) ),
        Seq( Suc( 0 ) ),
        Seq( Suc( 1 ) )
      )

      testChildren( o, "c_l" )(
        p.premise,
        Seq( Ant( 1 ) ),
        Seq( Ant( 0 ) ),
        Seq( Ant( 2 ) ),
        Seq( Ant( 0 ) ),

        Seq( Suc( 0 ) ),
        Seq( Suc( 1 ) )
      )
    }
  }

  "ContractionRightRule" should {

    "correctly create a proof" in {
      ContractionRightRule( WeakeningRightRule( LogicalAxiom( A ), A ), Suc( 0 ), Suc( 1 ) )
      ContractionRightRule( WeakeningRightRule( LogicalAxiom( A ), A ), A )

      success
    }

    "refuse to construct a proof" in {
      ContractionRightRule( LogicalAxiom( A ), Suc( 0 ), Suc( 1 ) ) must throwAn[LKRuleCreationException]
      ContractionRightRule( WeakeningRightRule( LogicalAxiom( A ), Ps ), Suc( 0 ), Suc( 1 ) ) must throwAn[LKRuleCreationException]
      ContractionRightRule( LogicalAxiom( A ), Suc( 0 ), Suc( 0 ) ) must throwAn[LKRuleCreationException]
      ContractionRightRule( LogicalAxiom( Ps ), A ) must throwAn[LKRuleCreationException]
      ContractionRightRule( LogicalAxiom( A ), A ) must throwAn[LKRuleCreationException]
    }

    "correctly return its main formula" in {
      val p = ContractionRightRule( WeakeningRightRule( LogicalAxiom( A ), A ), A )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( A )
    }

    "correctly return its aux formulas" in {
      val p = ContractionRightRule( WeakeningRightRule( LogicalAxiom( A ), A ), A )

      if ( p.auxIndices.length != 1 )
        failure
      if ( p.auxIndices.head.length != 2 )
        failure

      for ( i <- p.auxIndices.head ) {
        p.premise( i ) must beEqualTo( A )
      }
      success
    }

    "correctly connect occurrences" in {
      // end sequent of p: A, B :- B, C, A
      val p = ContractionRightRule( ArbitraryAxiom( A +: B +: Sequent() :+ A :+ B :+ A :+ C ), Suc( 0 ), Suc( 2 ) )

      val o = p.getOccConnector

      testParents( o, "c_r" )(
        p.endSequent,
        Seq( Ant( 0 ) ),
        Seq( Ant( 1 ) ),

        Seq( Suc( 1 ) ),
        Seq( Suc( 3 ) ),
        Seq( Suc( 0 ), Suc( 2 ) )
      )

      testChildren( o, "c_r" )(
        p.premise,
        Seq( Ant( 0 ) ),
        Seq( Ant( 1 ) ),

        Seq( Suc( 2 ) ),
        Seq( Suc( 0 ) ),
        Seq( Suc( 2 ) ),
        Seq( Suc( 1 ) )
      )
    }
  }

  "CutRule" should {
    "correctly produce a proof" in {
      CutRule( ArbitraryAxiom( A +: B +: Sequent() :+ B ), Suc( 0 ), LogicalAxiom( B ), Ant( 0 ) )
      CutRule( ArbitraryAxiom( A +: B +: Sequent() :+ B ), LogicalAxiom( B ), B )

      success
    }

    "refuse to produce a proof" in {
      val p1 = ArbitraryAxiom( Sequent() :+ A :+ B )
      val p2 = ArbitraryAxiom( C +: B +: Sequent() )

      CutRule( p1, Ant( 0 ), p2, Ant( 0 ) ) must throwAn[LKRuleCreationException]
      CutRule( p1, Suc( 0 ), p2, Suc( 0 ) ) must throwAn[LKRuleCreationException]
      CutRule( p1, Suc( 0 ), p2, Ant( 0 ) ) must throwAn[LKRuleCreationException]
      CutRule( p1, Suc( 2 ), p2, Ant( 0 ) ) must throwAn[LKRuleCreationException]
      CutRule( p1, Suc( 0 ), p2, Ant( 3 ) ) must throwAn[LKRuleCreationException]
    }

    "correctly return its aux formulas" in {
      val p1 = ArbitraryAxiom( Sequent() :+ A :+ B )
      val p2 = ArbitraryAxiom( C +: B +: Sequent() )

      val p = CutRule( p1, p2, B )
      if ( p.auxIndices.length != 2 )
        failure
      if ( ( p.auxIndices.head.length != 1 ) || ( p.auxIndices.tail.head.length != 1 ) )
        failure

      val ( i, j ) = ( p.auxIndices.head.head, p.auxIndices.tail.head.head )

      p.leftPremise( i ) must beEqualTo( B )
      p.rightPremise( j ) must beEqualTo( B )
    }

    "correctly connect occurrences" in {
      val p1 = ArbitraryAxiom( A +: B +: Sequent() :+ A :+ B :+ C )
      val p2 = ArbitraryAxiom( D +: B +: E +: F +: Sequent() :+ B :+ E )

      // end sequent of p: A, B, D, E, F :- A, C, B, E
      val p = CutRule( p1, p2, B )
      val oL = p.getLeftOccConnector
      val oR = p.getRightOccConnector

      testChildren( oL, "cut" )(
        p.leftPremise,
        Seq( Ant( 0 ) ),
        Seq( Ant( 1 ) ),

        Seq( Suc( 0 ) ),
        Seq(),
        Seq( Suc( 1 ) )
      )

      testParents( oL, "cut" )(
        p.endSequent,
        Seq( Ant( 0 ) ),
        Seq( Ant( 1 ) ),
        Seq(),
        Seq(),
        Seq(),

        Seq( Suc( 0 ) ),
        Seq( Suc( 2 ) ),
        Seq(),
        Seq()
      )

      testChildren( oR, "cut" )(
        p.rightPremise,
        Seq( Ant( 2 ) ),
        Seq(),
        Seq( Ant( 3 ) ),
        Seq( Ant( 4 ) ),

        Seq( Suc( 2 ) ),
        Seq( Suc( 3 ) )
      )

      testParents( oR, "cut" )(
        p.endSequent,
        Seq(),
        Seq(),
        Seq( Ant( 0 ) ),
        Seq( Ant( 2 ) ),
        Seq( Ant( 3 ) ),

        Seq(),
        Seq(),
        Seq( Suc( 0 ) ),
        Seq( Suc( 1 ) )
      )
    }
  }

  "NegLeftRule" should {

    "correctly create a proof" in {
      NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), Suc( 0 ) )
      NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), C )

      success
    }

    "refuse to create a proof" in {
      NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), Ant( 0 ) ) must throwAn[LKRuleCreationException]
      NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), Suc( 2 ) ) must throwAn[LKRuleCreationException]
      NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), A ) must throwAn[LKRuleCreationException]
    }

    "correctly return its main formula" in {
      val p = NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D ), C )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( Neg( C ) )
    }

    "correctly return its aux formulas" in {
      val p = NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D :+ E ), C )

      if ( p.auxIndices.length != 1 )
        failure
      if ( p.auxIndices.head.length != 1 )
        failure

      for ( i <- p.auxIndices.head ) {
        p.premise( i ) must beEqualTo( C )
      }
      success
    }

    "correctly connect occurrences" in {
      // end sequent of p: ¬D, A, B :- C, E
      val p = NegLeftRule( ArbitraryAxiom( A +: B +: Sequent() :+ C :+ D :+ E ), D )

      val o = p.getOccConnector

      testChildren( o, "¬:l" )(
        p.premise,
        Seq( Ant( 1 ) ),
        Seq( Ant( 2 ) ),

        Seq( Suc( 0 ) ),
        Seq( Ant( 0 ) ),
        Seq( Suc( 1 ) )
      )

      testParents( o, "¬:l" )(
        p.endSequent,
        Seq( Suc( 1 ) ),
        Seq( Ant( 0 ) ),
        Seq( Ant( 1 ) ),

        Seq( Suc( 0 ) ),
        Seq( Suc( 2 ) )
      )
    }
  }

  "AndLeftRule" should {

    "correctly create a proof" in {
      AndLeftRule( WeakeningLeftRule( LogicalAxiom( A ), B ), Ant( 0 ), Ant( 1 ) )
      AndLeftRule( WeakeningLeftRule( LogicalAxiom( A ), B ), A, B )
      AndLeftRule( WeakeningLeftRule( LogicalAxiom( A ), B ), And( A, B ) )

      success
    }

    "refuse to construct a proof" in {
      AndLeftRule( LogicalAxiom( A ), Ant( 0 ), Ant( 1 ) ) must throwAn[LKRuleCreationException]
      AndLeftRule( LogicalAxiom( A ), Ant( 0 ), Ant( 0 ) ) must throwAn[LKRuleCreationException]
      AndLeftRule( LogicalAxiom( B ), A ) must throwAn[LKRuleCreationException]
    }

    "correctly return its main formula" in {
      val p = AndLeftRule( WeakeningLeftRule( LogicalAxiom( A ), B ), A, B )

      if ( p.mainIndices.length != 1 )
        failure

      val i = p.mainIndices.head

      p.endSequent( i ) must beEqualTo( And( A, B ) )
    }

    "correctly return its aux formulas" in {
      val p = AndLeftRule( WeakeningLeftRule( LogicalAxiom( A ), B ), A, B )

      if ( p.auxIndices.length != 1 )
        failure
      if ( p.auxIndices.head.length != 2 )
        failure

      p.premise( p.auxIndices.head.head ) must beEqualTo( A )
      p.premise( p.auxIndices.head.tail.head ) must beEqualTo( B )
      success
    }

    "correctly connect occurrences" in {
      // end sequent of p: A, B, C :- A, B
      val p = AndLeftRule( ArbitraryAxiom( B +: A +: C +: A +: Sequent() :+ A :+ B ), A, A )

      val o = p.getOccConnector

      testParents( o, "∧_l" )(
        p.endSequent,
        Seq( Ant( 1 ), Ant( 3 ) ),
        Seq( Ant( 0 ) ),
        Seq( Ant( 2 ) ),
        Seq( Suc( 0 ) ),
        Seq( Suc( 1 ) )
      )

      testChildren( o, "∧_l" )(
        p.premise,
        Seq( Ant( 1 ) ),
        Seq( Ant( 0 ) ),
        Seq( Ant( 2 ) ),
        Seq( Ant( 0 ) ),

        Seq( Suc( 0 ) ),
        Seq( Suc( 1 ) )
      )
    }
  }
}
