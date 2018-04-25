package gapt.proofs.expansion

import gapt.expr._
import gapt.expr.hol.lcomp
import gapt.proofs.rup._
import gapt.proofs.lk._
import gapt.proofs._
import gapt.provers.escargot.EscargotChaud
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs._
import gapt.provers.sat.Sat4j._
import gapt.utils.quiet
import org.sat4j.core.VecInt
import org.sat4j.tools.SearchListenerAdapter

import scala.collection.mutable

class ExpansionProofToMG3iViaSAT( val expansionProof: ExpansionProof ) {
  val solver = SolverFactory.newDefault()
  def newVar(): Int = solver.nextFreeVarId( true )

  implicit def clause2sat4j( clause: Iterable[Int] ): IVecInt =
    new VecInt( clause.toArray )

  val shAtoms = expansionProof.subProofs.
    map( _.shallow ).
    toSeq.sortBy( lcomp( _ ) ).
    map( sh => sh -> newVar() ).
    toMap
  def atom( f: Formula ): Int = shAtoms( f )
  def atom( e: ExpansionTree ): Int = atom( e.shallow )

  val atomToSh = shAtoms.map( _.swap )
  val atomToET = expansionProof.subProofs.groupBy( atom ).withDefaultValue( Set() )

  def modelSequent( lits: Traversable[Int] ): HOLSequent =
    Sequent( lits.flatMap( l => atomToSh.get( math.abs( l ) ).map( _ -> Polarity( l < 0 ) ) ) )
  def implication( lits: Traversable[Int] ): HOLSequent =
    modelSequent( lits ).swapped
  def expSeq( lits: Traversable[Int] ): ExpansionSequent =
    Sequent( lits.flatMap( l => atomToET( math.abs( l ) ).map( e => e -> e.polarity ) ) )

  val drup = mutable.Buffer[RupProof.Line]()
  solver.setSearchListener( new SearchListenerAdapter[ISolverService] {
    override def learnUnit( p: Int ) = drup += RupProof.Rup( Set( p ) )
    override def learn( c: IConstr ) = drup += RupProof.Rup( c.toSet )
  } )

  val proofs = mutable.Map[Set[Int], Either[LKProof, ( Set[Int], LKProof => LKProof )]]()
  def clause( seq: HOLSequent ): Seq[Int] = seq.map( -atom( _ ), atom ).elements
  def addClause( p: LKProof ): Unit = addClause( p, p.endSequent )
  def addClause( p: LKProof, seq: HOLSequent ): Unit = {
    val cls = clause( seq ).toSet
    if ( !proofs.contains( cls ) ) {
      proofs( cls ) = Left( p )
      drup += RupProof.Input( cls )
      solver.addClause( cls )
    }
  }
  def addClause( lower: HOLSequent, upper: HOLSequent )( p: LKProof => LKProof ): Unit = {
    val lowerC = clause( lower ).toSet
    val upperC = clause( upper ).toSet
    if ( !proofs.contains( lowerC ) ) {
      require( !solver.isSatisfiable( upperC.map( -_ ) ) )
      drup += RupProof.Rup( upperC )
      proofs( lowerC ) = Right( ( upperC, p ) )
      drup += RupProof.Input( lowerC )
      solver.addClause( lowerC )
    }
  }

  expansionProof.subProofs.foreach {
    case ETWeakening( _, _ )              =>
    case ETMerge( _, _ ) | ETAtom( _, _ ) => // implicit because shallow formulas are the same
    case ETTop( _ )                       => addClause( TopAxiom )
    case ETBottom( _ )                    => addClause( BottomAxiom )
    case ETAnd( a, b ) =>
      addClause( AndLeftMacroRule( LogicalAxiom( a.shallow ), a.shallow, b.shallow ) )
      addClause( AndLeftMacroRule( LogicalAxiom( b.shallow ), a.shallow, b.shallow ) )
      addClause( AndRightRule( LogicalAxiom( a.shallow ), Suc( 0 ), LogicalAxiom( b.shallow ), Suc( 0 ) ) )
    case ETOr( a, b ) =>
      addClause( OrLeftRule( LogicalAxiom( a.shallow ), Ant( 0 ), LogicalAxiom( b.shallow ), Ant( 0 ) ) )
      addClause( OrRightMacroRule( LogicalAxiom( a.shallow ), a.shallow, b.shallow ) )
      addClause( OrRightMacroRule( LogicalAxiom( b.shallow ), a.shallow, b.shallow ) )
    case e @ ETWeakQuantifier( sh, insts ) =>
      for ( ( inst, a ) <- insts ) addClause {
        if ( e.polarity.inSuc ) ExistsRightRule( LogicalAxiom( a.shallow ), sh, inst )
        else ForallLeftRule( LogicalAxiom( a.shallow ), sh, inst )
      }
    case ETNeg( a ) =>
      addClause( NegLeftRule( LogicalAxiom( a.shallow ), a.shallow ) )
    case ETImp( a, b ) =>
      addClause( ImpLeftRule( LogicalAxiom( a.shallow ), Suc( 0 ), LogicalAxiom( b.shallow ), Ant( 0 ) ) )
      addClause( ImpRightMacroRule( LogicalAxiom( b.shallow ), a.shallow, b.shallow ) )
    case ETStrongQuantifier( _, _, _ ) =>
  }

  val escargot = new EscargotChaud( shAtoms.keys.collect { case a: Atom => a }.toSeq )

  type Counterexample = Set[Int] // just the assumptions
  type Result = Either[Counterexample, Unit]

  val unprovable = mutable.Buffer[( Set[Var], Counterexample )]()

  def solve( eigenVariables: Set[Var], assumptions: Set[Int] ): Result = {
    unprovable.find {
      case ( evs, ass ) => evs.subsetOf( eigenVariables ) && assumptions.subsetOf( ass )
    } match {
      case Some( ( _, ass ) ) =>
        return Left( ass )
      case _ =>
    }

    while ( solver.isSatisfiable( assumptions ) ) {
      val model = solver.model(): Seq[Int]
      val atomModel = modelSequent( model ).collect { case a: Atom => a }

      def tryEquational(): Option[Result] = {
        if ( !atomModel.exists( Eq.unapply( _ ).isDefined ) ) None else
          quiet( escargot.getAtomicLKProof( atomModel ) ) match {
            case Some( p ) =>
              addClause( p )
              Some( Right( () ) )
            case _ => None
          }
      }

      val assumptionsAnt = assumptions.filter( _ > 0 )
      def checkEVCond( e: ExpansionTree ): Boolean =
        freeVariables( e.shallow ).
          intersect( expansionProof.eigenVariables ).
          subsetOf( eigenVariables )

      def minimizeCtx( ctx: Set[Int], upper: Set[Int] ): Set[Int] = {
        def go( todo: List[Int], ctx: Set[Int] ): Set[Int] =
          todo match {
            case t :: ts if !solver.isSatisfiable( upper union ( ctx - t ) ) => go( ts, ctx - t )
            case _ :: ts => go( ts, ctx )
            case Nil => ctx
          }
        go( ctx.toList, ctx )
      }
      def addClauseWithCtx( ctx: Set[Int], upper: Set[Int], lower: Set[Int] )( p: LKProof => LKProof ): Unit =
        if ( solver.isSatisfiable( ctx ) ) {
          val ctx2 = minimizeCtx( ctx, upper )
          addClause( upper = modelSequent( upper ++ ctx2 ), lower = modelSequent( lower ++ ctx2 ) )( p )
        }

      def tryInvertible(): Option[Result] =
        model.filter( _ > 0 ).flatMap( atomToET ).filter( checkEVCond ).collectFirst {
          case e @ ETStrongQuantifier( sh, ev, a ) if e.polarity.inAnt &&
            !eigenVariables.contains( ev ) && !assumptions.contains( atom( a ) ) =>
            val provable = solve( eigenVariables + ev, assumptions + atom( a ) )
            if ( provable.isRight ) addClauseWithCtx( assumptions, Set( atom( a ) ), Set( atom( e ) ) )( p =>
              if ( !p.endSequent.antecedent.contains( a.shallow ) ) p
              else ExistsLeftRule( p, sh, ev ) )
            provable
        }

      def tryNonInvertible(): Result = {
        val nextSteps = model.filter( _ < 0 ).map( -_ ).flatMap( atomToET ).filter( checkEVCond ).collect {
          case e @ ETNeg( a ) if e.polarity.inSuc && !assumptions.contains( atom( a ) ) =>
            ( Set( atom( a ) ), Set( -atom( e ) ), eigenVariables, ( p: LKProof ) =>
              if ( !p.endSequent.antecedent.contains( a.shallow ) ) p else
                NegRightRule( p, a.shallow ) )
          case e @ ETImp( a, b ) if e.polarity.inSuc && !assumptions.contains( atom( a ) ) =>
            ( Set( atom( a ), -atom( b ) ), Set( -atom( e ) ), eigenVariables,
              ImpRightMacroRule( _: LKProof, a.shallow, b.shallow ) )
          case e @ ETStrongQuantifier( _, ev, a ) if e.polarity.inSuc && !eigenVariables.contains( ev ) =>
            ( Set( -atom( a ) ), Set( -atom( e ) ), eigenVariables + ev, ( p: LKProof ) =>
              if ( !p.endSequent.succedent.contains( a.shallow ) ) p else
                ForallRightRule( p, e.shallow, ev ) )
        }
        nextSteps.find( s => solve( s._3, assumptionsAnt ++ s._1 ).isRight ) match {
          case Some( ( upper, lower, _, transform ) ) =>
            addClauseWithCtx( assumptionsAnt, upper, lower )( transform )
            Right( () )
          case None =>
            Left( assumptions )
        }
      }

      tryInvertible().getOrElse( tryNonInvertible() match {
        case ok @ Right( _ )    => ok
        case reason @ Left( _ ) => tryEquational().getOrElse( reason )
      } ) match {
        case Right( _ ) => // next model
          require( !solver.isSatisfiable( model ) )
        case reason @ Left( _ ) =>
          require( solver.isSatisfiable( assumptions ) )
          unprovable += ( ( eigenVariables, assumptions ) )
          return reason
      }
    }
    Right( () )
  }

  def solve(): Either[HOLSequent, LKProof] =
    ( try {
      for ( e <- expansionProof.expansionSequent.antecedent )
        addClause( LogicalAxiom( e.shallow ), Sequent() :+ e.shallow )
      solve( Set(), expansionProof.expansionSequent.succedent.map( -atom( _ ) ).toSet )
    } catch {
      case _: ContradictionException =>
        Right( () )
    } ) match {
      case Left( reason ) =>
        require( solver.isSatisfiable( reason ) )
        val model = solver.model().toSet
        Left( modelSequent( model.toSeq.sortBy( -_ ) ) )
      case Right( () ) =>
        val goal = clause( expansionProof.expansionSequent.shallow ).toSet
        val drupP = RupProof( drup.toVector :+ RupProof.Rup( goal ) )
        val replayed = ( drupP.lines.map( _.clause ) zip drupP.toResProofs ).reverse.toMap
        def toLK( clause: Set[Int] ): LKProof =
          replayed( clause ).toLK( atomToSh, cls => proofs( cls ) match {
            case Left( p ) => p
            case Right( ( upper, f ) ) =>
              WeakeningMacroRule( f( toLK( upper ) ), implication( cls ), strict = false )
          } )
        val lk = toLK( goal )
        Right( lk )
    }
}

object ExpansionProofToMG3iViaSAT {
  def apply( f: Formula ): Either[( Unit, HOLSequent ), LKProof] =
    apply( Sequent() :+ f )

  def apply( seq: HOLSequent ): Either[( Unit, HOLSequent ), LKProof] =
    apply( ExpansionProof( formulaToExpansionTree( seq ) ) )

  def apply( exp: ExpansionProof ): Either[( Unit, HOLSequent ), LKProof] =
    new ExpansionProofToMG3iViaSAT( exp ).solve().left.map( () -> _ )
}
