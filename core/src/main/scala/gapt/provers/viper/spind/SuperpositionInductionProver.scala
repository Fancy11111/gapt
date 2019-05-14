package gapt.provers.viper.spind

import gapt.expr._
import gapt.expr.formula._
import gapt.expr.util.LambdaPosition
import gapt.logic.hol.skolemize
import gapt.proofs.context.Context
import gapt.proofs.lk.LKProof
import gapt.proofs.{ HOLSequent, Sequent, withSection }
import gapt.proofs.context.mutable.MutableContext
import gapt.proofs.lk.rules.CutRule
import gapt.proofs.lk.rules.macros.WeakeningContractionMacroRule
import gapt.proofs.resolution.{ ResolutionToLKProof, mapInputClauses, structuralCNF }
import gapt.provers.escargot.Escargot
import gapt.provers.viper.aip.axioms.{ Axiom, SequentialInductionAxioms, StandardInductionAxioms }
import gapt.provers.viper.grammars.enumerateTerms

object SuperpositionInductionProver extends SuperpositionInductionProver

class SuperpositionInductionProver {

  private implicit def labeledSequentToHOLSequent( sequent: Sequent[( String, Formula )] ): Sequent[Formula] =
    sequent map { case ( _, f ) => f }

  var allPositions: Map[Const, Positions] = Map()

  /**
   * Proves the given sequent by using induction.
   *
   * @param sequent The sequent to be proven.
   * @param ctx Defines the constants, types, etc.
   * @return An inductive proof the sequent is provable with the prover's induction schemes, otherwise None or
   *         the method does not terminate.
   */
  def inductiveLKProof( sequent: Sequent[( String, Formula )] )( implicit ctx: MutableContext ): Option[LKProof] = {
    val seq = labeledSequentToHOLSequent( sequent )

    allPositions = Positions.splitRules( ctx.normalizer.rules )

    withSection { section =>
      val ground = section.groundSequent( seq )

      // Perform an initial induction while the goal has not been split across several clauses
      val goals = ground.succedent
      val goalAxioms = goals flatMap ( goal => clauseAxioms( skolemize( goal ) +: Sequent() ) )
      val goalGround = goalAxioms.map( _.formula ) ++: ground

      val cnf = structuralCNF( goalGround )( ctx )
      val cnfMap = cnf.view.map( p => p.conclusion -> p ).toMap

      val clauses = cnfMap.keySet.map( _.map( _.asInstanceOf[Atom] ) )

      val prf = Escargot.getResolutionProofWithAxioms( clauses )

      prf map {
        case ( resolution, prfAxioms, indMap ) =>
          val axioms = goalAxioms ++ prfAxioms
          val res = mapInputClauses( resolution )( cnfMap ++ indMap )
          val lk = ResolutionToLKProof( res )
          val wlk = WeakeningContractionMacroRule( lk, axioms.map( _.formula ) ++: seq )
          val cut = cutAxioms( wlk, axioms )
          cut
      }
    }
  }

  /**
   * Cuts the specified axioms from the proof.
   *
   * @param proof The proof from which some axioms are to be cut. The end-sequent of this proof must
   *              contain the given axioms.
   * @param axioms The axioms to be cut out of the proof.
   * @return A proof whose end-sequent does not contain the specified axioms.
   */
  private def cutAxioms( proof: LKProof, axioms: Seq[Axiom] ): LKProof =
    axioms.foldRight( proof ) { ( axiom, mainProof ) =>
      if ( mainProof.conclusion.antecedent contains axiom.formula )
        CutRule( axiom.proof, mainProof, axiom.formula )
      else
        mainProof
    }

  // Replaces x with e in f.
  def replaceExpr( f: Expr, x: Expr, e: Expr ): Expr =
    f.find( x ).foldLeft( f )( ( f, pos ) => f.replace( pos, e ) )

  // Tests expr by substituting small concrete terms for vars and normalizing the resulting expression.
  def testFormula( expr: Expr, vars: List[Var] )( implicit ctx: Context ): Boolean = {
    val numberOfTestTerms = 5 // TODO: should be an option

    def go( e: Expr, vars: List[Var] ): Seq[Expr] = {
      vars match {
        case List() => Seq( e )
        case v :: vs =>
          val termStream = enumerateTerms.forType( v.ty )( ctx )
          val terms = termStream filter ( _.ty == v.ty ) take numberOfTestTerms
          terms.flatMap( t => go( e, vs ) map ( replaceExpr( _, v, t ) ) )
      }
    }

    val fs = go( expr, vars )

    fs forall { f =>
      val nf = ctx.normalize( f )
      nf match {
        // NOTE: This may throw out formulas valid due to constraints in other clauses
        case Eq( lhs, rhs )         => lhs == rhs
        case Iff( lhs, rhs )        => lhs == rhs
        case Neg( Eq( lhs, rhs ) )  => lhs != rhs
        case Neg( Iff( lhs, rhs ) ) => lhs != rhs
        // NOTE: And this may include invalid formulas
        case Neg( lhs )             => lhs != Top()
        case lhs                    => lhs != Bottom()
      }
    }
  }

  // TODO: this is kinda heavy
  // Given an expression, returns a triple:
  // 1: A map from subexpressions that occur in primary positions to those positions.
  // 2: A map from subexpressions that occur in passive positions to those positions.
  // 3: A set of subexpressions that occur in primary positions together directly under the same symbol.
  //  The sets are transitive, so if a and b occur together and b and c occur together, 3 will contain a set
  //  containing all of a, b and c.
  def occurrences( formula: Expr ): ( Map[Expr, Seq[LambdaPosition]], Map[Expr, Seq[LambdaPosition]], Set[Set[Expr]] ) = {
    val empty = Seq.empty[( Expr, List[Int] )]

    def newPos( i: Int, size: Int, pos: List[Int] ): List[Int] =
      2 :: List.fill( size - i - 1 )( 1 ) ++ pos

    var underSame = Set.empty[Set[Expr]]

    def go( expr: Expr, pos: List[Int] ): ( Seq[( Expr, List[Int] )], Seq[( Expr, List[Int] )] ) =
      expr match {
        case Apps( c @ Const( _, _, _ ), rhsArgs ) =>
          allPositions.get( c ) match {
            case None =>
              rhsArgs.zipWithIndex.foldLeft( ( empty, empty ) ) {
                case ( ( prim, pass ), ( e, i ) ) =>
                  val ( l, r ) = go( e, newPos( i, rhsArgs.size, pos ) )
                  ( l ++ prim, r ++ pass )
              }
            case Some( positions ) =>
              val pass1 = positions.passiveArgs.toSeq flatMap { i =>
                val p = newPos( i, rhsArgs.size, pos )
                val ( l, r ) = go( rhsArgs( i ), p )
                ( rhsArgs( i ), p ) +: ( l ++ r )
              }

              val same = positions.primaryArgs map rhsArgs
              underSame.find( _.intersect( same ).nonEmpty ) match {
                case None => underSame += same
                case Some( existing ) =>
                  underSame -= existing
                  underSame += existing union same
              }

              val ( prim1, pass2 ) = positions.primaryArgs.toSeq.foldLeft( ( empty, empty ) ) {
                case ( ( prim, pass ), i ) =>
                  val p = newPos( i, rhsArgs.size, pos )
                  val ( l, r ) = go( rhsArgs( i ), p )
                  ( ( rhsArgs( i ), p ) +: ( l ++ prim ), r ++ pass )
              }
              ( prim1, pass1 ++ pass2 )
          }
        case App( a, b ) =>
          val ( l1, r1 ) = go( a, 1 :: pos )
          val ( l2, r2 ) = go( b, 2 :: pos )
          ( l1 ++ l2, r1 ++ r2 )
        case _ => ( Seq(), Seq() )
      }

    val ( prim, pass ) = go( formula, List() )

    val primMap = prim.groupBy( _._1 ).mapValues( seq => seq.map { case ( _, pos ) => LambdaPosition( pos.reverse ) } )
    val passMap = pass.groupBy( _._1 ).mapValues( seq => seq.map { case ( _, pos ) => LambdaPosition( pos.reverse ) } )

    ( primMap, passMap, underSame )
  }

  def negate( f: Formula ): Formula = f match {
    case Neg( g ) => g
    case _        => Neg( f )
  }

  // Is c an inductive skolem constant, i.e. not a constructor
  def isInductive( c: Const )( implicit ctx: Context ): Boolean =
    ctx.getConstructors( c.ty ) match {
      case None            => false
      case Some( constrs ) => !constrs.contains( c )
    }

  def asInductiveConst( e: Expr )( implicit ctx: Context ): Option[Const] =
    e match {
      case c @ Const( _, _, _ ) if isInductive( c ) => Some( c )
      case _                                        => None
    }

  def clauseAxioms( cls: HOLSequent )( implicit ctx: MutableContext ): Seq[Axiom] = {
    val nameGen = ctx.newNameGenerator // TODO: is this a problem?

    val f = negate( cls.toFormula ).asInstanceOf[Expr]
    val ( primMap, passMap, underSame ) = occurrences( f )

    // TODO: we should do this for non-constructor headed subterms as well as constants
    val underSameConsts = underSame map ( _.flatMap( asInductiveConst( _ )( ctx ) ) )

    // Given a constant c to induct over in f, returns a fresh induction variable
    // and a prioritized list of induction goals, the first more general than the next.
    // TODO: CLEAN up this confusing passing of f which must coincide with f above except for substitutions
    def getTargets( c: Const, f: Expr ): ( Var, Seq[Expr] ) = {
      val primPoses = primMap.getOrElse( c, Seq() )
      val passPoses = passMap.getOrElse( c, Seq() )
      val v = Var( nameGen.fresh( "ind" ), c.ty )

      val targets = if ( primPoses.size >= 2 && passPoses.nonEmpty ) {
        // Induct only on primary occurences, i.e. generalize
        Seq(
          primPoses.foldLeft( f )( ( g, pos ) => g.replace( pos, v ) ),
          replaceExpr( f, c, v ) )
      } else {
        Seq( replaceExpr( f, c, v ) )
      }

      ( v, targets )
    }

    underSameConsts.toSeq.flatMap {
      case cs if cs.isEmpty => Seq()
      case cs if cs.size == 1 =>
        // This constant only appears alone in primary position, so we do a regular induction on it.
        val c = cs.head
        val ( v, targets ) = getTargets( c, f )
        targets.find( testFormula( _, List( v ) )( ctx ) ) flatMap { target =>
          StandardInductionAxioms( v, target.asInstanceOf[Formula] )( ctx ).toOption.map( Seq( _ ) )
        }
      case cs =>
        // These constants appear together so we need to induct on all of them together for the definitions to reduce.
        // For each of them, we might need to generalize passive occurrences, so we calculate a sequence of less and
        // less general formulas to be tested.
        val targets = cs.foldLeft( Seq( ( Seq.empty[Var], f ) ) ) {
          case ( vsfs, c ) =>
            vsfs.flatMap {
              case ( vs, g ) =>
                val ( v, ts ) = getTargets( c, g )
                ts map ( ( v +: vs, _ ) )
            }
        }

        targets.find { case ( vs, target ) => testFormula( target, vs.toList )( ctx ) } flatMap {
          case ( vs, target ) =>
            SequentialInductionAxioms()( Sequent() :+ ( "axiom", target.asInstanceOf[Formula] ) )( ctx ) toOption
        }
    } flatten
  }

  // Inductive axioms that will prove the negation of one of the clauses in clausesForInduction.
  def inductiveAxioms( clausesForInduction: List[HOLSequent] )( implicit ctx: MutableContext ): List[Axiom] = {
    val axioms = clausesForInduction flatMap { cls =>
      clauseAxioms( cls )
    }

    axioms.reverse
  }

}

