package at.logic.gapt.proofs.lk.reductions

import at.logic.gapt.expr.isConstructorForm
import at.logic.gapt.proofs.{ Context, SequentConnector, guessPermutation }
import at.logic.gapt.proofs.lk.{ InductionRule, LKProof, Reduction, unfoldInduction }

object inductionUnfoldingReduction extends Reduction {

  /**
   * Tries to apply the reduction.
   *
   * @param induction See `inductionUnfoldingReduction$.apply(induction:InductionRule)(ctx:Context):Option[LKProof]`
   * @param ctx Defines constants, types, etc.
   * @return If the induction rule could be unfolded a proof of the same end-sequent and a sequent connector
   *         is returned, otherwise None is returned.
   */
  def applyWithSequentConnector( induction: InductionRule )( implicit ctx: Context ): Option[( LKProof, SequentConnector )] =
    this( induction ) map { guessPermutation( induction, _ ) }

  /**
   * Tries to apply the induction unfolding reduction to a given inference.
   * @param proof The induction unfolding reduction is tried to applied to the last inference of this proof.
   * @param ctx Defines constants, types, etc.
   * @return None if the proof does not end with an induction inference, otherwise see
   *         `inductionUnfoldingReduction.apply(InductionRule)(Context): Option[LKProof]`.
   */
  def apply( proof: LKProof )( implicit ctx: Context ): Option[LKProof] = proof match {
    case ind @ InductionRule( _, _, _ ) => apply( ind )
    case _: LKProof                     => None
  }

  /**
   * Tries to unfold an induction inference.
   *
   * @param induction The induction inference to be unfolded.
   * @param ctx Defines constants, types, etc.
   * @return If the given induction's term is in constructor form a proof of the same end-sequent for
   *         which the induction inference has been unfolded is returned, otherwise None.
   */
  def apply( induction: InductionRule )( implicit ctx: Context ): Option[LKProof] = {
    if ( isConstructorForm( induction.term ) ) {
      Some( unfoldInduction( induction ) )
    } else {
      None
    }
  }

  override def reduce( proof: LKProof )( implicit ctx: Context ): Option[LKProof] =
    proof match {
      case ind @ InductionRule( _, _, _ ) => apply( ind )
      case _                              => None
    }
}
