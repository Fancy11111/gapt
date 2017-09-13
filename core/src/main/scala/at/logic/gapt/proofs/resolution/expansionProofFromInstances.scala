package at.logic.gapt.proofs.resolution

import at.logic.gapt.expr._
import at.logic.gapt.proofs._
import at.logic.gapt.proofs.expansion._

object expansionProofFromInstances {
  def apply[S <: Substitution](
    substitutions:          Map[HOLClause, Set[S]],
    justifications:         Set[ResolutionProof],
    pureFOLwithoutEquality: Boolean                = false ): ExpansionProof = {
    require( substitutions.keySet.toSet[HOLSequent] subsetOf justifications.map( _.conclusion ) )

    val proofs = for {
      ( clause, substs ) <- substitutions
      just <- justifications
      if just.conclusion == clause
      subst <- substs
    } yield Subst( just, subst )

    val expansionWithDefs = ExpansionProof( ETMerge( proofs.
      flatMapS( ResolutionToExpansionProof.withDefs(
        _,
        ResolutionToExpansionProof.inputsAsExpansionSequent, addConclusion = false ).
        expansionSequent ) ) )

    eliminateCutsET( eliminateDefsET( eliminateCutsET( eliminateMerges( expansionWithDefs ) ), pureFOLwithoutEquality ) )
  }
}