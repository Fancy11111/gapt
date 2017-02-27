package at.logic.gapt.provers.viper.aip.axioms

import at.logic.gapt.expr.{ All, HOLFormula, Var, freeVariables }
import at.logic.gapt.proofs.gaptic._
import at.logic.gapt.proofs.{ Context, Sequent }
import at.logic.gapt.provers.viper.aip._
import cats.instances.all._
import cats.syntax.all._

/**
 * Generates independent induction axioms.
 *
 * @param vsel The variables for which induction axioms are generated.
 * @param fsel The formula of a sequent for which axioms are generated.
 */
case class IndependentInductionAxioms(
    vsel: VariableSelector = allVariablesSelector( _ )( _ ),
    fsel: FormulaSelector  = firstFormulaSelector( _ )
) extends AxiomFactory {

  def forAllVariables = copy( vsel = allVariablesSelector( _ )( _ ) )

  def forVariables( variables: List[Var] ) = copy( vsel = ( _, _ ) => variables )

  def forVariables( variables: Var* ) = copy( vsel = ( _, _ ) => variables.toList )

  def forLabel( label: String ) = copy( fsel = findFormula( _, label ) )

  def forFormula( formula: HOLFormula ) = copy( fsel = _ => Right( formula ) )

  /**
   * Generates independent induction axioms for the given sequent.
   *
   * @param sequent The sequent for which the induction axioms are generated.
   * @return Either a list of induction axioms, or a list of error-messages if the axioms could not be created
   */
  override def apply( sequent: Sequent[( String, HOLFormula )] )( implicit ctx: Context ): ThrowsError[List[Axiom]] = {
    for {
      formula <- fsel( sequent )
      variables = vsel( formula, ctx )
      axioms <- variables.traverse[ThrowsError, Axiom] {
        variable => inductionAxiom( variables, variable, formula )
      }
    } yield axioms
  }

  /**
   * The quantifier induction form of a given formula and induction variables.
   *
   * @param inductionVariables Inductive variables x_1,...,x_n that may possibly occur in the given formula.
   * @param formula A formula of the form `∀Xφ`.
   * @return A formula of the form `∀Yφ` where Y does not contain any of x_1,...,x_n.
   */
  private def inductionQuantifierForm( inductionVariables: List[Var], formula: HOLFormula ) = {
    val All.Block( _, matrix ) = formula
    val quantifierPrefix = freeVariables( matrix ).diff( freeVariables( formula ) ).diff( inductionVariables toSet ) toSeq

    All.Block( quantifierPrefix, matrix )
  }

  /**
   * The independent induction axiom for the given variables, and formula.
   *
   * @param inductionVariables All the inductive variables that are considered for the induction. This list of
   *                           variables also contains the main induction variable `variable`.
   * @param variable The inductive variable on which the induction is carried out.
   * @param formula The formula on whose induction quantifier form the induction is carried out.
   * @param ctx Defines constants, types, etc.
   * @return An independent induction axiom.
   */
  private def inductionAxiom(
    inductionVariables: List[Var], variable: Var, formula: HOLFormula
  )( implicit ctx: Context ): ThrowsError[Axiom] = {
    val auxiliaryVariables = inductionVariables filter { _ != variable }
    val inductionFormula = inductionQuantifierForm( inductionVariables, formula )
    StandardInductionAxioms( variable, inductionFormula ) map { axiom =>
      new Axiom {
        val formula = All.Block( auxiliaryVariables, axiom.formula )
        def proof = {
          ProofState( Sequent() :+ formula ) + repeat( allR ) + insert( axiom.proof ) result
        }
      }
    }
  }
}