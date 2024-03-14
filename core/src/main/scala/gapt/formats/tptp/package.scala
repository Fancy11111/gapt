package gapt.formats

import gapt.expr._
import gapt.expr.formula.Atom
import gapt.expr.formula.Eq
import gapt.expr.formula.Formula
import gapt.expr.formula.hol.existentialClosure
import gapt.expr.ty.FunctionType
import gapt.expr.ty.Ti
import gapt.expr.ty.To
import gapt.proofs._
import gapt.expr.ty.Ty

package object tptp {

  type GeneralTerm = Expr
  type FormulaRole = String
  type InfoItem = GeneralTerm

  case class TptpFile( inputs: Seq[TptpInput] ) {
    override def toString = inputs.mkString

    def toSequent = existentialClosure( inputs.flatMapS {
      case AnnotatedFormula( _, _, "conjecture", formula, _ ) =>
        Sequent() :+ formula
      case AnnotatedFormula( _, _, _, formula, _ ) =>
        formula +: Sequent()
      case TypeDef( _, _, _, _, _ ) =>
        Sequent()
      case ConstDef( _, _, _, _, _ ) =>
        Sequent()
      case in => throw new IllegalArgumentException( in.toString )
    } )

    def toSequentWithIncludes = {
      val sequent = existentialClosure( inputs.flatMapS {
        case AnnotatedFormula( _, _, "conjecture", formula, _ ) =>
          Sequent() :+ formula
        case AnnotatedFormula( _, _, _, formula, _ ) =>
          formula +: Sequent()
        case IncludeDirective( _, _ ) =>
          Sequent()
        case TypeDef( _, _, _, _, _ ) =>
          Sequent()
        case ConstDef( _, _, _, _, _ ) =>
          Sequent()
      } )
      val names = inputs.collect( {
        case IncludeDirective( name, _ ) => name
      } )

      ( names, sequent )

    }

  }
  sealed trait TptpInput {
    override def toString = TptpToString.tptpInput( this )
  }
  case class AnnotatedFormula( language: String, name: String, role: FormulaRole, formula: Formula, annotations: Seq[GeneralTerm] ) extends TptpInput
  case class IncludeDirective( fileName: String, formulaSelection: Option[Seq[String]] ) extends TptpInput
  case class TypeDef( lang: String, name: String, tyName: String, ty: Ty, annotations: Seq[GeneralTerm] ) extends TptpInput
  case class ConstDef( lang: String, name: String, varName: String, v: Var, annotations: Seq[GeneralTerm] ) extends TptpInput

  object TptpTerm {
    def apply( sym: String, args: Seq[Expr] ): Expr =
      TptpTerm( sym, args, Ti )
    def apply( sym: String, args: Seq[Expr], out: Ty ): Expr =
      Apps( Const( sym, FunctionType( out, args.map( _.ty ) ) ), args ) // TODO: add optional ctx lookup
    def apply( sym: String, args: Expr* )( implicit dummyImplicit: DummyImplicit ): Expr =
      TptpTerm( sym, args )
    def apply( sym: String, args: Seq[Ctx => Expr], ctx: Ctx ): Expr =
      // TODO: correct exception
      Apps( Const( sym, ctx.vars.get( sym ).map( v => v.ty ).getOrElse( throw new RuntimeException() ) ), ctx( args ) )
    def unapplySeq( expr: Expr ): Option[( String, Seq[Expr] )] = expr match {
      case Apps( Const( sym, _, _ ), args ) => Some( ( sym, args ) )
      case _                                => None
    }
  }
  def TptpAtom( sym: String, args: Seq[Expr] ): Atom =
    ( sym, args ) match {
      case ( "equal", Seq( a, b ) ) => Eq( a, b ) // old tptp syntax
      case _                        => Apps( Const( sym, FunctionType( To, args.map( _.ty ) ) ), args ).asInstanceOf[Atom]
    }

  def TptpAtom( sym: String, args: Seq[Expr], ctx: Ctx ): Atom = {

    ( sym, args ) match {
      case _ => Apps( Const( sym, ctx.vars.get( sym ).getOrElse( throw new RuntimeException( "var with name " + sym + " not found" ) ).ty ), args ).asInstanceOf[Atom]
    }
  }

  object GeneralList {
    val name = "$general_list"
    def apply( elems: Seq[GeneralTerm] ): Expr = TptpTerm( name, elems )
    def apply( elems: GeneralTerm* )( implicit dummyImplicit: DummyImplicit ): Expr = TptpTerm( name, elems )
    def unapplySeq( expr: Expr ): Option[Seq[Expr]] = expr match {
      case Apps( Const( `name`, _, _ ), elems ) => Some( elems )
      case _                                    => None
    }
  }
  object GeneralColon {
    val name = "$general_colon"
    def apply( a: GeneralTerm, b: GeneralTerm ): Expr = TptpTerm( name, a, b )
    def unapplySeq( expr: Expr ): Option[Seq[Expr]] = expr match {
      case Apps( Const( `name`, _, _ ), elems ) => Some( elems )
      case _                                    => None
    }
  }

  /**
   * The roles of valid formula assertions in a TPTP file.
   * @see http://tptp.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html#formula_role
   */
  object TptpFormulaRoles {
    val roles: Set[FormulaRole] = Set( "axiom", "hypothesis", "definition", "assumption", "lemma",
      "theorem", "corollary", "conjecture", "negated_conjecture", "plain" )

    def apply() = roles
  }

}
