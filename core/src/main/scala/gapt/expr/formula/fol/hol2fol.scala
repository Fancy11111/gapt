package gapt.expr.formula.fol

import gapt.expr._
import gapt.expr.formula.All
import gapt.expr.formula.And
import gapt.expr.formula.Atom
import gapt.expr.formula.Ex
import gapt.expr.formula.Formula
import gapt.expr.formula.Imp
import gapt.expr.formula.Neg
import gapt.expr.formula.Or
import gapt.expr.formula.constants.AndC
import gapt.expr.formula.constants.ExistsC
import gapt.expr.formula.constants.ForallC
import gapt.expr.formula.constants.ImpC
import gapt.expr.formula.constants.NegC
import gapt.expr.formula.constants.OrC
import gapt.expr.formula.fol
import gapt.expr.formula.hol.HOLFunction
import gapt.expr.formula.hol._
import gapt.expr.ty.FunctionType
import gapt.expr.ty.To
import gapt.expr.ty.Ty
import gapt.expr.util.freeVariables
import gapt.proofs.HOLSequent

class Counter {
  private var state = 0
  def nextId(): Int = {
    state = state + 1
    state
  }
}

object reduceHolToFol extends reduceHolToFol
/**
 * Creates a FOL formula from a HOL formula, but applies transformations which do _not_ preserve validity!
 * Transformations applied:
 *
 *  - Replace all subterms (\x.t[v]) by a function q_n(v). The scope parameter is needed to pass existing term-constant
 *    mappings.
 *  - Change the type of constants and variables s.t. they are first order (i.e. Const("c", To->Ti) is mapped to
 *    FOLConst("c",Ti)
 *  - Logical operators inside the term structure are replaced by first order terms
 *
 * @note Make sure you need all of these tricks. To only replace abstraction subterms, use [[replaceAbstractions]].
 *
 */
class reduceHolToFol {

  private def folexp2term( exp: FOLExpression ) = exp match {
    case e: FOLTerm => exp.asInstanceOf[FOLTerm]
    case _          => throw new Exception( "Cannot cast " + exp + " to a fol term!" + exp.getClass )
  }

  /**
   * Convenience method when only a single expression is converted. Multiple expressions need to pass a scope which
   * holds the replacements which happened so far.
   * @param term a HOL expression to convert
   * @return the reduced FOL expression
   */
  def apply( term: Expr ): FOLExpression = {
    val counter = new Counter
    val emptymap = Map[Expr, String]()
    apply( term, emptymap, counter )._1
  }

  /**
   * Convenience method when only a single formula is converted. Multiple expressions need to pass a scope which
   * holds the replacements which happened so far.
   * @param formula a HOL formula to convert
   * @return the reduced FOL formula
   */
  def apply( formula: Formula ): FOLFormula =
    //inner cast needed to call the correct apply method
    reduceHolToFol( formula.asInstanceOf[Expr] ).asInstanceOf[FOLFormula]

  /**
   * Convenience method when only a single fsequent is converted. Multiple expressions need to pass a scope which
   * holds the replacements which happened so far.
   * @param fs an fsequent to convert
   * @return the reduced fsequent
   */
  def apply( fs: HOLSequent ): HOLSequent = {
    val counter = new Counter
    val emptymap = Map[Expr, String]()
    apply( fs, emptymap, counter )._1
  }

  /**
   * Convenience method when a single  list of fsequents is converted. Multiple expressions need to pass a scope which
   * holds the replacements which happened so far.
   * @param fs an fsequent to convert
   * @return the reduced fsequent
   */
  def apply( fs: List[HOLSequent] ): List[HOLSequent] = {
    val counter = new Counter
    val emptymap = Map[Expr, String]()
    apply( fs, emptymap, counter )._1
  }

  /**
   * Apply method for a formula when scope needs to passed on in a recursion.
   * @param formula the formula to convert
   * @param scope a mapping of replaced subterms to the constant names which replaced them. you need this for
   *              chained applications, like sequents or lists of formulas.
   * @param id an object with a function which nextId, which provides new numbers.
   * @return a pair of the reduced formula and the updated scope
   */
  def apply( formula: Formula, scope: Map[Expr, String], id: Counter ): ( FOLFormula, Map[Expr, String] ) = {
    val ( scope_, qterm ) = replaceAbstractions( formula, scope, id )
    ( apply_( qterm ).asInstanceOf[FOLFormula], scope_ )
  }

  /**
   * Apply method for a an expression when scope needs to passed on in a recursion.
   * @param term the expression to convert
   * @param scope a mapping of replaced subterms to the constant names which replaced them. you need this for
   *              chained applications, like sequents or lists of formulas.
   * @param id an object with a function which nextId, which provides new numbers.
   * @return a pair of the reduced expression and the updated scope
   */
  def apply( term: Expr, scope: Map[Expr, String], id: Counter ): ( FOLExpression, fol.replaceAbstractions.ConstantsMap ) = {
    val ( scope_, qterm ) = replaceAbstractions( term, scope, id )
    ( apply_( qterm ), scope_ )
  }

  /**
   * Apply method for a an FSequent when scope needs to passed on in a recursion.
   * @param s the fsequent to convert
   * @param scope a mapping of replaced subterms to the constant names which replaced them. you need this for
   *              chained applications, like sequents or lists of formulas.
   * @param id an object with a function which nextId, which provides new numbers.
   * @return a pair of the reduced expression and the updated scope
   */
  def apply( s: HOLSequent, scope: Map[Expr, String], id: Counter ): ( HOLSequent, Map[Expr, String] ) = {
    val ( scope1, ant ) = s.antecedent.foldLeft( ( scope, List[Formula]() ) )( ( r, formula ) => {
      val ( scope_, f_ ) = replaceAbstractions( formula, r._1, id )
      ( scope_, f_.asInstanceOf[Formula] :: r._2 )
    } )
    val ( scope2, succ ) = s.succedent.foldLeft( ( scope1, List[Formula]() ) )( ( r, formula ) => {
      val ( scope_, f_ ) = replaceAbstractions( formula, r._1, id )
      ( scope_, f_.asInstanceOf[Formula] :: r._2 )
    } )

    ( HOLSequent( ant.reverse map apply_, succ.reverse map apply_ ), scope ++ scope2 )
  }

  /**
   * Apply method for a an FSequent when scope needs to passed on in a recursion.
   * @param fss the fsequent to convert
   * @param scope a mapping of replaced subterms to the constant names which replaced them. you need this for
   *              chained applications, like sequents or lists of formulas.
   * @param id an object with a function which nextId, which provides new numbers.
   * @return a pair of the reduced expression and the updated scope
   */
  def apply( fss: List[HOLSequent], scope: Map[Expr, String], id: Counter ): ( List[HOLSequent], Map[Expr, String] ) = {
    fss.foldRight( ( List[HOLSequent](), scope ) )( ( fs, pair ) => {
      val ( list, scope ) = pair
      val ( fs_, scope_ ) = apply( fs, scope, id )
      ( fs_ :: list, scope_ )
    } )

  }

  private def apply_( f: Formula ): FOLFormula =
    apply_( f.asInstanceOf[Expr] ).asInstanceOf[FOLFormula]

  //assumes we are on the logical level of the hol formula - all types are mapped to i, i>o or i>i>o respectively
  private def apply_( term: Expr ): FOLExpression = {
    term match {
      case e: FOLExpression  => e // if it's already FOL - great, we are done.
      case Const( n, To, _ ) => FOLAtom( n, Nil )
      case Var( n, _ )       => FOLVar( n )
      case Const( n, _, _ )  => FOLConst( n )
      case Neg( n )          => Neg( apply_( n ) )
      case And( n1, n2 )     => And( apply_( n1 ), apply_( n2 ) )
      case Or( n1, n2 )      => Or( apply_( n1 ), apply_( n2 ) )
      case Imp( n1, n2 )     => Imp( apply_( n1 ), apply_( n2 ) )
      case All( v: Var, n )  => All( apply_( v ).asInstanceOf[FOLVar], apply_( n ) )
      case Ex( v: Var, n )   => Ex( apply_( v ).asInstanceOf[FOLVar], apply_( n ) )
      case Atom( Const( n, _, _ ), ls ) =>
        FOLAtom( n, ls.map( x => folexp2term( apply_termlevel( x ) ) ) )
      case Atom( Var( n, _ ), ls ) =>
        FOLAtom( n, ls.map( x => folexp2term( apply_termlevel( x ) ) ) )
      case HOLFunction( Const( n, _, _ ), ls ) =>
        FOLFunction( n, ls.map( x => folexp2term( apply_( x ) ) ) )
      case HOLFunction( Var( n, _ ), ls ) =>
        FOLFunction( n, ls.map( x => folexp2term( apply_( x ) ) ) )
      case _ => throw new IllegalArgumentException(
        // for cases of higher order atoms and functions
        "Cannot reduce hol term: " + term.toString + " to fol as it is a higher order variable function or atom" )
    }
  }

  //if we encountered an atom, we need to convert logical formulas to the term level too
  private def apply_termlevel( term: Expr ): FOLTerm = {
    term match {
      case e: FOLTerm       => e // if it's already FOL - great, we are done.
      case Var( n, _ )      => FOLVar( n )
      case Const( n, _, _ ) => FOLConst( n )
      //we cannot use the logical symbols directly because they are treated differently by the Function matcher
      case Neg( n )         => FOLFunction( NegC.name, List( apply_termlevel( n ) ) )
      case And( n1, n2 )    => FOLFunction( AndC.name, List( apply_termlevel( n1 ), apply_termlevel( n2 ) ) )
      case Or( n1, n2 )     => FOLFunction( OrC.name, List( apply_termlevel( n1 ), apply_termlevel( n2 ) ) )
      case Imp( n1, n2 )    => FOLFunction( ImpC.name, List( apply_termlevel( n1 ), apply_termlevel( n2 ) ) )
      case All( v: Var, n ) =>
        FOLFunction( ForallC.name, List( apply_termlevel( v ).asInstanceOf[FOLVar], apply_termlevel( n ) ) )
      case Ex( v: Var, n ) =>
        FOLFunction( ExistsC.name, List( apply_termlevel( v ).asInstanceOf[FOLVar], apply_termlevel( n ) ) )
      case Atom( head, ls ) =>
        FOLFunction( head.toString, ls.map( x => folexp2term( apply_termlevel( x ) ) ) )
      case HOLFunction( Const( name, _, _ ), ls ) =>
        FOLFunction( name, ls.map( x => folexp2term( apply_termlevel( x ) ) ) )

      // This case replaces an abstraction by a function term.
      //
      // the scope we choose for the variant is the Abs itself as we want all abs identical up to variant
      // use the same symbol
      //
      // TODO: at the moment, syntactic equality is used here... This means that alpha-equivalent terms may be replaced
      // by different constants, which is undesirable.
      /*
      case a @ Abs(v, exp) => {
        val sym = scope.getOrElseUpdate(a.variant(new VariantGenerator(
         new {var idd = 0; def nextId = {idd = idd+1; idd}}, "myVariantName")), ConstantString("q_{" + id.nextId + "}"))
        val freeVarList = a.getFreeVariables.toList.sortWith((x,y) => x.toString < y.toString)
        .map(x => apply(x.asInstanceOf[Expr],scope,id))
        if (freeVarList.isEmpty) FOLConst(sym) else Function(sym, freeVarList.asInstanceOf[List[FOLTerm]])
      }
      */
      case _ =>
        throw new IllegalArgumentException(
          // for cases of higher order atoms and functions
          "Cannot reduce hol term: " + term.toString + " to fol as it is a higher order variable function or atom" )
    }
  }

}

object replaceAbstractions extends replaceAbstractions

/**
 * Replace lambda-abstractions by constants.
 *
 * Each abstraction in an [[gapt.proofs.HOLSequent]] is replaced by a separate constant symbol; the used
 * constants are returned in a Map.
 */
class replaceAbstractions {
  type ConstantsMap = Map[Expr, String]

  def apply( l: List[HOLSequent] ): ( ConstantsMap, List[HOLSequent] ) = {
    val counter = new Counter
    l.foldLeft( ( Map[Expr, String](), List[HOLSequent]() ) )( ( rec, el ) => {
      val ( scope_, f ) = rec
      val ( nscope, rfs ) = replaceAbstractions( el, scope_, counter )
      ( nscope, rfs :: f )

    } )
  }

  def apply( f: HOLSequent, scope: ConstantsMap, id: Counter ): ( ConstantsMap, HOLSequent ) = {
    val ( scope1, ant ) = f.antecedent.foldLeft( ( scope, List[Formula]() ) )( ( rec, formula ) => {
      val ( scope_, f ) = rec
      val ( nscope, nformula ) = replaceAbstractions( formula, scope_, id )
      ( nscope, nformula.asInstanceOf[Formula] :: f )
    } )
    val ( scope2, succ ) = f.succedent.foldLeft( ( scope1, List[Formula]() ) )( ( rec, formula ) => {
      val ( scope_, f ) = rec
      val ( nscope, nformula ) = replaceAbstractions( formula, scope_, id )
      ( nscope, nformula.asInstanceOf[Formula] :: f )
    } )

    ( scope2, HOLSequent( ant.reverse, succ.reverse ) )
  }

  def apply( e: Expr ): Expr = {
    val counter = new Counter
    apply( e, Map[Expr, String](), counter )._2
  }

  def apply( formula: Formula ): Formula =
    apply( formula.asInstanceOf[Expr] ).asInstanceOf[Formula]

  // scope and id are used to give the same names for new functions and constants between different calls of this method
  def apply( e: Expr, scope: ConstantsMap, id: Counter ): ( ConstantsMap, Expr ) = e match {
    case Var( _, _ ) =>
      ( scope, e )
    case Const( _, _, _ ) =>
      ( scope, e )
    //quantifiers should be kept
    case All( x, f ) =>
      val ( scope_, e_ ) = replaceAbstractions( f, scope, id )
      ( scope_, All( x, e_.asInstanceOf[Formula] ) )
    case Ex( x, f ) =>
      val ( scope_, e_ ) = replaceAbstractions( f, scope, id )
      ( scope_, Ex( x, e_.asInstanceOf[Formula] ) )
    case App( s, t ) =>
      val ( scope1, s1 ) = replaceAbstractions( s, scope, id )
      val ( scope2, t1 ) = replaceAbstractions( t, scope1, id )
      ( scope2, App( s1, t1 ) )
    // This case replaces an abstraction by a function term.
    // the scope we choose for the variant is the Abs itself as we want all abs
    // identical up to variant use the same symbol
    case Abs( v, exp ) =>
      //systematically rename free variables for the index
      //TODO: check if variable renaming is really what we want
      val ( normalizeda, mapping ) = normalizeFreeVariables( e )
      //update scope with a new constant if neccessary
      val scope_ = if ( scope contains normalizeda ) scope else scope + ( ( normalizeda, "q_{" + id.nextId + "}" ) )
      val sym = scope_( normalizeda )
      val freeVarList = freeVariables( e ).toList.sortBy( _.toString ).asInstanceOf[List[Expr]]
      if ( freeVarList.isEmpty )
        ( scope_, Const( sym, e.ty ) )
      else {
        val c = Const( sym, FunctionType( e.ty, freeVarList.map( _.ty ) ) )
        ( scope_, HOLFunction( c, freeVarList ) )
      }
    case _ =>
      throw new Exception( "Unhandled case in abstraction replacement!" + e )

  }
}

object undoReplaceAbstractions extends undoReplaceAbstractions
/**
 * Replaces the constants introduced by [[replaceAbstractions]] with the original lambda-abstractions.
 */
class undoReplaceAbstractions {
  import gapt.expr.formula.fol.replaceAbstractions.ConstantsMap

  def apply( fs: HOLSequent, map: ConstantsMap ): HOLSequent = HOLSequent(
    fs.antecedent.map( apply( _, map ) ),
    fs.succedent.map( apply( _, map ) ) )
  def apply( f: Formula, map: ConstantsMap ): Formula = apply( f.asInstanceOf[Expr], map ).asInstanceOf[Formula]
  def apply( e: Expr, map: ConstantsMap ): Expr = {
    val stringsmap = map.map( x => ( x._2.toString, x._1 ) ) //inverting the map works because the symbols are unique
    HOLPosition.getPositions( e ).foldLeft( e )( ( exp, position ) =>
      //we check if the position is a constant with an abstraction symbol
      e( position ) match {
        case Const( name, _, _ ) if stringsmap.contains( name ) =>
          //if yes, we replace it by the original expression
          exp.replace( position, stringsmap( name ) )
        case _ => exp
      } )
  }
}

/**
 * Introducing abstractions and converting to fol changes more complex types to fol compatible ones. With changeTypeIn
 * you can change them back.
 */
object changeTypeIn {

  type TypeMap = Map[String, Ty]

  /**
   * Maps types of constants and variables to the given types.
   *
   * @param expression The expression in which the types are to be replaced.
   * @param typeMap Specifies the names of constants and variables whose type
   *                is to be replaced by the associated type.
   * @return An expression obtained from `expression` by replacing the types of leaf-occurrences of variables and
   *         constants by the given types. The types of inner constants and variables are changed according
   *         to the new types of their arguments.
   */
  def apply( expression: Expr, typeMap: TypeMap ): Expr = expression match {
    case v @ Var( n, _ ) =>
      typeMap.get( n ).map { Var( n, _ ) }.getOrElse( v )
    case c @ Const( n, _, _ ) =>
      typeMap.get( n ).map { Const( n, _ ) }.getOrElse( c )
    case HOLFunction( Const( f, FunctionType( r, _ ), _ ), as ) =>
      val newAs = as.map { changeTypeIn( _, typeMap ) }
      val newTs = newAs.map { _.ty }
      HOLFunction( Const( f, FunctionType( r, newTs ) ), newAs )
    case HOLFunction( Var( x, FunctionType( r, _ ) ), as ) =>
      val newAs = as.map { changeTypeIn( _, typeMap ) }
      val newTs = newAs.map { _.ty }
      HOLFunction( Var( x, FunctionType( r, newTs ) ), newAs )
    case Atom( Const( f, FunctionType( r, _ ), _ ), as ) =>
      val newAs = as.map { changeTypeIn( _, typeMap ) }
      val newTs = newAs.map { _.ty }
      Atom( Const( f, FunctionType( r, newTs ) ), newAs )
    case Atom( Var( f, FunctionType( r, _ ) ), as ) =>
      val newAs = as.map { changeTypeIn( _, typeMap ) }
      val newTs = newAs.map { _.ty }
      Atom( Const( f, FunctionType( r, newTs ) ), newAs )
    case Neg( x ) =>
      Neg( changeTypeIn( x, typeMap ) )
    case And( s, t ) =>
      And( changeTypeIn( s, typeMap ), changeTypeIn( t, typeMap ) )
    case Or( s, t ) =>
      Or( changeTypeIn( s, typeMap ), changeTypeIn( t, typeMap ) )
    case Imp( s, t ) =>
      Imp( changeTypeIn( s, typeMap ), changeTypeIn( t, typeMap ) )
    case All( x, f ) =>
      val newX = typeMap.get( x.name ).map { Var( x.name, _ ) }.getOrElse( x )
      All( newX, changeTypeIn( f, typeMap ) )
    case Ex( x, f ) =>
      val newX = typeMap.get( x.name ).map { Var( x.name, _ ) }.getOrElse( x )
      Ex( newX, changeTypeIn( f, typeMap ) )
    case Abs( x, t ) =>
      val newX = typeMap.get( x.name ).map { Var( x.name, _ ) }.getOrElse( x )
      Abs( newX, changeTypeIn( t, typeMap ) )
    case App( s, t ) =>
      App( changeTypeIn( s, typeMap ), changeTypeIn( t, typeMap ) )
    case _ =>
      throw new Exception( "Unhandled case of a HOL Formula! " + expression )
  }

  /**
   * @see `changeTypeIn.apply( Expr, TypeMap )`.
   */
  def apply( e: Formula, tmap: TypeMap ): Formula =
    changeTypeIn( e.asInstanceOf[Expr], tmap ).asInstanceOf[Formula]

  def apply( fs: HOLSequent, tmap: TypeMap ): HOLSequent = HOLSequent(
    fs.antecedent.map( x => changeTypeIn( x, tmap ) ),
    fs.succedent.map( x => changeTypeIn( x, tmap ) ) )
}
