package gapt.formats.tptp

import gapt.expr._
import gapt.formats.InputFile
import org.parboiled2._
import ammonite.ops._
import gapt.expr
import gapt.expr.formula.And
import gapt.expr.formula.Atom
import gapt.expr.formula.Bottom
import gapt.expr.formula.Eq
import gapt.logic.fol.arithmetic.GreaterEq
import gapt.expr.formula.Ex
import gapt.expr.formula.Formula
import gapt.expr.formula.Imp
import gapt.expr.formula.Neg
import gapt.expr.formula.Or
import gapt.expr.formula.QuantifierHelper
import gapt.expr.formula.Top
import gapt.expr.formula.fol.FOLAtom
import gapt.expr.formula.fol.FOLConst
import gapt.expr.formula.fol.FOLVar
import gapt.expr.ty.FunctionType
import gapt.expr.ty.TBase
import gapt.expr.ty.Ti
import gapt.expr.ty.To
import gapt.expr.ty.Ty
import gapt.expr.ty.TVar
import gapt.expr.ty.TArr
import gapt.expr.ty.->:

import scala.util.{ Failure, Success }
import gapt.formats.tptp.GeneralTerm
import org.parboiled2.support.hlist
import org.parboiled2.support.hlist.HNil
import gapt.formats.tptp.statistics.ParsingError
import gapt.formats.tptp.TptpFile
import gapt.logic.fol.arithmetic.TReal
import gapt.logic.fol.arithmetic.TRat
import gapt.logic.fol.arithmetic.TInt
import gapt.logic.fol.arithmetic.Lesser
import gapt.logic.fol.arithmetic.Greater
import gapt.logic.fol.arithmetic.LesserEq

class Sig( val vars: Map[String, Var], val types: Map[String, Ty] ) {
  private var nextTyVar: Int = 0

  def getNextTyVar: TVar = {
    nextTyVar = nextTyVar + 1
    TVar( "" + nextTyVar )
  }

  def apply[A]( to: ( Sig => A ) ): A = {
    to( this )
  }

  def apply[A]( to: Seq[( Sig => A )] ): Seq[A] = {
    to.map( _( this ) )
  }

  def apply[A]( to: Option[( Sig => A )] ): Option[A] = {
    to.map( this( _ ) )
  }

  def >=[A]( to: ( Sig => A ) ): A = {
    to( this )
  }
}

class TptpNotYetImplementedException( val explanation: String )
  extends Exception( s"Feature of TPTP is not yet implemented: $explanation" )

object Sig {

  def mReturn[A]( value: A ): ( Sig => A ) = {
    ( sig ) => value
  }

  def unapply( sig: Sig ): Option[Tuple2[Map[String, Var], Map[String, Ty]]] = {
    Some( ( sig.vars, sig.types ) )
  }

  def apply( vars: Map[String, Var], types: Map[String, Ty] ): Sig = {
    new Sig( vars, types )
  }

  def apply( sig: Sig, name: String, v: Var ): Sig = {
    Sig( sig.vars + ( name -> v ), sig.types )
  }

  def apply( sig: Sig, addVars: Seq[Var] ): Sig = {
    val newSig = Sig( sig.vars ++ ( addVars map {
      case Var( name, ty ) => ( name, Var( name, ty ) )
    } ).toMap, sig.types )
    newSig.nextTyVar = sig.nextTyVar
    newSig
  }

  def apply( sig: Sig, name: String, t: Ty ): Sig = {
    val newSig = Sig( sig.vars, sig.types + ( name -> t ) )
    newSig.nextTyVar = sig.nextTyVar
    newSig
  }

  def apply(): Sig = {
    new Sig( Map.empty[String, Var], Map.empty[String, Ty] )
  }

  val default = new Sig(
    Map(),
    Map() )

}

// object TNum extends TBase("$num", List())

class TptpParser( val input: ParserInput ) extends Parser {
  import CharPredicate._

  type SigTo[A] = Sig => A

  def partitionTypesAndVars( ty: Ty ): ( Seq[Ty], Seq[Ty] ) = {
    ty match {
      case TVar( name ) => ( Seq( ty ), Seq() )
      case TArr( from, to ) => {
        val ( vars, types ) = partitionTypesAndVars( from )
        val ( newVars, newTypes ) = partitionTypesAndVars( to )
        ( vars ++ newVars, types ++ newTypes )
      }
      case TBase( name, args ) => {
        // TODO: is this correct?
        ( Seq(), Seq( ty ) )
        // if ( args.isEmpty ) {
        //   ( Seq( ty ), Seq() )
        // } else {
        //   val ( vars, types ) = args.foldLeft( ( Seq[Ty](), Seq[Ty]() ) ) {
        //     case ( ( vars, types ), ty ) => {
        //       val ( newVars, newTypes ) = partitionTypesAndVars( ty )
        //       ( vars ++ newVars, types ++ newTypes )
        //     }
        //   }
        //   if ( vars.isEmpty ) {
        //     ( Seq(), Seq( ty ) )
        //   } else {
        //     ( vars, types )
        //   }
        // }
      }
    }
  }

  def extractVars( ty: Ty ): Seq[TVar] = {
    ty match {
      case tVar @ TVar( name ) => Seq( tVar )
      case TArr( from, to )    => extractVars( from ) ++ extractVars( to )
      case TBase( name, args ) => Seq( args.map( _.asInstanceOf[TVar] ): _* )
    }
  }

  private def Ws = rule {
    quiet( zeroOrMore( anyOf( " \t \n" ) |
      ( '%' ~ zeroOrMore( noneOf( "\n" ) ) ) |
      ( "/*" ~ not_star_slash ~ oneOrMore( "*" ) ~ "/" ) ) )
  }
  private def not_star_slash = rule { ( noneOf( "*" ).* ~ oneOrMore( "*" ) ~ noneOf( "/*" ) ).* ~ noneOf( "*" ).* }
  private def Comma = rule { "," ~ Ws }
  private def Colon = rule { ":" ~ Ws }

  // def TPTP_file: Rule1[SigTo[TptpFile]] = rule { Ws ~ TPTP_input.* ~ EOI ~> ( ( seq: Seq[SigTo[TptpInput]] ) => ( sig: Sig ) => ( TptpFile( seq.map( _( sig ) ) ) ) ) }
  def TPTP_file: Rule1[SigTo[TptpFile]] = rule {
    Ws ~ TPTP_input.* ~ EOI ~> ( ( seq: Seq[SigTo[TptpInput]] ) => ( sig: Sig ) =>
      seq.foldLeft( ( sig, Seq[TptpInput]() ) ) {
        ( acc, contextLookup ) =>
          val ( acc_sig, acc_inputs ) = acc
          val tptp_input = contextLookup( acc_sig )
          tptp_input match {
            case TypeDef( _, _, name, ty, _ ) =>
              ( Sig( acc_sig, name, ty ), acc_inputs :+ tptp_input )
            case ConstDef( _, _, name, v, _ ) =>
              ( Sig( acc_sig, name, v ), acc_inputs :+ tptp_input )
            case other => // Formula or include directive
              ( acc_sig, acc_inputs :+ other )
          }

      } match {
        case ( _sig, formulas ) => TptpFile( formulas )
      } )
  }

  // private def TPTP_input = rule { typedef_formula | annotated_formula | include }
  // private def TPTP_input = rule { typedef_formula | atom_def_formula | tff_annotated_formula | annotated_formula | lift( include ) }
  private def TPTP_input = rule { atom_def_formula | tff_annotated_formula | annotated_formula | lift( include ) }

  private def annotated_formula: Rule1[SigTo[TptpInput]] = rule {

    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ ( formula_role ~ Comma ~ lift( formula ) ) ~ annotations ~ ")." ~ Ws ~>
      ( ( lang: String, name: String, role: String, form: SigTo[Formula], ann: Seq[SigTo[GeneralTerm]] ) => ( sig: Sig ) => ( AnnotatedFormula( lang, name, role, form( sig ), ann.map( _( sig ) ) ) ) )
  }

  //  tff(animal_type,type, animal: $tType ).

  //  TODO
  // def typedef_formula: Rule1[SigTo[TypeDef]] = rule {
  //   atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ Ws ~ "type" ~ Ws ~ Comma ~ Ws ~ atomic_word ~ Ws ~ ":" ~ Ws ~ "$tType" ~ Ws ~ annotations ~ ")." ~ Ws ~>
  //     ( ( lang: String, name: String, typeName: String, ann: Seq[SigTo[GeneralTerm]] ) => ( sig: Sig ) => ( TypeDef( lang, name, typeName, TBase( typeName ), ann.map( _( sig ) ) ) ) )
  // }

  private def atom_def_formula: Rule1[SigTo[TptpInput]] = rule {
    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ Ws ~ "type" ~ Ws ~ Comma ~ Ws ~ atomic_word ~ Ws ~ ":" ~ Ws ~ tff_top_level_type ~ annotations ~ ")." ~ Ws ~>
      ( ( lang: String, name: String, varName: String, ty: SigTo[Ty], ann: Seq[SigTo[GeneralTerm]] ) => ( sig: Sig ) => {
        val ( tyVars, tyTypes ) = partitionTypesAndVars( ty( sig ) )
        if ( tyVars.nonEmpty && tyTypes.isEmpty ) {
          // all type vars -> type def
          ty( sig ) match {
            case TArr( in, out ) => TypeDef( lang, name, varName, TBase( varName, in ), ann.map( _( sig ) ) )
            case TVar( _ )       => TypeDef( lang, name, varName, TBase( varName ), ann.map( _( sig ) ) )
            case _               => throw new MalformedInputFileException( "Illegal mix of types and type vars" )
          }
        } else if ( tyVars.isEmpty && tyTypes.nonEmpty ) {
          // all types, atom def
          ConstDef( lang, name, varName, Var( varName, ty( sig ) ), ann.map( _( sig ) ) )
        } else {
          // Should not happen, as this case is already checked in the type rules
          throw new MalformedInputFileException( "Illegal mix of types and type vars" )
        }
      } )
  }

  private def tff_annotated_formula: Rule1[SigTo[TptpInput]] = rule {
    "tff(" ~ Ws ~ name ~ Comma ~ ( formula_role ~ Comma ~ tff_logic_formula ) ~ annotations ~ ")." ~ Ws ~>
      ( ( name: String, role: String, form: SigTo[Formula], ann: Seq[SigTo[GeneralTerm]] ) => ( sig: Sig ) => ( AnnotatedFormula( "tff", name, role, form( sig ), ann.map( _( sig ) ) ) ) )
  }

  // TODO: maybe fix the list of possible roles to values defined in specs
  private def formula_role = rule { atomic_word }
  private def annotations = rule { ( Comma ~ general_term ).* }

  // TODO: implement (Either (tff_atom_typing, tff_subtype )
  // private def typedef: Rule1[Formula] = rule { ( Ws ~ lower_word ~ ":" ~ Ws ~ complex_type ) ~> ( ( a: Ty ) => Top() ) }
  // private def typedef: Rule1[Formula] = rule { (variable ~  ":" ~ Ws ~ name)  ~> ((b:String, a: FOLVar) => FOLAtom(a.name, a) ) }

  private def formula = rule { typed_logic_formula }
  private def typed_logic_formula = rule { logic_formula } //add type annotation
  private def logic_formula: Rule1[Formula] = rule { unitary_formula ~ ( binary_nonassoc_part | or_formula_part | and_formula_part ).? }
  private def binary_nonassoc_part = rule { binary_connective ~ unitary_formula ~> ( ( a: Formula, c: ( Expr, Expr ) => Formula, b: Formula ) => c( a, b ) ) }
  private def or_formula_part = rule { ( "|" ~ Ws ~ unitary_formula ).+ ~> ( ( a: Formula, as: Seq[Formula] ) => Or.leftAssociative( a +: as: _* ) ) }
  private def and_formula_part = rule { ( "&" ~ Ws ~ unitary_formula ).+ ~> ( ( a: Formula, as: Seq[Formula] ) => And.leftAssociative( a +: as: _* ) ) }
  private def unitary_formula: Rule1[Formula] = rule { quantified_formula | unary_formula | atomic_formula | "(" ~ Ws ~ logic_formula ~ ")" ~ Ws }
  private def quantified_formula = rule { fol_quantifier ~ "[" ~ Ws ~ variable_list ~ "]" ~ Ws ~ ":" ~ Ws ~ unitary_formula ~> ( ( q: QuantifierHelper, vs, m ) => q.Block( vs, m ) ) }
  private def variable_list = rule { ( ( typed_variable | variable ) ~> ( ( a: Var ) => a ) ).+.separatedBy( Comma ) }
  private def unary_formula = rule { "~" ~ Ws ~ unitary_formula ~> ( Neg( _ ) ) }

  private def atomic_formula = rule { defined_prop | infix_formula | plain_atomic_formula | ( distinct_object ~> ( FOLAtom( _ ) ) ) }
  private def plain_atomic_formula = rule { atomic_word ~ ( "(" ~ Ws ~ arguments ~ ")" ~ Ws ).? ~> ( ( p, as ) => TptpAtom( p, as.getOrElse( Seq() ) ) ) }
  private def defined_prop = rule { "$" ~ Ws ~ ( "true" ~ push( Top() ) | "false" ~ push( Bottom() ) ) ~ Ws }
  private def infix_formula = rule { term ~ ( "=" ~ Ws ~ term ~> ( Eq( _: Expr, _ ) ) | "!=" ~ Ws ~ term ~> ( ( _: Expr ) !== _ ) ) }

  private def fol_quantifier = rule { ( "!" ~ push( expr.formula.All ) | "?" ~ push( Ex ) ) ~ Ws }
  private def binary_connective = rule {
    ( ( "<=>" ~ push( ( a: Expr, b: Expr ) => a <-> b ) ) |
      ( "=>" ~ push( Imp( _: Expr, _: Expr ) ) ) |
      ( "<=" ~ push( ( a: Expr, b: Expr ) => Imp( b, a ) ) ) |
      ( "<~>" ~ push( ( a: Expr, b: Expr ) => -( a <-> b ) ) ) |
      ( "~|" ~ push( ( a: Expr, b: Expr ) => -( a | b ) ) ) |
      ( "~&" ~ push( ( a: Expr, b: Expr ) => -( a & b ) ) ) ) ~ Ws
  }

  private def term: Rule1[Expr] = rule { variable | ( distinct_object ~> ( FOLConst( _ ) ) ) | ( number ~> ( FOLConst( _ ) ) ) | function_term }
  private def function_term = rule { name ~ ( "(" ~ Ws ~ term.+.separatedBy( Comma ) ~ ")" ~ Ws ).? ~> ( ( hd, as ) => TptpTerm( hd, as.getOrElse( Seq() ) ) ) }
  private def typed_variable = rule { capture( upper_word ) ~ Ws ~ ":" ~ Ws ~ basic_type ~> ( Var( _, _ ) ) }
  private def variable = rule { capture( upper_word ) ~ Ws ~> ( FOLVar( _: String ) ) }
  private def arguments = rule { term.+.separatedBy( Comma ) }

  private def include = rule { "include(" ~ Ws ~ file_name ~ formula_selection ~ ")." ~ Ws ~> ( IncludeDirective( _, _ ) ) }
  private def formula_selection = rule { ( "," ~ Ws ~ "[" ~ name.*.separatedBy( Comma ) ~ "]" ~ Ws ).? }

  private def general_list: Rule1[Seq[SigTo[Expr]]] = rule { "[" ~ Ws ~ general_term.*.separatedBy( Comma ) ~ "]" ~ Ws }
  private def general_terms = rule { general_term.+.separatedBy( Comma ) }
  private def general_term: Rule1[SigTo[Expr]] = rule {
    general_data ~ ( ":" ~ Ws ~ general_term ).? ~> ( ( d, to ) => ( sig: Sig ) => {
      val dExp = d( sig )
      to.fold( dExp )( t => GeneralColon( dExp, t( sig ) ) )
    } ) |
      general_list ~> ( ( l: Seq[SigTo[Expr]] ) => ( ( sig: Sig ) => GeneralList( sig( l ) ) ) )
  }
  private def general_data: Rule1[SigTo[Expr]] = rule {
    formula_data | general_function | atomic_word ~> ( ( s: String ) => Sig.mReturn( FOLConst( s ) ) ) |
      lift( variable ) | number ~> ( ( s: String ) => Sig.mReturn( FOLConst( s ) ) ) | distinct_object ~> ( ( s: String ) => Sig.mReturn( FOLConst( s ) ) )
  }
  private def formula_data: Rule1[SigTo[Expr]] = rule {
    ( ( capture( "$" ~ ( "fof" | "cnf" ) ) ~ "(" ~ Ws ~ lift( formula ) ~ ")" ~ Ws ) |
      ( capture( "$t" ~ ( "ff" | "hf" | "cf" ) ) ~ "(" ~ Ws ~ tff_logic_formula ~ ")" ~ Ws ) |
      ( capture( "$fot" ) ~ "(" ~ Ws ~ lift( term ) ~ ")" ~ Ws ) ) ~> ( ( s: String, t: SigTo[Expr] ) => ( sig: Sig ) => TptpTerm( s, t( sig ) ) )
  }
  private def general_function = rule { atomic_word ~ "(" ~ Ws ~ general_terms ~ ")" ~ Ws ~> ( ( s: String, gt: Seq[SigTo[Expr]] ) => ( sig: Sig ) => ( TptpTerm( s, sig( gt ) ) ) ) }

  // ==========
  // tff
  // ==========
  //
  private def tff_logic_formula: Rule1[SigTo[Formula]] = rule { tff_unit_formula }

  private def tff_unit_formula: Rule1[SigTo[Formula]] = rule { "(" ~ Ws ~ tff_logic_formula ~ Ws ~ ")" | tff_quantified_formula }

  private def tff_quantified_formula = rule { fol_quantifier ~ Ws ~ "[" ~ tff_variable_list ~ "]" ~ Ws ~ ":" ~ Ws ~ tff_logic_formula ~> ( ( q: QuantifierHelper, vs, m ) => ( sig: Sig ) => q.Block( vs( sig ), m( sig ) ) ) }

  private def tff_variable_list = rule { tff_variable.+.separatedBy( Comma ) ~> ( ( vs: Seq[SigTo[Var]] ) => ( sig: Sig ) => sig.apply( vs ) ) }

  private def tff_variable: Rule1[SigTo[Var]] = rule { capture( upper_word ) ~ Ws ~ ":" ~ Ws ~ tff_atomic_type ~> ( ( sym: String, t: SigTo[Ty] ) => ( ( sig: Sig ) => Var( sym, t( sig ) ) ) ) }

  private def tff_top_level_type: Rule1[SigTo[Ty]] = rule { tff_atomic_type | tff_non_atomic_type }

  private def tff_non_atomic_type: Rule1[SigTo[Ty]] = rule { tff_mapping_type } // TODO:

  def tff_mapping_type: Rule1[SigTo[Ty]] = rule {
    tff_unitary_type ~ Ws ~ ">" ~ Ws ~ tff_atomic_type ~> ( ( f: SigTo[Ty], t: SigTo[Ty] ) => ( sig: Sig ) => {
      val from = f( sig )
      val to = t( sig )
      if ( containsTy( from, To ) ) throw new TptpNotYetImplementedException( "Currently there is only support for TF0, so no $o parameters to functions and predicates" )

      val ( fromVars, fromTypes ) = partitionTypesAndVars( from )
      if ( fromVars.nonEmpty && fromTypes.isEmpty ) {
        to match {
          case TVar( name ) => {
            println( s"type constructor: $fromVars > $to" )
            from ->: to
          }
          case _ => {
            println( s"mismatched type constructor: $fromVars > $to; vars: $fromVars" )
            throw new MalformedInputFileException( "Illegal mix of types and type vars" )
          }
        }
      } else if ( fromVars.isEmpty && fromTypes.nonEmpty ) {
        to match {
          case TVar( name ) => {
            throw new MalformedInputFileException( s"Illegal mix of types and type vars, from: $fromVars | $fromTypes, to: $to" )
          }
          case _ => {
            println( s"function definition: $fromTypes > $to" )
            from ->: to
          }
        }
      } else {
        throw new MalformedInputFileException( "Illegal mix of types and type vars" )
      }
    } )
  }

  private def tff_xprod_type: Rule1[SigTo[Ty]] = rule {
    ( tff_unitary_type ~ ( Ws ~ "*" ~ Ws ~ tff_atomic_type ).+ ) ~> ( ( l: SigTo[Ty], r: Seq[SigTo[Ty]] ) => ( sig: Sig ) => {
      val left = l( sig )
      val rights = sig( r )

      left ->: rights.reduceRight( _ ->: _ )
    } )
  }

  private def tff_unitary_type: Rule1[SigTo[Ty]] = rule { ( "(" ~ Ws ~ tff_xprod_type ~ Ws ~ ")" ) | tff_atomic_type }

  private def tff_atomic_type: Rule1[SigTo[Ty]] = rule {
    "(" ~ Ws ~ tff_atomic_type ~ Ws ~ ")" | tff_defined_type |
      tff_type_constructor | tff_type_constant
  }

  private def tff_type_constructor = rule {
    tff_type_constant ~ Ws ~ "(" ~ Ws ~ tff_atomic_type.+.separatedBy( Comma ) ~ Ws ~ ")" ~> ( ( tyFunc: SigTo[Ty], args: Seq[SigTo[Ty]] ) => ( sig: Sig ) => {
      val typeFunctor = tyFunc( sig )
      val typeVars = extractVars( typeFunctor )
      val typeArgs = sig( args )
      val typeVarsToActualMap = typeVars.zip( typeArgs ).toMap

      println( s"substituting in $typeFunctor with $typeVarsToActualMap, from $typeVars to $typeArgs" )

      val subst = new expr.subst.Substitution( Map(), typeVarsToActualMap )
      subst( typeFunctor )

      // TBase( name, args.map( _( sig ) ) )
    } )
  }

  private def tff_defined_type = rule {
    capture( dollar_word ) ~> ( ( name: String ) => ( ( sig: Sig ) =>
      name match {
        case "$o"     => To
        case "$oType" => To
        case "$i"     => Ti
        case "$iType" => Ti
        case "$real"  => TReal
        case "$rat"   => TRat
        case "$int"   => TInt
        // case "$tType" => throw new TptpNotYetImplementedException( "Currently there is no support for TFF1, so $tType can only appear in \"tff(user_sort_type, type, user_sort: $tType).\" style fragments" )
        case "$tType" => sig.getNextTyVar
        case name     => throw new MalformedInputFileException( "Expected $o, $oType, $i, $iType, $real, $int, $rat, got unexpected dollar word type \"" + name + "\"" )
      } ) )

  }

  private def tff_type_constant = rule { atomic_word ~> ( ( sym: String ) => ( sig: Sig ) => sig.types.get( sym ).getOrElse( throw new MalformedInputFileException( "Type \"" + sym + "\") not defined in context; Known types: " + sig.types ) ) ) }

  // private def tff_formula = rule { tff_typed_logic_formula }
  // private def tff_typed_logic_formula = rule { tff_logic_formula } //add type annotation
  //
  // def tff_logic_formula: Rule1[SigTo[Formula]] = rule { tff_unitary_formula ~ ( tff_binary_nonassoc_part | tff_or_formula_part | tff_and_formula_part ).? }
  //
  // def tff_non_atomic_formula: Rule1[SigTo[Formula]] = rule {
  //   ( tff_quantified_formula | tff_unary_formula ) ~
  //     ( tff_binary_nonassoc_part | tff_or_formula_part | tff_and_formula_part ).?
  // }
  //
  // private def tff_binary_nonassoc_part = rule { binary_connective ~ tff_unitary_formula ~> ( ( a: SigTo[Formula], c: ( Expr, Expr ) => Formula, b: SigTo[Formula] ) => ( sig: Sig ) => c( a( sig ), b( sig ) ) ) }
  // private def tff_or_formula_part = rule { ( "|" ~ Ws ~ tff_unitary_formula ).+ ~> ( ( a: SigTo[Formula], as: Seq[SigTo[Formula]] ) => ( sig: Sig ) => Or.leftAssociative( a( sig ) +: sig( as ): _* ) ) }
  // private def tff_and_formula_part = rule { ( "&" ~ Ws ~ tff_unitary_formula ).+ ~> ( ( a: SigTo[Formula], as: Seq[SigTo[Formula]] ) => ( sig: Sig ) => And.leftAssociative( a( sig ) +: sig( as ): _* ) ) }
  // private def tff_unitary_formula: Rule1[SigTo[Formula]] = rule { tff_quantified_formula | tff_unary_formula | tff_atomic_formula | "(" ~ Ws ~ tff_logic_formula ~ ")" ~ Ws }
  // def tff_quantified_formula = rule {
  //   fol_quantifier ~ "[" ~ Ws ~ tff_variable_list ~ "]" ~ Ws ~ ":" ~ Ws ~ tff_unitary_formula ~> ( ( q: QuantifierHelper, variable_list, formula ) => ( sig: Sig ) => {
  //     val vars = variable_list.map( _( sig ) )
  //     q.Block( vars, formula( Sig( sig, vars ) ) )
  //   } )
  // }
  // private def tff_unary_formula = rule { "~" ~ Ws ~ tff_unitary_formula ~> ( f => ( sig: Sig ) => Neg( f( sig ) ) ) }
  //
  // private def tff_atomic_formula = rule { lift( defined_prop ) | tff_defined_predicate_formula | txf_conditional_boolean | tff_infix_formula | tff_plain_atomic_formula | ( distinct_object ~> ( ( o: String ) => Sig.mReturn( FOLAtom( o ) ) ) ) }
  // private def tff_defined_predicate_formula = rule {
  //   tff_defined_unary_predicate ~ "(" ~ Ws ~ tff_term ~ Ws ~ ")" ~> ( ( p, a ) => ( sig: Sig ) => p( a( sig ) ) ) |
  //     tff_defined_binary_predicate ~ "(" ~ Ws ~ tff_term ~ Comma ~ tff_term ~ Ws ~ ")" ~> ( ( p, a, b ) => ( sig: Sig ) => p( a( sig ), b( sig ) ) )
  // }
  //
  // private def tff_defined_unary_predicate = rule {
  //   ( "$is_int" ~ Ws ~ push( ( a: Expr ) => TptpAtom( "$is_int", Seq( a ) ) ) ) |
  //     ( "$is_rat" ~ Ws ~ push( ( a: Expr ) => TptpAtom( "$is_rat", Seq( a ) ) ) )
  // }
  //
  // private def tff_defined_binary_predicate = rule {
  //   ( "$lesseq" ~ Ws ~ push( ( a: Expr, b: Expr ) => LesserEq( a, b ) ) ) |
  //     ( "$less" ~ Ws ~ push( ( a: Expr, b: Expr ) => Lesser( a, b ) ) ) |
  //     ( "$greatereq" ~ Ws ~ push( ( a: Expr, b: Expr ) => GreaterEq( a, b ) ) ) |
  //     ( "$greater" ~ Ws ~ push( ( a: Expr, b: Expr ) => Greater( a, b ) ) )
  // }
  //
  // private def tff_plain_atomic_formula = rule {
  //   atomic_word ~ ( "(" ~ Ws ~ tff_arguments ~ ")" ~ Ws ).? ~> ( ( p: String, as: Option[Seq[Sig => Expr]] ) =>
  //     ( sig: Sig ) => TptpAtom( p, as.map( sig( _ ) ).getOrElse( Seq() ), sig ) )
  // }
  // private def tff_infix_formula = rule { tff_term ~ ( "=" ~ Ws ~ tff_term ~> ( ( a: SigTo[Expr], b ) => ( sig: Sig ) => Eq( a( sig ): Expr, b( sig ) ) ) | "!=" ~ Ws ~ tff_term ~> ( ( a: SigTo[Expr], b ) => ( sig: Sig ) => ( a( sig ): Expr ) !== b( sig ) ) ) }
  //
  // private def tff_term: Rule1[SigTo[Expr]] = rule { tff_variable | ( distinct_object ~> ( d => Sig.mReturn( FOLConst( d ) ) ) ) | tff_number | tff_defined_function_term | tff_function_term | tff_non_atomic_formula }
  //
  // private def tff_number: Rule1[SigTo[Expr]] = rule {
  //   rational ~> { ( n: String ) => Sig.mReturn( Const( n, TRat, Nil ) ) } |
  //     real ~> { ( n: String ) => Sig.mReturn( Const( n, TReal, Nil ) ) } |
  //     integer ~> { ( n: String ) => Sig.mReturn( Const( n, TInt, Nil ) ) }
  // }
  //
  // private def tff_function_term: Rule1[SigTo[Expr]] = rule {
  //   name ~ ( "(" ~ Ws ~ tff_term.+.separatedBy( Comma ) ~ ")" ~ Ws ).? ~> ( ( hd: String, as: Option[Seq[SigTo[Expr]]] ) => ( ( sig: Sig ) => TptpTerm( hd, as.getOrElse( Seq() ), sig ) ) )
  // }
  // private def tff_defined_function_term: Rule1[SigTo[Expr]] = rule {
  //   // unary operators
  //   tff_unary_arithmetic_op( "$uminus" ) |
  //     tff_unary_arithmetic_op( "$floor" ) |
  //     tff_unary_arithmetic_op( "$ceiling" ) |
  //     tff_unary_arithmetic_op( "$truncate" ) |
  //     tff_unary_arithmetic_op( "$round" ) |
  //     // coercions
  //     tff_unary_arithmetic_coercion( "$to_int", TInt ) |
  //     tff_unary_arithmetic_coercion( "$to_real", TReal ) |
  //     tff_unary_arithmetic_coercion( "$to_rat", TRat ) |
  //     // binary operators
  //     tff_binary_arithmetic_op( "$sum" ) |
  //     tff_binary_arithmetic_op( "$product" ) |
  //     tff_binary_arithmetic_op( "$difference" ) |
  //     tff_binary_arithmetic_op( "$quotient" ) |
  //     tff_binary_arithmetic_op( "$quotient_e" ) |
  //     tff_binary_arithmetic_op( "$quotient_t" ) |
  //     tff_binary_arithmetic_op( "$quotient_f" ) |
  //     tff_binary_arithmetic_op( "$remainder_e" ) |
  //     tff_binary_arithmetic_op( "$remainder_t" ) |
  //     tff_binary_arithmetic_op( "$remainder_f" ) |
  //     txf_conditional_ad_hoc
  // }
  //
  // private def txf_conditional_boolean = rule {
  //   ( "$ite(" ~ Ws ~ tff_logic_formula ~ Ws ~ "," ~ Ws ~ tff_term ~ Ws ~ "," ~ Ws ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> (
  //     ( bool: SigTo[Formula], then_val: SigTo[Expr], else_val: SigTo[Expr] ) => ( sig: Sig ) => {
  //       val then_int = then_val( sig )
  //       val else_int = else_val( sig )
  //       if ( then_int.ty != else_int.ty ) {
  //         throw new MalformedInputFileException( "Expected then and else term of $ite to be of same type, got if: " + then_int.ty + "; then: " + else_int.ty )
  //       }
  //       TptpAtom( "$ite", Seq( bool( sig ), then_int, else_int ) )
  //       // return Apps( Const( "$ite", FunctionType( else_int.ty, Seq( To, then_int.ty, then_int.ty ) ) ), Seq( bool( sig ), then_int, else_int ) ).asInstanceOf[Atom]
  //     } )
  // }
  //
  // private def txf_conditional_ad_hoc = rule {
  //   "$ite(" ~ Ws ~ tff_logic_formula ~ Ws ~ "," ~ Ws ~ tff_term ~ Ws ~ "," ~ Ws ~ tff_term ~ ")" ~> (
  //     ( bool: SigTo[Formula], then_val: SigTo[Expr], else_val: SigTo[Expr] ) => ( sig: Sig ) => {
  //       val then_int = then_val( sig )
  //       val else_int = else_val( sig )
  //       if ( then_int.ty != else_int.ty ) {
  //         throw new MalformedInputFileException( "Expected then and else term of $ite to be of same type, got if: " + then_int.ty + "; then: " + else_int.ty )
  //       }
  //       // return Const("$ite", FunctionType(else_int.ty, Seq(To, then_int.ty, then_int.ty)))
  //       // core/src/main/scala/gapt/formats/tptp/TptpParser.scala|328 col 16-81 error| type mismatch; found   : gapt.expr.Expr required: org.parboiled2.Rule1[TptpParser.this.SigTo[gapt.expr.Expr]] (which expands to)  org.parboiled2.Rule[org.parboiled2.support.hlist.HNil,gapt.formats.tptp.Sig => gapt.expr.Expr :: org.parboiled2.support.hlist.HNil]
  //       TptpTerm( "$ite", Seq( bool( sig ), then_int, else_int ), then_int.ty )
  //       // return Apps( Const( "$ite", FunctionType( else_int.ty, Seq( To, then_int.ty, then_int.ty ) ) ), Seq( bool( sig ), then_int, else_int ) ).asInstanceOf[Atom]
  //     } )
  // }
  //
  // //TODO: remove arguments that it can be inserted as macro
  // private def tff_unary_arithmetic_op( name: String ): Rule1[SigTo[Expr]] = rule {
  //   ( f"$name" ~ "(" ~ Ws ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> (
  //     ( a: SigTo[Expr] ) => ( sig: Sig ) => UnaryTFATerm( name, a, sig( a ).ty, sig ) )
  // }
  //
  // //TODO: remove arguments that it can be inserted as macro
  // private def tff_unary_arithmetic_coercion( name: String, to: Ty ): Rule1[SigTo[Expr]] = rule {
  //   ( f"$name" ~ "(" ~ Ws ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> (
  //     ( a: SigTo[Expr] ) => ( sig: Sig ) => UnaryTFATerm( name, a, to, sig ) )
  // }
  //
  // private def tff_binary_arithmetic_op( name: String ): Rule1[SigTo[Expr]] = rule {
  //   ( name ~ "(" ~ Ws ~ tff_term ~ Comma ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> ( ( a: SigTo[Expr], b: SigTo[Expr] ) =>
  //     ( sig: Sig ) => BinaryTFATerm( name, a, b, sig ) )
  // }
  //
  // private def tff_arguments: Rule1[Seq[Sig => Expr]] = rule { tff_term.+.separatedBy( Comma ) }
  //
  // private def tff_general_function = rule { atomic_word ~ "(" ~ Ws ~ general_terms ~ ")" ~ Ws ~> ( ( n: String, gt: Seq[SigTo[Expr]] ) => ( sig: Sig ) => TptpTerm( n, gt.map( _( sig ) ) ) ) }
  //
  // def tff_variable_list: Rule1[Seq[Sig => Var]] = rule { ( ( tff_typed_variable | lift( variable ) ) ).+.separatedBy( Comma ) }
  // private def tff_typed_variable = rule { capture( upper_word ) ~ Ws ~ ":" ~ Ws ~ tff_complex_type ~> ( ( name, t ) => ( ( sig: Sig ) => Var( name, t( sig ) ) ) ) }
  // private def tff_variable: Rule1[( Sig ) => Var] = rule {
  //   capture( upper_word ) ~ Ws ~> ( ( n: String ) => ( sig: Sig ) =>
  //     {
  //       // TODO: are all variables necessarily quantified
  //       sig.vars.get( n ).getOrElse( Var( n, Ti ) )
  //     } )
  // }
  //
  // private def tff_complex_type: Rule1[SigTo[Ty]] = rule { tff_mapping_type | tff_product_type | tff_basic_type }
  // private def tff_mapping_type: Rule1[SigTo[Ty]] = rule {
  //   ( tff_basic_type | ( "(" ~ Ws ~ tff_product_type ~ Ws ~ ")" ) ) ~ Ws ~ ">" ~ Ws ~ tff_complex_type ~>
  //     ( ( t: SigTo[Ty], t2: SigTo[Ty] ) =>
  //       ( sig: Sig ) => {
  //         fixCurrying( sig( t ), t2( sig ) )
  //       } )
  // }
  //
  // private def tff_product_type: Rule1[SigTo[Ty]] = rule {
  //   tff_basic_type ~ Ws ~ "*" ~ Ws ~ tff_complex_type ~> (
  //     ( bt: SigTo[Ty], ct: SigTo[Ty] ) =>
  //       ( sig: Sig ) => expr.ty.TArr( bt( sig ), ct( sig ) ) )
  // }
  //
  // // private def product_type = rule { root_type ~ ""}
  // private def tff_basic_type: Rule1[SigTo[Ty]] = rule {
  //   atomic_word ~> ( ( name: String ) => ( ( sig: Sig ) =>
  //     name match {
  //       case "$o"    => To
  //       case "$i"    => Ti
  //       case "$real" => TReal
  //       case "$rat"  => TRat
  //       case "$int"  => TInt
  //       case name    => sig.types.get( name ).getOrElse( throw new MalformedInputFileException( "Type (" + name + ") not defined in context; Known types: " + sig.types ) )
  //     } ) )
  // }

  private def name: Rule1[String] = rule { atomic_word | integer }
  // We include defined words as atomic_word, since no prover can keep them apart...
  private def atomic_word = rule { ( capture( lower_word ) ~ Ws ) | single_quoted }

  private def number = rule { rational | real | integer }

  private def file_name = rule { single_quoted }

  private def single_quoted = rule { '\'' ~ sg_char.* ~ '\'' ~ Ws ~> ( ( l: Seq[String] ) => l.mkString ) }

  private def distinct_object = rule { '"' ~ do_char.* ~ '"' ~ Ws ~> ( ( l: Seq[String] ) => l.mkString ) }

  private val alpha_numeric = UpperAlpha ++ LowerAlpha ++ Digit ++ CharPredicate( "$_" )
  private def upper_word = rule { UpperAlpha ~ alpha_numeric.* }
  private def lower_word = rule { ( LowerAlpha ++ CharPredicate( "$_" ) ) ~ alpha_numeric.* }
  private def dollar_word = rule { "$" ~ alpha_numeric.* }

  private def real = rule { capture( anyOf( "+-" ).? ~ decimal ~ ( '.' ~ Digit.* ) ~ ( anyOf( "Ee" ) ~ anyOf( "+-" ).? ~ decimal ).? ) ~ Ws }
  private def rational = rule { capture( anyOf( "+-" ).? ~ decimal ~ '/' ~ positive_decimal ) ~ Ws }
  private def integer = rule { capture( anyOf( "+-" ).? ~ decimal ) ~ Ws }
  private def decimal = rule { '0' | positive_decimal }
  private def positive_decimal = rule { Digit19 ~ Digit.* }

  private val do_char_pred = CharPredicate( ' ' to '!', '#' to '[', '(' to '[', ']' to '~' )
  private def do_char = rule { capture( do_char_pred ) | ( "\\\\" ~ push( "\\" ) ) | ( "\\\"" ~ push( "\"" ) ) }
  private val sg_char_pred = CharPredicate( ' ' to '&', '(' to '[', ']' to '~' )
  private def sg_char = rule { capture( sg_char_pred ) | ( "\\\\" ~ push( "\\" ) ) | ( "\\'" ~ push( "'" ) ) }

  private def complex_type: Rule1[Ty] = rule { ( basic_type ~ !( Ws ~ ( ">" | "*" ) ) ) | mapping_type | product_type }
  private def mapping_type = rule { ( basic_type | ( "(" ~ Ws ~ product_type ~ Ws ~ ")" ) ) ~ Ws ~ ">" ~ Ws ~ complex_type ~> ( expr.ty.TArr ) }
  private def product_type = rule { basic_type ~ Ws ~ "*" ~ Ws ~ complex_type ~> ( expr.ty.TArr ) }
  // private def product_type = rule { root_type ~ ""}
  private def basic_type = rule {
    atomic_word ~> ( name =>
      name match {
        case "$o" => To
        case "$i" => Ti
        case name => TBase( name )
      } )
  }

  private def lift[A]( inner: Rule1[A] ): Rule1[SigTo[A]] = {
    rule { inner ~> ( ( res: A ) => Sig.mReturn( res ) ) }
  }

  private def fixCurrying( in: Ty, out: Ty ): Ty = {
    in match {
      case i ->: o => expr.ty.TArr( i, fixCurrying( o, out ) )
      case _       => expr.ty.TArr( in, out )
    }
  }

  private def containsTy( examined: Ty, searched: Ty ): Boolean = {
    examined match {
      case i ->: o => return containsTy( i, searched ) || containsTy( o, searched )
      case _       => return examined == searched
    }
  }

}

object TptpImporter {
  /**
   * Parse a TPTP file, but do not resolve include directives.
   */
  private def parse( file: InputFile, sig: Sig = Sig.default ): ( Sig, TptpFile ) = {
    val input = file.read
    val parser = new TptpParser( input )
    parser.TPTP_file.run() match {
      case Failure( error: ParseError ) =>
        throw new IllegalArgumentException( s"Parse error in ${file.fileName}:\n" +
          parser.formatError( error, new ErrorFormatter( showTraces = true ) ) )
      case Failure( exception ) =>
        throw exception
      // TODO: rework list of types in context, maybe move to parser def

      case Success( value ) =>
        ( sig, value( sig ) )
    }
  }

  /**
   * Load a TPTP file, but don't resolve includes.
   * @param file The input file to load.
   * @return The parsed file.
   */
  def loadWithoutIncludes( file: InputFile ): TptpFile = parse( file )._2

  /**
   * Load a TPTP file and resolve includes.
   * @param file The input file to load.
   * @param resolver How to resolve included files.
   * @return The parsed file.
   */
  def loadWithIncludes( file: InputFile, resolver: String => TptpFile ): TptpFile =
    resolveIncludes( parse( file )._2, resolver )

  def loadWithIncludes( file: InputFile, relativeTo: Path ): TptpFile =
    loadWithIncludes( file, fileName => parse( Path( fileName, relativeTo ) )._2 )

  def loadWithIncludes( file: InputFile, relativeTo: FilePath ): TptpFile =
    loadWithIncludes( file, Path( relativeTo, pwd ) )

  def loadWithIncludes( file: InputFile, relativeTo: String ): TptpFile =
    loadWithIncludes( file, FilePath( relativeTo ) )

  def loadWithIncludes( file: InputFile ): TptpFile =
    loadWithIncludes( file, pwd )

  def main( args: Array[String] ): Unit =
    print( loadWithIncludes( FilePath( args.head ) ) )

}
