package gapt.formats.tptp

import gapt.expr._
import gapt.formats.InputFile
import org.parboiled2._
import ammonite.ops._
import gapt.expr
import gapt.expr.formula.And
import gapt.expr.formula.Bottom
import gapt.expr.formula.Eq
import gapt.expr.formula.GreaterEq
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
import gapt.expr.ty.TBase
import gapt.expr.ty.Ti
import gapt.expr.ty.To
import gapt.expr.ty.Ty
import gapt.expr.ty.->:

import scala.util.{ Failure, Success }
import gapt.formats.tptp.GeneralTerm
import org.parboiled2.support.hlist
import org.parboiled2.support.hlist.HNil
import gapt.formats.tptp.statistics.ParsingError
import gapt.formats.tptp.TptpFile

class Ctx( val vars: Map[String, Var], val types: Map[String, Ty] ) {

  def apply[A]( to: A ): A = to

  def apply[A]( to: ( Ctx => A ) ): A = {
    to( this )
  }

  def apply[A]( to: Seq[( Ctx => A )] ): Seq[A] = {
    to.map( _( this ) )
  }

  def apply[A]( to: Option[A] ): Option[A] = {
    to.map( this( _ ) )
  }

  def >=[A]( to: ( Ctx => A ) ): A = {
    to( this )
  }
}
//

// trait CtxTo[A] {
//   def apply(ctx: Ctx): A
// }

object Ctx {

  def mReturn[A]( value: A ): ( Ctx => A ) = {
    ( ctx ) => value
  }

  def unapply( ctx: Ctx ): Option[Tuple2[Map[String, Var], Map[String, Ty]]] = {
    Some( ( ctx.vars, ctx.types ) )
  }

  def apply( vars: Map[String, Var], types: Map[String, Ty] ): Ctx = {
    new Ctx( vars, types )
  }

  def apply( ctx: Ctx, name: String, v: Var ): Ctx = {
    ctx match {
      case Ctx( vars, types ) => {
        Ctx( vars + ( name -> v ), types )
      }
    }
  }

  def apply( ctx: Ctx, addVars: Seq[Var] ): Ctx = {
    ctx match {
      case Ctx( vars, types ) => {
        Ctx( vars ++ addVars.map( v => {
          v match {
            case Var( name, ty ) => ( name, Var( name, ty ) )
          }
        } ).toMap, types )
      }
    }
  }

  def apply( ctx: Ctx, name: String, t: Ty ): Ctx = {
    ctx match {
      case Ctx( vars, types ) => {
        Ctx( vars, types + ( name -> t ) )
      }
    }
  }

  def apply(): Ctx = {
    new Ctx( Map.empty[String, Var], Map.empty[String, Ty] )
  }

}

// object TNum extends TBase("$num", List())

class TptpParser( val input: ParserInput ) extends Parser {
  import CharPredicate._

  type CtxTo[A] = Ctx => A

  private def Ws = rule {
    quiet( zeroOrMore( anyOf( " \t \n" ) |
      ( '%' ~ zeroOrMore( noneOf( "\n" ) ) ) |
      ( "/*" ~ not_star_slash ~ oneOrMore( "*" ) ~ "/" ) ) )
  }
  private def not_star_slash = rule { ( noneOf( "*" ).* ~ oneOrMore( "*" ) ~ noneOf( "/*" ) ).* ~ noneOf( "*" ).* }
  private def Comma = rule { "," ~ Ws }
  private def Colon = rule { ":" ~ Ws }

  // def TPTP_file: Rule1[CtxTo[TptpFile]] = rule { Ws ~ TPTP_input.* ~ EOI ~> ( ( seq: Seq[CtxTo[TptpInput]] ) => ( ctx: Ctx ) => ( TptpFile( seq.map( _( ctx ) ) ) ) ) }
  def TPTP_file: Rule1[CtxTo[TptpFile]] = rule {
    Ws ~ TPTP_input.* ~ EOI ~> ( ( seq: Seq[CtxTo[TptpInput]] ) => ( ctx: Ctx ) =>
      seq.foldLeft( ( ctx, Seq[TptpInput]() ) ) {
        ( acc, ctxToFormula ) =>
          acc match {
            case ( c, s ) => {
              val formula = ctxToFormula( c )
              formula match {
                case TypeDef( _, _, name, ty, o ) => {
                  ( Ctx( c, name, ty ), s :+ formula )
                }
                case ConstDef( _, _, name, v, _ ) => {
                  ( Ctx( c, name, v ), s :+ formula )
                }
                case other => {
                  ( c, s :+ other )
                }
              }
            }
          }
      } match {
        case ( _, formulas ) => TptpFile( formulas )
      } )

  }

  // private def TPTP_input = rule { typedef_formula | annotated_formula | include }
  private def TPTP_input = rule { typedef_formula | atom_def_formula | tff_annotated_formula | annotated_formula | lift( include ) }

  private def annotated_formula: Rule1[CtxTo[TptpInput]] = rule {

    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ ( formula_role ~ Comma ~ lift( formula ) ) ~ annotations ~ ")." ~ Ws ~>
      ( ( lang: String, name: String, role: String, form: CtxTo[Formula], ann: Seq[CtxTo[GeneralTerm]] ) => ( ctx: Ctx ) => ( AnnotatedFormula( lang, name, role, form( ctx ), ann.map( _( ctx ) ) ) ) )
  }

  //  tff(animal_type,type, animal: $tType ).

  //  TODO
  def typedef_formula: Rule1[CtxTo[TypeDef]] = rule {
    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ Ws ~ "type" ~ Ws ~ Comma ~ Ws ~ atomic_word ~ Ws ~ ":" ~ Ws ~ "$tType" ~ Ws ~ annotations ~ ")." ~ Ws ~>
      ( ( lang: String, name: String, typeName: String, ann: Seq[CtxTo[GeneralTerm]] ) => ( ctx: Ctx ) => ( TypeDef( lang, name, typeName, TBase( typeName ), ann.map( _( ctx ) ) ) ) )
  }

  private def atom_def_formula: Rule1[CtxTo[ConstDef]] = rule {
    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ Ws ~ "type" ~ Ws ~ Comma ~ Ws ~ atomic_word ~ Ws ~ ":" ~ Ws ~ tff_complex_type ~ annotations ~ ")." ~ Ws ~>
      ( ( lang: String, name: String, varName: String, ty: CtxTo[Ty], ann: Seq[CtxTo[GeneralTerm]] ) => ( ctx: Ctx ) => ( ConstDef( lang, name, varName, Var( varName, ty( ctx ) ), ann.map( _( ctx ) ) ) ) )
  }

  private def tff_annotated_formula: Rule1[CtxTo[TptpInput]] = rule {
    "tff(" ~ Ws ~ name ~ Comma ~ ( formula_role ~ Comma ~ tff_logic_formula ) ~ annotations ~ ")." ~ Ws ~>
      ( ( name: String, role: String, form: CtxTo[Formula], ann: Seq[CtxTo[GeneralTerm]] ) => ( ctx: Ctx ) => ( AnnotatedFormula( "tff", name, role, form( ctx ), ann.map( _( ctx ) ) ) ) )
  }

  // TODO: maybe fix the list of possible roles to values defined in specs
  private def formula_role = rule { atomic_word }
  private def annotations = rule { ( Comma ~ general_term ).* }

  // TODO: implement (Either (tff_atom_typing, tff_subtype )
  private def typedef: Rule1[Formula] = rule { ( Ws ~ lower_word ~ ":" ~ Ws ~ complex_type ) ~> ( ( a: Ty ) => Top() ) }
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

  private def general_list: Rule1[Seq[CtxTo[Expr]]] = rule { "[" ~ Ws ~ general_term.*.separatedBy( Comma ) ~ "]" ~ Ws }
  private def general_terms = rule { general_term.+.separatedBy( Comma ) }
  private def general_term: Rule1[CtxTo[Expr]] = rule {
    general_data ~ ( ":" ~ Ws ~ general_term ).? ~> ( ( d, to ) => ( ctx: Ctx ) => {
      val dExp = d( ctx )
      to.fold( dExp )( t => GeneralColon( dExp, t( ctx ) ) )
    } ) |
      general_list ~> ( ( l: Seq[CtxTo[Expr]] ) => ( ( ctx: Ctx ) => GeneralList( ctx( l ) ) ) )
  }
  private def general_data: Rule1[CtxTo[Expr]] = rule {
    formula_data | general_function | atomic_word ~> ( ( s: String ) => Ctx.mReturn( FOLConst( s ) ) ) |
      lift( variable ) | number ~> ( ( s: String ) => Ctx.mReturn( FOLConst( s ) ) ) | distinct_object ~> ( ( s: String ) => Ctx.mReturn( FOLConst( s ) ) )
  }
  private def formula_data: Rule1[CtxTo[Expr]] = rule {
    ( ( capture( "$" ~ ( "fof" | "cnf" ) ) ~ "(" ~ Ws ~ lift( formula ) ~ ")" ~ Ws ) |
      ( capture( "$t" ~ ( "ff" | "hf" ) ) ~ "(" ~ Ws ~ tff_logic_formula ~ ")" ~ Ws ) |
      ( capture( "$fot" ) ~ "(" ~ Ws ~ lift( term ) ~ ")" ~ Ws ) ) ~> ( ( s: String, t: CtxTo[Expr] ) => ( ctx: Ctx ) => TptpTerm( s, t( ctx ) ) )
  }
  private def general_function = rule { atomic_word ~ "(" ~ Ws ~ general_terms ~ ")" ~ Ws ~> ( ( s: String, gt: Seq[CtxTo[Expr]] ) => ( ctx: Ctx ) => ( TptpTerm( s, ctx( gt ) ) ) ) }

  // ==========
  // tff
  // ==========
  // private def tff_formula = rule { tff_typed_logic_formula }
  // private def tff_typed_logic_formula = rule { tff_logic_formula } //add type annotation
  //
  def tff_logic_formula: Rule1[CtxTo[Formula]] = rule { tff_unitary_formula ~ ( tff_binary_nonassoc_part | tff_or_formula_part | tff_and_formula_part ).? }
  private def tff_binary_nonassoc_part = rule { binary_connective ~ tff_unitary_formula ~> ( ( a: CtxTo[Formula], c: ( Expr, Expr ) => Formula, b: CtxTo[Formula] ) => ( ctx: Ctx ) => c( a( ctx ), b( ctx ) ) ) }
  private def tff_or_formula_part = rule { ( "|" ~ Ws ~ tff_unitary_formula ).+ ~> ( ( a: CtxTo[Formula], as: Seq[CtxTo[Formula]] ) => ( ctx: Ctx ) => Or.leftAssociative( a( ctx ) +: ctx( as ): _* ) ) }
  private def tff_and_formula_part = rule { ( "&" ~ Ws ~ tff_unitary_formula ).+ ~> ( ( a: CtxTo[Formula], as: Seq[CtxTo[Formula]] ) => ( ctx: Ctx ) => And.leftAssociative( a( ctx ) +: ctx( as ): _* ) ) }
  private def tff_unitary_formula: Rule1[CtxTo[Formula]] = rule { tff_quantified_formula | tff_unary_formula | tff_atomic_formula | "(" ~ Ws ~ tff_logic_formula ~ ")" ~ Ws }
  def tff_quantified_formula = rule {
    fol_quantifier ~ "[" ~ Ws ~ tff_variable_list ~ "]" ~ Ws ~ ":" ~ Ws ~ tff_unitary_formula ~> ( ( q: QuantifierHelper, vs, m ) => ( ctx: Ctx ) => {
      val vars = vs.map( _( ctx ) )
      q.Block( vs.map( _( ctx ) ), m( Ctx( ctx, vars ) ) )
    } )
  }
  private def tff_unary_formula = rule { "~" ~ Ws ~ tff_unitary_formula ~> ( f => ( ctx: Ctx ) => Neg( f( ctx ) ) ) }

  private def tff_atomic_formula = rule { lift( defined_prop ) | tff_infix_formula | tff_defined_predicate_formula | tff_plain_atomic_formula | ( distinct_object ~> ( ( o: String ) => Ctx.mReturn( FOLAtom( o ) ) ) ) }
  private def tff_defined_predicate_formula = rule {
    tff_defined_predicate ~ "(" ~ Ws ~ tff_term ~ Comma ~ tff_term ~ Ws ~ ")" ~> ( ( p, a, b ) => ( ctx: Ctx ) => p( a( ctx ), b( ctx ) ) )
  }

  private def tff_defined_predicate = rule {
    ( "$less" ~ Ws ~> ( () => ( a: Expr, b: Expr ) => -( GreaterEq( a, b ) ) ) ) |
      ( "$greatereq" ~ Ws ~> ( () => ( a: Expr, b: Expr ) => GreaterEq( a, b ) ) ) |
      ( "$greater" ~ Ws ~> ( () => ( a: Expr, b: Expr ) => -( GreaterEq( b, a ) ) ) ) |
      ( "$lesseq" ~ Ws ~> ( () => ( a: Expr, b: Expr ) => GreaterEq( b, a ) ) )

  }

  private def tff_plain_atomic_formula = rule { atomic_word ~ ( "(" ~ Ws ~ tff_arguments ~ ")" ~ Ws ).? ~> ( ( p: String, as: Option[Seq[Ctx => Expr]] ) => ( ctx: Ctx ) => TptpAtom( p, as.map( ctx( _ ) ).getOrElse( Seq() ), ctx ) ) }
  private def tff_infix_formula = rule { tff_term ~ ( "=" ~ Ws ~ tff_term ~> ( ( a: CtxTo[Expr], b ) => ( ctx: Ctx ) => Eq( a( ctx ): Expr, b( ctx ) ) ) | "!=" ~ Ws ~ tff_term ~> ( ( a: CtxTo[Expr], b ) => ( ctx: Ctx ) => ( a( ctx ): Expr ) !== b( ctx ) ) ) }

  private def tff_term: Rule1[CtxTo[Expr]] = rule { tff_variable | ( distinct_object ~> ( d => Ctx.mReturn( FOLConst( d ) ) ) ) | ( number ~> ( n => Ctx.mReturn( FOLConst( n ) ) ) ) | tff_defined_function_term | tff_function_term }
  private def tff_function_term: Rule1[CtxTo[Expr]] = rule { name ~ ( "(" ~ Ws ~ tff_term.+.separatedBy( Comma ) ~ ")" ~ Ws ).? ~> ( ( hd: String, as: Option[Seq[CtxTo[Expr]]] ) => ( ( ctx: Ctx ) => TptpTerm( hd, as.getOrElse( Seq() ), ctx ) ) ) }
  private def tff_defined_function_term: Rule1[CtxTo[Expr]] = rule {
    ( "$uminus" ~ "(" ~ Ws ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> ( ( as: CtxTo[Expr] ) => ( ( ctx: Ctx ) => {
      val a = as( ctx )
      // TODO: maybe num type?
      TptpTerm( "$uminus", Seq( a ), a.ty )
    } ) ) |
      ( "$difference" ~ "(" ~ Ws ~ tff_term ~ Comma ~ tff_term ~ Ws ~ ")" ~ Ws ) ~> ( ( a: CtxTo[Expr], b: CtxTo[Expr] ) => ( ( ctx: Ctx ) => {
        val aFromCtx = a( ctx )
        val bFromCtx = b( ctx )
        if ( aFromCtx.ty != bFromCtx.ty ) {
          throw new IllegalArgumentException( "type mismatch: $difference expects two params of same type, got a: " + aFromCtx.ty + ", b: " + bFromCtx.ty )
        }
        TptpTerm( "$differenceTest", Seq( aFromCtx, bFromCtx ), aFromCtx.ty )
      } ) )
  }

  private def tff_arguments: Rule1[Seq[Ctx => Expr]] = rule { tff_term.+.separatedBy( Comma ) }

  private def tff_general_function = rule { atomic_word ~ "(" ~ Ws ~ general_terms ~ ")" ~ Ws ~> ( ( n: String, gt: Seq[CtxTo[Expr]] ) => ( ctx: Ctx ) => TptpTerm( n, gt.map( _( ctx ) ) ) ) }

  def tff_variable_list: Rule1[Seq[Ctx => Var]] = rule { ( ( tff_typed_variable | tff_variable ) ~> ( ( v: CtxTo[Var] ) => ( ctx: Ctx ) => ( v( ctx ) ) ) ).+.separatedBy( Comma ) }
  private def tff_typed_variable = rule { capture( upper_word ) ~ Ws ~ ":" ~ Ws ~ tff_complex_type ~> ( ( name, t ) => ( ( ctx: Ctx ) => Var( name, t( ctx ) ) ) ) }
  private def tff_variable: Rule1[( Ctx ) => Var] = rule {
    capture( upper_word ) ~ Ws ~> ( ( n: String ) => ( ctx: Ctx ) =>
      {
        // TODO: are all variables necessarily quantified
        ctx.vars.get( n ).getOrElse( Var( n, To ) )
      } )
  }

  private def tff_complex_type: Rule1[CtxTo[Ty]] = rule { tff_mapping_type | tff_product_type | tff_basic_type }
  private def tff_mapping_type: Rule1[CtxTo[Ty]] = rule {
    ( tff_basic_type | ( "(" ~ Ws ~ tff_product_type ~ Ws ~ ")" ) ) ~ Ws ~ ">" ~ Ws ~ tff_complex_type ~>
      ( ( t: CtxTo[Ty], t2: CtxTo[Ty] ) =>
        ( ctx: Ctx ) => {
          fixCurrying( ctx( t ), t2( ctx ) )
        } )
  }

  private def tff_product_type: Rule1[CtxTo[Ty]] = rule {
    tff_basic_type ~ Ws ~ "*" ~ Ws ~ tff_complex_type ~> (
      ( bt: CtxTo[Ty], ct: CtxTo[Ty] ) => ( ( ctx: Ctx ) => {
        expr.ty.TArr( bt( ctx ), ct( ctx ) )
      } ) )
  }

  // private def product_type = rule { root_type ~ ""}
  private def tff_basic_type: Rule1[CtxTo[Ty]] = rule {
    atomic_word ~> ( ( name: String ) => ( ( ctx: Ctx ) =>
      name match {
        case "$o"    => To
        case "$i"    => Ti
        case "$real" => TBase( "real" )
        case "$rat"  => TBase( "rat" )
        case "$int"  => TBase( "int" )
        // case "$real" => NumTy
        // case "$rat" => NumTy
        // case "$int" => NumTy
        case name    => ctx.types.get( name ).getOrElse( throw new MalformedInputFileException( "Type (" + name + ") not defined in context; Known types: " + ctx.types ) )
      } ) )
  }

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

  private def real = rule { capture( anyOf( "+-" ).? ~ decimal ~ ( '.' ~ Digit.* ).? ~ ( anyOf( "Ee" ) ~ anyOf( "+-" ).? ~ decimal ).? ) ~ Ws }
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

  private def lift[A]( inner: Rule1[A] ): Rule1[CtxTo[A]] = {
    rule { inner ~> ( ( res: A ) => Ctx.mReturn( res ) ) }
  }

  private def fixCurrying( in: Ty, out: Ty ): Ty = {
    in match {
      case i ->: o => expr.ty.TArr( i, fixCurrying( o, out ) )
      case _       => expr.ty.TArr( in, out )
    }
  }
}

object TptpImporter {
  /**
   * Parse a TPTP file, but do not resolve include directives.
   */
  private def parse( file: InputFile ): TptpFile = {
    val input = file.read
    val parser = new TptpParser( input )
    parser.TPTP_file.run() match {
      case Failure( error: ParseError ) =>
        throw new IllegalArgumentException( s"Parse error in ${file.fileName}:\n" +
          parser.formatError( error, new ErrorFormatter( showTraces = true ) ) )
      case Failure( exception ) => throw exception
      // TODO: rework list of types in context, maybe move to parser def
      case Success( value )     => value( new Ctx( Map(), Map( "$real" -> TBase( "$real" ), "$int" -> TBase( "$int" ), "$rat" -> TBase( "$rat" ), "$tType" -> TBase( "$tType" ) ) ) )
    }
  }

  /**
   * Load a TPTP file, but don't resolve includes.
   * @param file The input file to load.
   * @return The parsed file.
   */
  def loadWithoutIncludes( file: InputFile ): TptpFile = parse( file )

  /**
   * Load a TPTP file and resolve includes.
   * @param file The input file to load.
   * @param resolver How to resolve included files.
   * @return The parsed file.
   */
  def loadWithIncludes( file: InputFile, resolver: String => TptpFile ): TptpFile =
    resolveIncludes( parse( file ), resolver )

  def loadWithIncludes( file: InputFile, relativeTo: Path ): TptpFile =
    loadWithIncludes( file, fileName => parse( Path( fileName, relativeTo ) ) )

  def loadWithIncludes( file: InputFile, relativeTo: FilePath ): TptpFile =
    loadWithIncludes( file, Path( relativeTo, pwd ) )

  def loadWithIncludes( file: InputFile, relativeTo: String ): TptpFile =
    loadWithIncludes( file, FilePath( relativeTo ) )

  def loadWithIncludes( file: InputFile ): TptpFile =
    loadWithIncludes( file, pwd )

  def main( args: Array[String] ): Unit =
    print( loadWithIncludes( FilePath( args.head ) ) )

}
