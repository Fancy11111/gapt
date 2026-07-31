package gapt.formats.tptp

import gapt.expr._
import gapt.formats.InputFile
import org.parboiled2._
import os._
import gapt.expr
import gapt.expr.formula.And
import gapt.expr.formula.Bottom
import gapt.expr.formula.Eq
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
import gapt.expr.ty.*
import gapt.logic.fol.arithmetic.*

import scala.util.{Failure, Success}

class TptpTffParser(val input: ParserInput) extends Parser {
  import CharPredicate._

  private def Ws = rule {
    quiet(zeroOrMore(anyOf(" \t \n") |
      ('%' ~ zeroOrMore(noneOf("\n"))) |
      ("/*" ~ not_star_slash ~ oneOrMore("*") ~ "/")))
  }
  private def not_star_slash = rule { (noneOf("*").* ~ oneOrMore("*") ~ noneOf("/*")).* ~ noneOf("*").* }
  private def Comma = rule { "," ~ Ws }
  private def Colon = rule { ":" ~ Ws }

  def TPTP_file: Rule1[TptpFile] = rule { Ws ~ TPTP_input.* ~ EOI ~> (TptpFile(_)) }

  private def TPTP_input = rule { atom_def | annotated_formula | include }

  private def atom_def = rule {
    atomic_word ~ "(" ~ Ws ~ "type" ~ Comma ~ atomic_word ~ Comma ~ atomic_word ~ Colon ~ tff_top_level_type ~ annotations ~ ")." ~ Ws ~>
      (TopLevelDefinition(_, _, _, _, _))
  }

  private def annotated_formula = rule {
    atomic_word ~ "(" ~ Ws ~ name ~ Comma ~ formula_role ~ Comma ~ formula ~ annotations ~ ")." ~ Ws ~>
      (AnnotatedFormula(_, _, _, _, _))
  }
  private def formula_role = rule { atomic_word }
  private def annotations = rule { (Comma ~ general_term).* }

  private def formula = rule { typed_logic_formula }
  private def typed_logic_formula = rule { logic_formula } // add type annotation
  private def logic_formula: Rule1[Formula] = rule { unitary_formula ~ (binary_nonassoc_part | or_formula_part | and_formula_part).? }
  private def binary_nonassoc_part = rule { binary_connective ~ unitary_formula ~> ((a: Formula, c: (Expr, Expr) => Formula, b: Formula) => c(a, b)) }
  private def or_formula_part = rule { ("|" ~ Ws ~ unitary_formula).+ ~> ((a: Formula, as: Seq[Formula]) => Or.leftAssociative(a +: as: _*)) }
  private def and_formula_part = rule { ("&" ~ Ws ~ unitary_formula).+ ~> ((a: Formula, as: Seq[Formula]) => And.leftAssociative(a +: as: _*)) }
  private def unitary_formula: Rule1[Formula] = rule { quantified_formula | unary_formula | atomic_formula | "(" ~ Ws ~ logic_formula ~ ")" ~ Ws }
  private def quantified_formula = rule { fol_quantifier ~ "[" ~ Ws ~ variable_list ~ "]" ~ Ws ~ Colon ~ unitary_formula ~> ((q: QuantifierHelper, vs, m) => q.Block(vs, m)) }
  // private def variable_list = rule { (variable ~ (Colon ~ name).? ~> ((a, b) => a)).+.separatedBy(Comma) }
  private def unary_formula = rule { "~" ~ Ws ~ unitary_formula ~> (Neg(_)) }

  private def atomic_formula = rule { defined_prop | infix_formula | plain_atomic_formula | (distinct_object ~> (FOLAtom(_))) }
  private def plain_atomic_formula = rule { atomic_word ~ ("(" ~ Ws ~ arguments ~ ")" ~ Ws).? ~> ((p, as) => TptpAtom(p, as.getOrElse(Seq()))) }
  private def defined_prop = rule { "$" ~ Ws ~ ("true" ~ push(Top()) | "false" ~ push(Bottom())) ~ Ws }
  private def infix_formula = rule { term ~ ("=" ~ Ws ~ term ~> (Eq(_: Expr, _)) | "!=" ~ Ws ~ term ~> ((_: Expr) !== _)) }

  // private def tff_unitary_formula: Rule1[Formula] = rule { tff_quantified_formula | "(" ~ Ws ~ typed_logic_formula ~ ")" ~ Ws }

  private def tff_top_level_type: Rule1[Ty] = rule { tff_atomic_type | tff_non_atomic_type }
  private def tff_atomic_type: Rule1[Ty] = rule { (type_constant ~> (expr.ty.TBase(_, List()))) | (&("$") ~ defined_type) | (capture(upper_word) ~> (expr.ty.TVar(_))) | (type_functor ~ Ws ~ "(" ~ Ws ~ tff_type_arguments ~ Ws ~ ")" ~ Ws) ~> ((name, args) => TBase(name, args.toList)) | txf_tuple_type }
  private def tff_non_atomic_type: Rule1[Ty] = rule { tff_mapping_type | ("(" ~ Ws ~ tff_non_atomic_type ~ Ws ~ ")") }
  // private def tf1_quantified_type: Rule1[Ty] = rule { ("|>" | "?*") ~ Ws ~ "[" ~ Ws ~ tff_variable_list ~ Ws ~ "]" ~ Ws ~ ":" ~ tff_monotype ~ fail("quantified types are not supported") ~> ((a, b) => To) }
  // private def tf1_quantified_type = rule { ("|>" | "?*") ~ todo }
  private def variable_list = rule { tff_variable.+.separatedBy(Comma) }
  private def tff_variable = rule { capture(upper_word) ~ (Ws ~ Colon ~ Ws ~ tff_atomic_type).? ~ Ws ~> ((name, tyOpt) => Var(name, tyOpt.getOrElse(Ti))) }
  // private def tff_monotype = rule { tff_atomic_type | ("(" ~ Ws ~ tff_mapping_type ~ Ws ~ ")") | tf1_quantified_type }
  def tff_mapping_type = rule { tff_unitary_type ~ Ws ~ ">" ~ Ws ~ tff_atomic_type ~> ((left, right) => mkMapping(left, right)) }
  def mkMapping(left: Ty, target: Ty): Ty = left match { case TArr(l, r) => TArr(l, mkMapping(r, target)); case _ => TArr(left, target) }
  private def tff_unitary_type = rule { tff_atomic_type | ("(" ~ Ws ~ tff_xprod_type ~ Ws ~ ")") }
  // a * b * c * d == (a -> (b -> (c -> d)))
  private def tff_xprod_type = rule { tff_atomic_type ~ (Ws ~ "*" ~ Ws ~ tff_atomic_type).* ~> ((first, rest) => curry(first, rest)) }
  // a [b,c,d] => a > (b > (c > d))
  def curry(first: Ty, rest: Seq[Ty]): Ty = {
    val l: Seq[Ty] = (rest.reverse.:+(first))
    l.reduce((acc, c) => TArr(c, acc))
  }

  private def type_constant = rule { type_functor }
  private def type_functor = rule { atomic_word }
  private def defined_type: Rule1[Ty] = rule {
    atomic_word ~> ((name: String) =>
      name match {
        case "$o"    => To
        case "$i"    => Ti
        case "$real" => TReal
        case "$rat"  => TRat
        case "$int"  => TInt
        case other => {
          // TODO:
          TBase(other, List())
        }
      }
    )
  }
  private def tff_type_arguments: Rule1[Seq[Ty]] = rule { tff_atomic_type.+.separatedBy(Comma) }
  private def txf_tuple_type: Rule1[Ty] = rule { MISMATCH.named("TODO: figure out tuple representation") }

  private def fol_quantifier = rule { ("!" ~ push(expr.formula.All) | "?" ~ push(Ex)) ~ Ws }
  private def binary_connective = rule {
    (("<=>" ~ push((a: Expr, b: Expr) => a <-> b)) |
      ("=>" ~ push(Imp(_: Expr, _: Expr))) |
      ("<=" ~ push((a: Expr, b: Expr) => Imp(b, a))) |
      ("<~>" ~ push((a: Expr, b: Expr) => -(a <-> b))) |
      ("~|" ~ push((a: Expr, b: Expr) => -(a | b))) |
      ("~&" ~ push((a: Expr, b: Expr) => -(a & b)))) ~ Ws
  }

  private def term: Rule1[Expr] = rule { variable | (distinct_object ~> (FOLConst(_))) | (number ~> (FOLConst(_))) | function_term }
  private def function_term = rule { name ~ ("(" ~ Ws ~ term.+.separatedBy(Comma) ~ ")" ~ Ws).? ~> ((hd, as) => TptpTerm(hd, as.getOrElse(Seq()))) }
  private def variable = rule { capture(upper_word) ~ Ws ~> (FOLVar(_: String)) }
  private def arguments = rule { term.+.separatedBy(Comma) }

  private def include = rule { "include(" ~ Ws ~ file_name ~ formula_selection ~ ")." ~ Ws ~> (IncludeDirective(_, _)) }
  private def formula_selection = rule { ("," ~ Ws ~ "[" ~ name.*.separatedBy(Comma) ~ "]" ~ Ws).? }

  private def general_list: Rule1[Seq[Expr]] = rule { "[" ~ Ws ~ general_term.*.separatedBy(Comma) ~ "]" ~ Ws }
  private def general_terms = rule { general_term.+.separatedBy(Comma) }
  private def general_term: Rule1[Expr] = rule {
    general_data ~ (Colon ~ general_term).? ~> ((d, to) => to.fold(d)(t => GeneralColon(d, t))) |
      general_list ~> (GeneralList(_: Seq[Expr]))
  }
  private def general_data: Rule1[Expr] = rule {
    formula_data | general_function | atomic_word ~> (FOLConst(_)) |
      variable | (number ~> (FOLConst(_))) | (distinct_object ~> (FOLConst(_)))
  }
  private def formula_data: Rule1[Expr] = rule {
    ((capture("$" ~ ("thf" | "tff" | "fof" | "cnf")) ~ "(" ~ Ws ~ formula ~ ")" ~ Ws) |
      (capture("$fot") ~ "(" ~ Ws ~ term ~ ")" ~ Ws)) ~> (TptpTerm(_: String, _: Expr))
  }
  private def general_function = rule { atomic_word ~ "(" ~ Ws ~ general_terms ~ ")" ~ Ws ~> (TptpTerm(_, _)) }

  private def name: Rule1[String] = rule { atomic_word | integer }
  // We include defined words as atomic_word, since no prover can keep them apart...
  private def atomic_word = rule { (capture(lower_word) ~ Ws) | single_quoted }

  private def number = rule { rational | real | integer }

  private def file_name = rule { single_quoted }

  private def single_quoted = rule { '\'' ~ sg_char.* ~ '\'' ~ Ws ~> ((l: Seq[String]) => l.mkString) }

  private def distinct_object = rule { '"' ~ do_char.* ~ '"' ~ Ws ~> ((l: Seq[String]) => l.mkString) }

  private val alpha_numeric = UpperAlpha ++ LowerAlpha ++ Digit ++ CharPredicate("$_")
  private def upper_word = rule { UpperAlpha ~ alpha_numeric.* }
  private def lower_word = rule { (LowerAlpha ++ CharPredicate("$_")) ~ alpha_numeric.* }

  private def real = rule { capture(anyOf("+-").? ~ decimal ~ ('.' ~ Digit.*).? ~ (anyOf("Ee") ~ anyOf("+-").? ~ decimal).?) ~ Ws }
  private def rational = rule { capture(anyOf("+-").? ~ decimal ~ '/' ~ positive_decimal) ~ Ws }
  private def integer = rule { capture(anyOf("+-").? ~ decimal) ~ Ws }
  private def decimal = rule { '0' | positive_decimal }
  private def positive_decimal = rule { Digit19 ~ Digit.* }

  private val do_char_pred = CharPredicate(' ' to '!', '#' to '[', '(' to '[', ']' to '~')
  private def do_char = rule { capture(do_char_pred) | ("\\\\" ~ push("\\")) | ("\\\"" ~ push("\"")) }
  private val sg_char_pred = CharPredicate(' ' to '&', '(' to '[', ']' to '~')
  private def sg_char = rule { capture(sg_char_pred) | ("\\\\" ~ push("\\")) | ("\\'" ~ push("'")) }
}

object TptpTffImporter {

  /**
   * Parse a TPTP file, but do not resolve include directives.
   */
  private def parse(file: InputFile): TptpFile = {
    val input = file.read
    val parser = new TptpTffParser(input)
    parser.TPTP_file.run() match {
      case Failure(error: ParseError) =>
        throw new IllegalArgumentException(s"Parse error in ${file.fileName}:\n" +
          parser.formatError(error, new ErrorFormatter(showTraces = true)))
      case Failure(exception) => throw exception
      case Success(value)     => value
    }
  }

  /**
   * Load a TPTP file, but don't resolve includes.
   * @param file The input file to load.
   * @return The parsed file.
   */
  def loadWithoutIncludes(file: InputFile): TptpFile = parse(file)

  /**
   * Load a TPTP file and resolve includes.
   * @param file The input file to load.
   * @param resolver How to resolve included files.
   * @return The parsed file.
   */
  def loadWithIncludes(file: InputFile, resolver: String => TptpFile): TptpFile =
    resolveIncludes(parse(file), resolver)

  def loadWithIncludes(file: InputFile, relativeTo: Path): TptpFile =
    loadWithIncludes(file, fileName => parse(Path(fileName, relativeTo)))

  def loadWithIncludes(file: InputFile, relativeTo: FilePath): TptpFile =
    loadWithIncludes(file, Path(relativeTo, pwd))

  def loadWithIncludes(file: InputFile, relativeTo: String): TptpFile =
    loadWithIncludes(file, FilePath(relativeTo))

  def loadWithIncludes(file: InputFile): TptpFile =
    loadWithIncludes(file, pwd)

  def main(args: Array[String]): Unit =
    print(loadWithIncludes(FilePath(args.head)))

}
