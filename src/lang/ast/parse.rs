use crate::cli::GlobalConfig;

use super::*;

#[derive(Debug)]
pub enum ParseError {}

impl Prog {
    pub fn parse(string: &str, global_config: &GlobalConfig) -> Result<Prog, ParseError> {
        Ok(parser::prog(string, global_config).unwrap())
    }
}

type Id = String;

peg::parser! {
    grammar parser(global_config: &GlobalConfig) for str {
        rule whitespace() = quiet!{[' ' | '\n' | '\t']*}
        rule whitespace_except_newline() = quiet!{[' ' | '\t']*}
        rule _ = whitespace()

        rule alphabetic() -> String
            = s:([ 'a'..='z' | 'A'..='Z'])
                { String::from(s) }
        rule alphanumeric() -> String
            = s:([ 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'])
                { String::from(s) }

        rule char() -> char
            = !['{' | '}' | '\''] c:[c if c.is_ascii()]
                { c }
        rule chars() -> String
            = s:(char()+)
                { String::from_iter(s.to_vec().iter()) }

        //

        rule seplist<T>(tr: rule<T>, sepr: rule<()>) -> Vec<T>
            = v:(t:tr() ** (_ sepr() _) {t}) (_ sepr() _)?
                { v }

        //

        rule paren<T>(tr: rule<T>) -> T
            = "(" _ t:tr() _ ")" { t }
        rule brace<T>(tr: rule<T>) -> T
            = "{" _ t:tr() _ "}" { t }
        rule brack<T>(tr: rule<T>) -> T
            = "[" _ t:tr() _ "]" { t }
        rule angle<T>(tr: rule<T>) -> T
            = "<" _ t:tr() _ ">" { t }
        rule lined<T>(tr: rule<T>) -> T
            = "|" _ t:tr() _ "|" { t }

        //

        rule one_or_many<T>(tr: rule<T>, many: rule<Vec<T>>) -> Vec<T>
            = ts:many()
                { ts }
            / t:tr()
                { Vec::from([t]) }

        rule none_or_one_or_many<T>(tr: rule<T>, many: rule<Vec<T>>) -> Vec<T>
            = opt_t:one_or_many(tr, many)?
                { match opt_t {
                    Some(t) => t,
                    None => Vec::from([])
                } }

        //

        rule id() -> Id
            = s:(s1:alphabetic() s2:alphanumeric()* { format!("{s1}{}", s2.join("")) } )
                { Id::from(s) }

        rule bool_lit() -> bool
            = "true" { true }
            / "false" { false }
        rule num_lit() -> i32
            = n:$(['0'..='9']+) { n.parse().unwrap() }

        rule str_lit_reg() -> StrLitReg
            = e:brace(<exp()>)
                // insert implicit cast
                { StrLitReg::Exp(Exp::Call(Id::from("cast_str"), vec![e])) }
            / s:chars()
                // todo: fix this hardcoding
                { StrLitReg::Str(s.replace("\\n", "\n").replace("\\t", "\t")) }
        rule str_lit() -> Vec<StrLitReg>
            = "'" regs:(str_lit_reg()*) "'"
                { regs }

        rule handler_lit() -> Handler
            = "stdout"
                { Handler::Stdout }
            / "counts"
                { Handler::Counts }
            / "average"
                { Handler::Average }

        rule base() -> char
            = b:['A' | 'C' | 'G' | 'T'] { b }
        rule seq_lit() -> Vec<u8>
            = s:(base()+) { s.iter().map(|c| *c as u8).collect() }

        rule list_lit() -> Vec<Exp>
            = v:paren(<seplist(<exp()>, <",">)>) { v }
        rule struct_lit() -> HashMap<Id, Exp>
            // todo: break this up a bit
            = v:angle(<seplist(<i:id() _ ":" _ e:exp() {(i, e)}>,<",">)>)
                { v.into_iter().collect() }

        rule prefix_un_op() -> Id
            = "-"
                { Id::from("minus") }
        rule infix_bin_op() -> Id
            = "|>"
                { Id::from("out") }
            / "<|"
                { Id::from("send") }
            / ">"
                { Id::from("greater_than") }
            / ">="
                { Id::from("greater_than_or_equal_to") }
            / "<"
                { Id::from("less_than") }
            / "=="
                { Id::from("equal_to") }
            / "+"
                { Id::from("plus") }
            / "-"
                { Id::from("minus") }
            / "*"
                { Id::from("times") }

        rule describe_line() -> Exp
            = i:id() _ ":" _ e:exp()
                { Exp::StructLit(HashMap::from([
                    (Id::from("name"), Exp::StrLit(vec![StrLitReg::Str(i)])),
                    (Id::from("seq"), e),
                ]))}
            / i:id()
                { Exp::StructLit(HashMap::from([
                    (Id::from("name"), Exp::StrLit(vec![StrLitReg::Str(i.clone())])),
                    (Id::from("seq"), Exp::Id(i)),
                ])) }

        #[cache_left_rec]
        rule exp() -> Exp
            // describe sugar
            = e:exp() _ "as" _ "{" _ lines:(describe_line() ** (whitespace_except_newline() ['\n' | ';'] _)) (whitespace_except_newline() ['\n' | ';'] _)? _ "}"
                { Exp::Call(String::from("describe"), vec![e, Exp::ListLit(lines), Exp::NumLit(2), Exp::BoolLit(false)]) }

            // clumsy grouping sugar
            / "((" _ e:exp() _ "))"
                { e }
            // prefix unary operation sugar
            / o:prefix_un_op() _ e:exp()
                { Exp::Call(o, vec![e]) }
            // infix binary operation sugar
            / e1:exp() _ o:infix_bin_op() _ e2:exp()
                { Exp::Call(o, vec![e1, e2]) }
            // method sugar
            / e:exp() _ "." _ i:id() args:paren(<seplist(<e:exp()>,<",">)>)
                { Exp::Call(i, [e].into_iter().chain(args).collect()) }

            / b:bool_lit()
                { Exp::BoolLit(b) }
            / n:num_lit()
                { Exp::NumLit(n) }
            / s:str_lit()
                { Exp::StrLit(s) }
            / s:seq_lit()
                { Exp::SeqLit(s) }
            / h:handler_lit()
                { Exp::HandlerLit(h) }

            / l:list_lit()
                { Exp::ListLit(l) }
            / s:struct_lit()
                { Exp::StructLit(s) }

            / h:handler_lit()
                { Exp::HandlerLit(h) }

            / e:exp() "." i:id()
                { Exp::Add(Rc::new(e), i) }

            / i:id() args:paren(<seplist(<e:exp()>,<",">)>)
                { Exp::Call(i, args) }

            / i:id()
                { Exp::Id(i) }

        rule match_config() -> MatchConfig
            = ""
                { MatchConfig::new(global_config.error) }
        // rule match_mode() -> MatchMode
        //     = "*" { MatchMode::First }
        //     / ""  { MatchMode::All }

        // rule bind_mode() -> BindMode
        //     = "^" { BindMode::Best }
        //     / ""  { BindMode::All }
        // rule bind() -> Bind
        //     = ids:one_or_many(<id()>,<paren(<seplist(<id()>, <",">)>)>) _ "~" _ e:exp()
        //         { Bind { mode, ids, list: e } }

        rule bind() -> (Id, Exp)
            = id:id() _ "in" _ e:exp()
                { (id, e) }

        rule pat_reg() -> Reg
            = "_"
                { Reg::Hole }
            / a:lined(<e1:exp() _ ":" _ v:one_or_many(<pat_reg()>,<paren(<seplist(<pat_reg()>, <_()>)>)>) _ {(e1, v)}>)
                { Reg::Sized(a.0, a.1) }
            / a:lined(<e1:exp()>)
                { Reg::Sized(a, Vec::from([ Reg::Hole ])) }

            / i:id() _ ":" _ v:one_or_many(<pat_reg()>,<paren(<seplist(<pat_reg()>, <_()>)>)>)
                { Reg::Named(i, v) }

            / e:exp()
                // insert implicit cast
                { Reg::Exp(Exp::Call(Id::from("cast_seq"), vec![e])) }

        rule arm() -> Arm
            = regs:brack(<seplist(<pat_reg()>,<_()>)>) _ opt_binds:("for" _ l:seplist(<bind()>,<",">) {l})? _ "=>" _ stmt:stmt()
                { Arm {
                    binds: match opt_binds {
                        Some(v) => v,
                        None => vec![]
                    },
                    regs,
                    stmt
                } }

        rule matcher() -> Matcher
            = "if" _ read:exp() _ "is" _ arms:one_or_many(<arm()>,<(arm() ** (whitespace_except_newline() ['\n' | ';'] _))>)
                { Matcher { config: MatchConfig::new(global_config.error), read, arms } }

        rule stmt() -> Stmt
            = matcher:matcher()
                { Stmt::Match(matcher)}
            / id:id() _ ":" _ exp:exp()
                { Stmt::Let(id, exp) }
            / "if" _ exp:exp() _ "=>" _ stmt:stmt()
                { Stmt::If(exp, Rc::new(stmt)) }
            / exp:exp()
                { Stmt::Exp(exp) }

            // arm-on-its-own sugar
            / arm:arm()
                { Stmt::Match(Matcher::new_default(arm, global_config)) }
            / "{" _ stmts:(stmt() ** (whitespace_except_newline() ['\n' | ';'] _)) (whitespace_except_newline() ['\n' | ';'] _)? _ "}"
                { Stmt::Scope(stmts) }

        pub rule prog() -> Prog
            // parse a scope without expecting braces
            = _ stmts:(stmt() ** (whitespace_except_newline() ['\n' | ';'] _)) (whitespace_except_newline() ['\n' | ';'] _)? _
                { Prog { stmt: Stmt::Scope(stmts) } }
    }
}
