//! Utilies for running in a build script.

use crate::file_text::FileText;
use crate::grammar::parse_tree as pt;
use crate::grammar::repr as r;
use crate::lexer::intern_token;
use crate::lr1;
use crate::message::builder::InlineBuilder;
use crate::message::{Content, Message};
use crate::normalize;
use crate::parser;
use crate::rust::RustWrite;
use crate::session::{ColorConfig, Session};
use crate::tls::Tls;
use crate::tok;
use crate::util::Sep;
use atty;
use itertools::Itertools;
use lalrpop_util::ParseError;
use term;
use tiny_keccak::{Hasher, Sha3};

use std::fs;
use std::io::{self, BufRead, Read, Write};
use std::path::{Path, PathBuf};
use std::process::exit;
use std::rc::Rc;

mod action;
mod fake_term;

use self::fake_term::FakeTerminal;

const LALRPOP_VERSION_HEADER: &str = concat!(
    "// auto-generated: \"",
    env!("CARGO_PKG_NAME"),
    " ",
    env!("CARGO_PKG_VERSION"),
    "\""
);

fn hash_file(file: &Path) -> io::Result<String> {
    let mut file = fs::File::open(&file)?;
    let mut file_bytes = Vec::new();
    file.read_to_end(&mut file_bytes).unwrap();

    let mut sha3 = Sha3::v256();
    sha3.update(&file_bytes);

    let mut output = [0u8; 32];
    sha3.finalize(&mut output);

    Ok(format!("// sha3: {:02x}", output.iter().format("")))
}

pub fn process_dir<P: AsRef<Path>>(session: Rc<Session>, root_dir: P) -> io::Result<()> {
    let lalrpop_files = lalrpop_files(root_dir)?;
    for lalrpop_file in lalrpop_files {
        process_file(session.clone(), lalrpop_file)?;
    }
    Ok(())
}

pub fn process_file<P: AsRef<Path>>(session: Rc<Session>, lalrpop_file: P) -> io::Result<()> {
    let lalrpop_file = lalrpop_file.as_ref();
    let rs_file = resolve_rs_file(&session, lalrpop_file)?;
    let report_file = resolve_report_file(&session, lalrpop_file)?;
    process_file_into(session, lalrpop_file, &rs_file, &report_file)
}

fn resolve_rs_file(session: &Session, lalrpop_file: &Path) -> io::Result<PathBuf> {
    gen_resolve_file(session, lalrpop_file, "rs")
}

fn resolve_report_file(session: &Session, lalrpop_file: &Path) -> io::Result<PathBuf> {
    gen_resolve_file(session, lalrpop_file, "report")
}

fn gen_resolve_file(session: &Session, lalrpop_file: &Path, ext: &str) -> io::Result<PathBuf> {
    let in_dir = if let Some(ref d) = session.in_dir {
        d.as_path()
    } else {
        Path::new(".")
    };
    let out_dir = if let Some(ref d) = session.out_dir {
        d.as_path()
    } else {
        in_dir
    };

    // If the lalrpop file is not in in_dir, the result is that the
    // .rs file is created in the same directory as the lalrpop file
    // for compatibility reasons
    Ok(out_dir
        .join(lalrpop_file.strip_prefix(&in_dir).unwrap_or(lalrpop_file))
        .with_extension(ext))
}

fn process_file_into(
    session: Rc<Session>,
    lalrpop_file: &Path,
    rs_file: &Path,
    report_file: &Path,
) -> io::Result<()> {
    session.emit_rerun_directive(lalrpop_file);
    if session.force_build || needs_rebuild(&lalrpop_file, &rs_file)? {
        log!(
            session,
            Informative,
            "processing file `{}`",
            lalrpop_file.to_string_lossy()
        );
        if let Some(parent) = rs_file.parent() {
            fs::create_dir_all(parent)?;
        }
        remove_old_file(&rs_file)?;

        // Load the LALRPOP source text for this file:
        let file_text = Rc::new(FileText::from_path(lalrpop_file.to_path_buf())?);

        // Store the session and file-text in TLS -- this is not
        // intended to be used in this high-level code, but it gives
        // easy access to this information pervasively in the
        // low-level LR(1) and grammar normalization code. This is
        // particularly useful for error-reporting.
        let _tls = Tls::install(session.clone(), file_text.clone());

        // Do the LALRPOP processing itself and write the resulting
        // buffer into a file. We use a buffer so that if LR(1)
        // generation fails at some point, we don't leave a partial
        // file behind.
        {
            let grammar = parse_and_normalize_grammar(&session, &file_text)?;
            let buffer = emit_recursive_ascent(&session, &grammar, &report_file)?;
            let mut output_file = fs::File::create(&rs_file)?;
            writeln!(output_file, "{}", LALRPOP_VERSION_HEADER)?;
            writeln!(output_file, "{}", hash_file(&lalrpop_file)?)?;
            output_file.write_all(&buffer)?;
        }
    }
    Ok(())
}

fn remove_old_file(rs_file: &Path) -> io::Result<()> {
    match fs::remove_file(rs_file) {
        Ok(()) => Ok(()),
        Err(e) => {
            // Unix reports NotFound, Windows PermissionDenied!
            match e.kind() {
                io::ErrorKind::NotFound | io::ErrorKind::PermissionDenied => Ok(()),
                _ => Err(e),
            }
        }
    }
}

fn needs_rebuild(lalrpop_file: &Path, rs_file: &Path) -> io::Result<bool> {
    match fs::File::open(&rs_file) {
        Ok(rs_file) => {
            let mut version_str = String::new();
            let mut hash_str = String::new();

            let mut f = io::BufReader::new(rs_file);

            f.read_line(&mut version_str)?;
            f.read_line(&mut hash_str)?;

            Ok(hash_str.trim() != hash_file(&lalrpop_file)?
                || version_str.trim() != LALRPOP_VERSION_HEADER)
        }
        Err(e) => match e.kind() {
            io::ErrorKind::NotFound => Ok(true),
            _ => Err(e),
        },
    }
}

fn lalrpop_files<P: AsRef<Path>>(root_dir: P) -> io::Result<Vec<PathBuf>> {
    let mut result = vec![];
    for entry in fs::read_dir(root_dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;

        let path = entry.path();

        if file_type.is_dir() {
            result.extend(lalrpop_files(&path)?);
        }

        let is_symlink_file = || -> io::Result<bool> {
            if !file_type.is_symlink() {
                Ok(false)
            } else {
                // Ensure all symlinks are resolved
                Ok(fs::metadata(&path)?.is_file())
            }
        };

        if (file_type.is_file() || is_symlink_file()?)
            && path.extension().is_some()
            && path.extension().unwrap() == "lalrpop"
        {
            result.push(path);
        }
    }
    Ok(result)
}

fn parse_and_normalize_grammar(session: &Session, file_text: &FileText) -> io::Result<r::Grammar> {
    let grammar = match parser::parse_grammar(file_text.text()) {
        Ok(grammar) => grammar,

        Err(ParseError::InvalidToken { location }) => {
            let ch = file_text.text()[location..].chars().next().unwrap();
            report_error(
                &file_text,
                pt::Span(location, location),
                &format!("invalid character `{}`", ch),
            );
        }

        Err(ParseError::UnrecognizedEOF { location, .. }) => {
            report_error(
                &file_text,
                pt::Span(location, location),
                "unexpected end of file",
            );
        }

        Err(ParseError::UnrecognizedToken {
            token: (lo, _, hi),
            expected,
        }) => {
            let _ = expected; // didn't implement this yet :)
            let text = &file_text.text()[lo..hi];
            report_error(
                &file_text,
                pt::Span(lo, hi),
                &format!("unexpected token: `{}`", text),
            );
        }

        Err(ParseError::ExtraToken { token: (lo, _, hi) }) => {
            let text = &file_text.text()[lo..hi];
            report_error(
                &file_text,
                pt::Span(lo, hi),
                &format!("extra token at end of input: `{}`", text),
            );
        }

        Err(ParseError::User { error }) => {
            let string = match error.code {
                tok::ErrorCode::UnrecognizedToken => "unrecognized token",
                tok::ErrorCode::UnterminatedEscape => "unterminated escape; missing '`'?",
                tok::ErrorCode::UnrecognizedEscape => {
                    "unrecognized escape; only \\n, \\r, \\t, \\\" and \\\\ are recognized"
                }
                tok::ErrorCode::UnterminatedStringLiteral => {
                    "unterminated string literal; missing `\"`?"
                }
                tok::ErrorCode::UnterminatedCharacterLiteral => {
                    "unterminated character literal; missing `'`?"
                }
                tok::ErrorCode::UnterminatedAttribute => "unterminated #! attribute; missing `]`?",
                tok::ErrorCode::ExpectedStringLiteral => "expected string literal; missing `\"`?",
                tok::ErrorCode::UnterminatedCode => {
                    "unterminated code block; perhaps a missing `;`, `)`, `]` or `}`?"
                }
            };

            report_error(
                &file_text,
                pt::Span(error.location, error.location + 1),
                string,
            )
        }
    };

    match normalize::normalize(session, grammar) {
        Ok(grammar) => Ok(grammar),
        Err(error) => report_error(&file_text, error.span, &error.message),
    }
}

fn report_error(file_text: &FileText, span: pt::Span, message: &str) -> ! {
    println!("{} error: {}", file_text.span_str(span), message);

    let out = io::stderr();
    let mut out = out.lock();
    file_text.highlight(span, &mut out).unwrap();

    exit(1);
}

fn report_messages(messages: Vec<Message>) -> term::Result<()> {
    let builder = InlineBuilder::new().begin_paragraphs();
    let builder = messages
        .into_iter()
        .fold(builder, |b, m| b.push(Box::new(m)));
    let content = builder.end().end();
    report_content(&*content)
}

fn report_content(content: &dyn Content) -> term::Result<()> {
    // FIXME -- can we query the size of the terminal somehow?
    let canvas = content.emit_to_canvas(80);

    let try_colors = match Tls::session().color_config {
        ColorConfig::Yes => true,
        ColorConfig::No => false,
        ColorConfig::IfTty => atty::is(atty::Stream::Stdout),
    };

    if try_colors {
        if let Some(mut stdout) = term::stdout() {
            return canvas.write_to(&mut *stdout);
        }
    }

    let stdout = io::stdout();
    let mut stdout = FakeTerminal::new(stdout.lock());
    canvas.write_to(&mut stdout)
}

fn emit_module_attributes<W: Write>(
    grammar: &r::Grammar,
    rust: &mut RustWrite<W>,
) -> io::Result<()> {
    rust.write_module_attributes(grammar)
}

fn emit_uses<W: Write>(grammar: &r::Grammar, rust: &mut RustWrite<W>) -> io::Result<()> {
    rust.write_uses("", grammar)
}

fn emit_recursive_ascent(
    session: &Session,
    grammar: &r::Grammar,
    report_file: &Path,
) -> io::Result<Vec<u8>> {
    let mut rust = RustWrite::new(vec![]);

    // We generate a module structure like this:
    //
    // ```
    // mod <output-file> {
    //     // For each public symbol:
    //     pub fn parse_XYZ();
    //     mod __XYZ { ... }
    //
    //     // For each bit of action code:
    //     <action-code>
    // }
    // ```
    //
    // Note that the action code goes in the outer module.  This is
    // intentional because it means that the foo.lalrpop file serves
    // as a module in the rust hierarchy, so if the action code
    // includes things like `super::` it will resolve in the natural
    // way.

    emit_module_attributes(grammar, &mut rust)?;
    emit_uses(grammar, &mut rust)?;

    if grammar.start_nonterminals.is_empty() {
        println!("Error: no public symbols declared in grammar");
        exit(1);
    }

    for (user_nt, start_nt) in &grammar.start_nonterminals {
        // We generate these, so there should always be exactly 1
        // production. Otherwise the LR(1) algorithm doesn't know
        // where to stop!
        assert_eq!(grammar.productions_for(start_nt).len(), 1);

        log!(
            session,
            Verbose,
            "Building states for public nonterminal `{}`",
            user_nt
        );

        let _lr1_tls = lr1::Lr1Tls::install(grammar.terminals.clone());

        let lr1result = lr1::build_states(&grammar, start_nt.clone());
        if session.emit_report {
            let mut output_report_file = fs::File::create(&report_file)?;
            lr1::generate_report(&mut output_report_file, &lr1result)?;
        }

        let states = match lr1result {
            Ok(states) => states,
            Err(error) => {
                let messages = lr1::report_error(&grammar, &error);
                let _ = report_messages(messages);
                exit(1) // FIXME -- propagate up instead of calling `exit`
            }
        };

        match grammar.algorithm.codegen {
            r::LrCodeGeneration::RecursiveAscent => lr1::codegen::ascent::compile(
                &grammar,
                user_nt.clone(),
                start_nt.clone(),
                &states,
                "super",
                &mut rust,
            )?,
            r::LrCodeGeneration::TableDriven => lr1::codegen::parse_table::compile(
                &grammar,
                user_nt.clone(),
                start_nt.clone(),
                &states,
                "super",
                &mut rust,
            )?,

            r::LrCodeGeneration::TestAll => lr1::codegen::test_all::compile(
                &grammar,
                user_nt.clone(),
                start_nt.clone(),
                &states,
                &mut rust,
            )?,
        }

        rust!(
            rust,
            "{}use self::{}parse{}::{}Parser;",
            grammar.nonterminals[&user_nt].visibility,
            grammar.prefix,
            start_nt,
            user_nt
        );
    }

    if let Some(ref intern_token) = grammar.intern_token {
        intern_token::compile(&grammar, intern_token, &mut rust)?;
        rust!(
            rust,
            "pub(crate) use self::{}lalrpop_util::lexer::Token;",
            grammar.prefix
        );
    }

    action::emit_action_code(grammar, &mut rust)?;

    emit_to_triple_trait(grammar, &mut rust)?;

    Ok(rust.into_inner())
}

fn write_where_clause<W: Write>(
    where_clauses: &[r::WhereClause],
    to_triple_where_clauses: &Sep<&Vec<r::WhereClause>>,
    rust: &mut RustWrite<W>,
) -> io::Result<()> {
    if !where_clauses.is_empty() {
        rust!(rust, "where {}", to_triple_where_clauses);
    }

    Ok(())
}

fn emit_to_triple_trait<W: Write>(grammar: &r::Grammar, rust: &mut RustWrite<W>) -> io::Result<()> {
    #![allow(non_snake_case)]

    let L = grammar.types.terminal_loc_type();
    let T = grammar.types.terminal_token_type();
    let E = grammar.types.error_type();

    let parse_error = format!(
        "{p}lalrpop_util::ParseError<{L}, {T}, {E}>",
        p = grammar.prefix,
        L = L,
        T = T,
        E = E,
    );

    let mut user_type_parameters = String::new();
    for type_parameter in &grammar.type_parameters {
        user_type_parameters.push_str(&format!("{}, ", type_parameter));
    }

    let where_clauses = &grammar.where_clauses;
    let to_triple_where_clauses = Sep(",", where_clauses);

    rust!(rust, "");
    rust!(
        rust,
        "pub trait {}ToTriple<{}>",
        grammar.prefix,
        user_type_parameters,
    );
    write_where_clause(where_clauses, &to_triple_where_clauses, rust)?;
    rust!(rust, "{{");
    rust!(
        rust,
        "fn to_triple(value: Self) -> Result<({L},{T},{L}), {parse_error}>;",
        L = L,
        T = T,
        parse_error = parse_error,
    );
    rust!(rust, "}}");

    rust!(rust, "");
    if grammar.types.opt_terminal_loc_type().is_some() {
        rust!(
            rust,
            "impl<{utp}> {p}ToTriple<{utp}> for ({L}, {T}, {L})",
            p = grammar.prefix,
            utp = user_type_parameters,
            L = L,
            T = T,
        );
        write_where_clause(where_clauses, &to_triple_where_clauses, rust)?;
        rust!(rust, "{{");
        rust!(
            rust,
            "fn to_triple(value: Self) -> Result<({L},{T},{L}), {parse_error}> {{",
            L = L,
            T = T,
            parse_error = parse_error,
        );
        rust!(rust, "Ok(value)");
        rust!(rust, "}}");
        rust!(rust, "}}");

        rust!(
            rust,
            "impl<{utp}> {p}ToTriple<{utp}> for Result<({L}, {T}, {L}), {E}>",
            utp = user_type_parameters,
            p = grammar.prefix,
            L = L,
            T = T,
            E = E,
        );
        write_where_clause(where_clauses, &to_triple_where_clauses, rust)?;
        rust!(rust, "{{");
        rust!(
            rust,
            "fn to_triple(value: Self) -> Result<({L},{T},{L}), {parse_error}> {{",
            L = L,
            T = T,
            parse_error = parse_error,
        );
        rust!(rust, "match value {{");
        rust!(rust, "Ok(v) => Ok(v),");
        rust!(
            rust,
            "Err(error) => Err({p}lalrpop_util::ParseError::User {{ error }}),",
            p = grammar.prefix
        );
        rust!(rust, "}}"); // match
        rust!(rust, "}}");
        rust!(rust, "}}");
    } else {
        rust!(
            rust,
            "impl<{utp}> {p}ToTriple<{utp}> for {T}",
            utp = user_type_parameters,
            p = grammar.prefix,
            T = T,
        );
        write_where_clause(where_clauses, &to_triple_where_clauses, rust)?;
        rust!(rust, "{{");
        rust!(
            rust,
            "fn to_triple(value: Self) -> Result<((),{T},()), {parse_error}> {{",
            T = T,
            parse_error = parse_error,
        );
        rust!(rust, "Ok(((), value, ()))");
        rust!(rust, "}}");
        rust!(rust, "}}");

        rust!(
            rust,
            "impl<{utp}> {p}ToTriple<{utp}> for Result<{T},{E}>",
            utp = user_type_parameters,
            p = grammar.prefix,
            T = T,
            E = E,
        );
        write_where_clause(where_clauses, &to_triple_where_clauses, rust)?;
        rust!(rust, "{{");
        rust!(
            rust,
            "fn to_triple(value: Self) -> Result<((),{T},()), {parse_error}> {{",
            T = T,
            parse_error = parse_error,
        );
        rust!(rust, "match value {{");
        rust!(rust, "Ok(v) => Ok(((), v, ())),");
        rust!(
            rust,
            "Err(error) => Err({p}lalrpop_util::ParseError::User {{ error }}),",
            p = grammar.prefix
        );
        rust!(rust, "}}"); // match
        rust!(rust, "}}"); // fn
        rust!(rust, "}}"); // impl
    }

    Ok(())
}
