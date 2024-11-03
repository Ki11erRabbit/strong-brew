use ariadne::{ColorGenerator, Label, ReportKind, Source};
use lalrpop_util::{lalrpop_mod, ParseError};

use crate::ast::outer;

use self::lexer::{LexerError, SpannedLexerError};

mod lexer;


lalrpop_mod!(grammar);

pub fn parse<'a>(name: &str, input: &'a str) -> Result<outer::File<'a>, ()> {
    let lexer = lexer::TokenLexer::new(input);
    match grammar::FileParser::new().parse(input, lexer) {
        Ok(file) => Ok(file),
        Err(e) => {
            match e {
                ParseError::User { error } => {
                    let SpannedLexerError { error, start, end } = error;
                    match error {
                        LexerError::ErrorCollection(errors) => {
                            for error in errors {
                                let mut colors = ColorGenerator::new();
                                let SpannedLexerError { error, start, end } = error;
                                match error {
                                    LexerError::InvalidIdentifier(x, y) => {
                                        ariadne::Report::build(ReportKind::Error, (name, start..end))
                                            .with_label(
                                                Label::new((name, start..end))
                                                    .with_message(format!("{}", error))
                                                    .with_color(colors.next()),
                                            )
                                            .with_label(
                                                Label::new((name, x..y))
                                                    .with_message("invalid identifier")
                                                    .with_color(colors.next()),
                                            )
                                            .with_note(format!(
                                                "Identifiers must start with a letter or an underscore and must not end with an dash or a dash and then a number."
                                                    ))
                                            .finish()
                                            .print((name, Source::from(input)))
                                            .unwrap();
                                    }
                                    _ => {
                                        ariadne::Report::build(ReportKind::Error, (name, start..end))
                                            .with_label(
                                                Label::new((name, start..end))
                                                    .with_message(format!("{}", error))
                                                    .with_color(colors.next()),
                                            )
                                            .finish()
                                            .print((name, Source::from(input)))
                                            .unwrap();

                                    }
                                }
                            }
                            

                        }
                        e => eprintln!("{}", e),
                    }
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    let (start, token, end) = token;
                    let mut colors = ColorGenerator::new();
                    let oneof = expected.iter().map(|x| format!("{}", x)).reduce(|x, y| format!("{}, {}", x, y)).unwrap();
                    ariadne::Report::build(ReportKind::Error, (name, start..end))
                        .with_label(
                            Label::new((name, start..end))
                                .with_message(format!("Unrecognized token: {}", token))
                                .with_color(colors.next()),
                        )
                        .with_note(format!("Expected one of: {}", oneof))
                        .finish()
                        .print((name, Source::from(input)))
                        .unwrap();
                }
                _ => eprintln!("{}", e),
            }
            Err(())
        }
    }
}

