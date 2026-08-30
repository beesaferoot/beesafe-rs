/*
    repl.rs contains the parts of the interactive prompt that can be tested
    without a terminal: deciding when an entry is finished, colouring a line, and
    completing a word.
*/

use std::borrow::Cow;

use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Cmd, ConditionalEventHandler, Context, Event, EventContext, Helper, RepeatCount};

use crate::builtins::BUILTINS;
use crate::lexer::{Lexer, TType, Token};

const KEYWORDS: &[&str] = &[
    "declare", "define", "else", "false", "for", "if", "in", "init", "null", "return", "true",
    "while",
];

const RESET: &str = "\x1b[0m";
const KEYWORD: &str = "\x1b[35m";
const STRING: &str = "\x1b[32m";
const NUMBER: &str = "\x1b[36m";
const COMMENT: &str = "\x1b[90m";
const INVALID: &str = "\x1b[31m";

pub enum InputState {
    Complete,
    /// An unclosed bracket, which a later line can still close.
    Incomplete,
    /// Nothing a later line can fix.
    Invalid(String),
}

/// A string literal ends at the newline, so an unterminated one can never be
/// completed by typing more.
pub fn input_state(src: &str) -> InputState {
    let mut depth = 0i32;
    for token in tokens(src) {
        match token.ttype {
            TType::Lbrace | TType::Lparen | TType::Lbracket => depth += 1,
            TType::Rbrace | TType::Rparen | TType::Rbracket => depth -= 1,
            TType::Invalid if token.lexeme.starts_with(['"', '\'']) => {
                return InputState::Invalid("unterminated string literal".to_string())
            }
            _ => (),
        }
    }
    if depth > 0 {
        InputState::Incomplete
    } else {
        InputState::Complete
    }
}

pub struct ReplRenderer {
    names: Vec<String>,
}

impl ReplRenderer {
    pub fn new() -> Self {
        Self { names: Vec::new() }
    }

    pub fn set_names(&mut self, names: Vec<String>) {
        self.names = names;
    }

    fn candidates(&self, prefix: &str) -> Vec<String> {
        if prefix.is_empty() {
            return Vec::new();
        }
        let mut found: Vec<String> = KEYWORDS
            .iter()
            .map(|word| word.to_string())
            .chain(BUILTINS.iter().map(|builtin| builtin.name.to_string()))
            .chain(self.names.iter().cloned())
            .filter(|word| word.starts_with(prefix) && word != prefix)
            .collect();
        found.sort();
        found.dedup();
        found
    }
}

impl Default for ReplRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl Highlighter for ReplRenderer {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        if line.is_empty() {
            return Cow::Borrowed(line);
        }

        let mut out = String::with_capacity(line.len() * 2);
        let mut at = 0usize;
        for token in tokens(line) {
            // Newline and Eob carry no text, and Eob reports an offset inside the
            // previous token. The trailing gap covers whatever they stood for.
            if matches!(token.ttype, TType::Newline | TType::Eob) {
                continue;
            }
            let start = token.offset.max(0) as usize;
            if start >= line.len() || start < at {
                continue;
            }
            // Whatever the lexer skipped: whitespace, and comments, which it
            // consumes without producing a token.
            out.push_str(&paint_gap(&line[at..start]));

            let end = (start + span_len(line, &token)).min(line.len());
            let text = &line[start..end];
            match colour(&token.ttype) {
                Some(code) => {
                    out.push_str(code);
                    out.push_str(text);
                    out.push_str(RESET);
                }
                None => out.push_str(text),
            }
            at = end.max(at);
        }
        out.push_str(&paint_gap(&line[at.min(line.len())..]));
        Cow::Owned(out)
    }

    fn highlight_char(&self, line: &str, _pos: usize, forced: bool) -> bool {
        forced || !line.is_empty()
    }
}

impl Completer for ReplRenderer {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        let start = line[..pos]
            .char_indices()
            .rev()
            .find(|(_, c)| !(c.is_alphanumeric() || *c == '_'))
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0);
        Ok((start, self.candidates(&line[start..pos])))
    }
}

impl Hinter for ReplRenderer {
    type Hint = String;
}

impl Validator for ReplRenderer {}

impl Helper for ReplRenderer {}

/// Indent width used when Tab has no word to complete.
pub const INDENT: &str = "    ";

/// Tab completes a word, and indents when there is no word under the cursor.
pub struct TabHandler;

impl ConditionalEventHandler for TabHandler {
    fn handle(
        &self,
        _evt: &Event,
        n: RepeatCount,
        _positive: bool,
        ctx: &EventContext<'_>,
    ) -> Option<Cmd> {
        let before = &ctx.line()[..ctx.pos()];
        match before.chars().next_back() {
            Some(c) if c.is_alphanumeric() || c == '_' => None,
            _ => Some(Cmd::Insert(n, INDENT.to_string())),
        }
    }
}

fn tokens(src: &str) -> Vec<Token> {
    Lexer::new(src).parse_tokens()
}

fn colour(ttype: &TType) -> Option<&'static str> {
    match ttype {
        TType::Declare
        | TType::Define
        | TType::Else
        | TType::False
        | TType::For
        | TType::If
        | TType::In
        | TType::Init
        | TType::Null
        | TType::Return
        | TType::True
        | TType::While => Some(KEYWORD),
        TType::Literal => Some(STRING),
        TType::Num => Some(NUMBER),
        TType::Invalid => Some(INVALID),
        _ => None,
    }
}

/// A literal's lexeme is its unescaped contents, so its width in the source has
/// to be measured rather than taken from the token.
fn span_len(line: &str, token: &Token) -> usize {
    let start = token.offset.max(0) as usize;
    match token.ttype {
        TType::Literal | TType::Invalid if line[start..].starts_with(['"', '\'']) => {
            let quote = line[start..].chars().next().unwrap();
            let mut escaped = false;
            for (offset, c) in line[start + 1..].char_indices() {
                if escaped {
                    escaped = false;
                } else if c == '\\' {
                    escaped = true;
                } else if c == quote {
                    return offset + 2;
                }
            }
            line.len() - start
        }
        _ => token.lexeme.len(),
    }
}

fn paint_gap(gap: &str) -> String {
    match gap.find("//") {
        Some(at) => format!("{}{}{}{}", &gap[..at], COMMENT, &gap[at..], RESET),
        None => gap.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn state(src: &str) -> &'static str {
        match input_state(src) {
            InputState::Complete => "complete",
            InputState::Incomplete => "incomplete",
            InputState::Invalid(_) => "invalid",
        }
    }

    #[test]
    fn completeness_follows_the_brackets() {
        assert_eq!(state("print(1)"), "complete");
        assert_eq!(state("init a = [1, 2]"), "complete");
        assert_eq!(state("define f() {"), "incomplete");
        assert_eq!(state("define f() {\n  return 1\n}"), "complete");
        assert_eq!(state("print(1"), "incomplete");
        assert_eq!(state("init a = [1,"), "incomplete");
    }

    #[test]
    fn an_unterminated_string_can_never_complete() {
        assert_eq!(state("print(\"oops"), "invalid");
        assert_eq!(state("print(\"fine\")"), "complete");
    }

    #[test]
    fn highlighting_leaves_the_text_intact() {
        let renderer = ReplRenderer::new();
        for line in [
            "init x = 5",
            "define f() { return \"hi\" }",
            "print(1) // trailing",
            "for i in 0..3 { }",
        ] {
            let painted = renderer.highlight(line, line.len());
            let stripped: String = strip_ansi(&painted);
            assert_eq!(stripped, line, "highlighting altered {:?}", line);
        }
    }

    #[test]
    fn a_pasted_block_survives_highlighting() {
        // A bracketed paste arrives as one buffer with embedded newlines.
        let renderer = ReplRenderer::new();
        let block = "define f(n) {\n    if (n > 0) { // guard\n        return n\n    }\n    return 0\n}";
        assert_eq!(strip_ansi(&renderer.highlight(block, 0)), block);
        assert!(matches!(input_state(block), InputState::Complete));
    }

    #[test]
    fn keywords_strings_and_comments_are_coloured() {
        let renderer = ReplRenderer::new();
        let painted = renderer.highlight("init x = \"hi\" // note", 0);
        assert!(painted.contains(&format!("{}init{}", KEYWORD, RESET)));
        assert!(painted.contains(&format!("{}\"hi\"{}", STRING, RESET)));
        assert!(painted.contains(&format!("{}// note{}", COMMENT, RESET)));
    }

    #[test]
    fn an_empty_prefix_offers_nothing() {
        let mut renderer = ReplRenderer::new();
        renderer.set_names(vec!["thing".to_string()]);
        assert!(renderer.candidates("").is_empty());
    }

    #[test]
    fn completion_offers_keywords_builtins_and_globals() {
        let mut renderer = ReplRenderer::new();
        renderer.set_names(vec!["length_of".to_string(), "other".to_string()]);

        let found = renderer.candidates("le");
        assert!(found.contains(&"len".to_string()));
        assert!(found.contains(&"length_of".to_string()));
        assert!(!found.contains(&"other".to_string()));

        assert!(renderer.candidates("de").contains(&"define".to_string()));
    }

    fn strip_ansi(text: &str) -> String {
        let mut out = String::new();
        let mut chars = text.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\x1b' {
                while let Some(next) = chars.next() {
                    if next.is_ascii_alphabetic() {
                        break;
                    }
                }
                continue;
            }
            out.push(c);
        }
        out
    }
}
