/*
    source.rs contains the text of every program the interpreter has run.

    Diagnostics name their source by id rather than by reference, so an error can
    outlive the program that produced it without tying a lifetime to the value it
    is stored in.
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u32);

pub struct SourceMap {
    texts: Vec<String>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self { texts: Vec::new() }
    }

    pub fn add(&mut self, text: String) -> SourceId {
        self.texts.push(text);
        SourceId((self.texts.len() - 1) as u32)
    }

    /// Empty for an id from a different map, and for the placeholder id used by
    /// errors that have no source position.
    pub fn text(&self, id: SourceId) -> &str {
        self.texts.get(id.0 as usize).map(String::as_str).unwrap_or("")
    }

    pub fn len(&self) -> usize {
        self.texts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.texts.is_empty()
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceId {
    /// For errors constructed without a program, such as a failed file read.
    pub const PLACEHOLDER: SourceId = SourceId(u32::MAX);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_round_trips_through_the_id() {
        let mut sources = SourceMap::new();
        let first = sources.add("print(1)".to_string());
        let second = sources.add("print(2)".to_string());

        assert_ne!(first, second);
        assert_eq!(sources.text(first), "print(1)");
        assert_eq!(sources.text(second), "print(2)");
        assert_eq!(sources.len(), 2);
    }

    #[test]
    fn an_unknown_id_reads_as_empty() {
        let sources = SourceMap::new();
        assert_eq!(sources.text(SourceId::PLACEHOLDER), "");
    }
}
