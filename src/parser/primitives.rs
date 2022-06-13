use crate::parser::Parser;

pub fn parse_number<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut idx: usize = 0;

        while let Some(ch) = input.chars().nth(idx) {
            if ch.is_digit(10) {
                idx += 1;
            } else {
                break;
            }
        }

        Ok((&input[idx..], &input[..idx]))
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut matched_count: usize = 0;
        let mut chars = input.chars();

        if let Some(next) = chars.next() {
            if next.is_alphabetic() || next == '_' {
                matched_count += 1;
            } else {
                return Err(String::from(
                    "Identifier can not start with a non alphabetic character",
                ));
            }
        } else {
            return Err(String::from("UnexpectedEOF"));
        }

        while let Some(next) = chars.next() {
            if next.is_alphanumeric() || next == '_' {
                matched_count += 1;
            } else {
                break;
            }
        }

        Ok((&input[matched_count..], &input[..matched_count]))
    }
}

pub fn parse_literal<'a>(literal: &'a str) -> impl Parser<'a, &str> {
    move |input: &'a str| match input.get(0..literal.len()) {
        Some(substr) => {
            if substr == literal {
                Ok((&input[substr.len()..], substr))
            } else {
                Err(format!("expected {}, got {}", literal.to_string(), substr))
            }
        }
        None => Err("Unexpected EOF".to_string()),
    }
}

#[cfg(test)]
#[path = "primitives.test.rs"]
mod tests;
