use std::{error::Error, fmt, str::FromStr};

use bytemuck::{Pod, Zeroable};

pub const DEFAULT_BACKGROUND_COLOR: Chip8Color = Chip8Color::new(0, 0,0);
pub const DEFAULT_FOREGROUND_COLOR: Chip8Color = Chip8Color::new(255, 255, 255);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Pod, Zeroable)]
#[repr(C, packed)]
pub struct Chip8Color {
    padding: u8,
    pub b: u8,
    pub g: u8,
    pub r: u8,
}

impl Chip8Color {
    pub const fn new(r: u8, g: u8, b: u8) -> Chip8Color{
        Chip8Color { r, g, b, padding: 0 }
    }
}

impl FromStr for Chip8Color {
    type Err = Chip8ColorParseError;

    // Required method
    fn from_str(mut s: &str) -> Result<Chip8Color, Chip8ColorParseError> {
        if s.starts_with("0x") {
            s = &s[2..];
        }

        if s.len() != 6 {
            return Err(Chip8ColorParseError);
        }

        if s.chars().any(|c| !c.is_ascii_hexdigit()) {
            return Err(Chip8ColorParseError);
        }

        let r = u8::from_str_radix(&s[0..2], 16)
            .map_err(|_| Chip8ColorParseError)?;

        let g = u8::from_str_radix(&s[2..4], 16)
            .map_err(|_| Chip8ColorParseError)?;

        let b = u8::from_str_radix(&s[4..6], 16)
            .map_err(|_| Chip8ColorParseError)?;

        Ok(Chip8Color::new(r, g, b))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Chip8ColorParseError;

impl fmt::Display for Chip8ColorParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "failed to parse hex color:".fmt(f)
    }
}

impl Error for Chip8ColorParseError {
    fn description(&self) -> &str {
        "failed to parse hex color"
    }
}
