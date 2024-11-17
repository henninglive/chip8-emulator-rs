use std::path::PathBuf;

use chip_8_core::Chip8Builder;
use clap::Parser;


/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Filepath to Chip-8 ROM file that will be executed
    #[clap(index = 1)]
    rom: PathBuf,

    /// Print debug information
    #[clap(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    // Load rom
    let rom = std::fs::read(args.rom).unwrap();

    let mut chip = Chip8Builder::new()
        .with_rom(&rom[..])
        .with_debug(true)
        .build();

    for _ in 0..25 {
        chip.step();
    }

    for y in 0..32 {
        for x in 0..64 {
            let i = y * 64 + x;
            let pxl = if chip.display[i] > 0 {'█'} else {' '};
            print!("{}", pxl);
        }
        println!();
    }
}