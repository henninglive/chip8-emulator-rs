mod chip8;
use chip8::*;

fn main() {
    // Load rom
    let rom = std::fs::read("roms/ibm-logo.ch8").unwrap();

    let mut chip = Chip8Builder::new()
        .with_rom(&rom[..])
        .with_debug(true)
        .build();

    loop {
        chip.step();
    }
}
