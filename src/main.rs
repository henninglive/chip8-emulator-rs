mod chip8;
use chip8::*;

fn main() {
    // Load rom
    let rom = std::fs::read("roms/ibm-logo.ch8").unwrap();

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
