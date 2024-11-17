// CHIP-8 emulator 
//
// Useful links:
// * [Guide to making a CHIP-8 emulator](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/)
// * [Building a CHIP-8 Emulator](https://austinmorlan.com/posts/chip8_emulator/)
// * [high-level assembler for the Chip8 virtual machine](https://github.com/JohnEarnest/Octo/blob/gh-pages/js/emulator.js)
// 

use rand::{rngs::StdRng, RngCore, SeedableRng};

const MEMORY_SIZE: usize = 0x1000;
pub const SCREEN_WITDH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;

const PIXEL_ON: u32 = 0xFFFFFFFF;
const PIXEL_OFF: u32 = 0;

pub static DEFAULT_FONT: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Chip8Mode {
    COSMAC_VIP,
    CHIP_48,
    SUPER_CHIP,
}

//TODO: Bitflags
#[derive(Clone, Copy, Debug)]
struct Chip8Flags {
    /// Bitwise shift (8XY6 and 8XYE) quirk: if true VY is copied into VX before shifting (COSMAC VIP)
    quirk_shift: bool,
    /// Jump with offset (BNNN/BXNN) quirk: Jump to the address XNN plus the value in the register VX (CHIP-48 and SUPER-CHIP),
    /// instead of the address NNN plus the value in the register V0 (COSMAC VIP)
    quirk_jump_with_offset: bool,
    /// Store and load memory (FX55/FX65) quirk: Increment I register while loading and storing registers ( COSMAC VIP)
    quirk_inc_idx_load_store: bool,
    /// Debug mode: print debug info
    debug: bool,
}

#[derive(Debug)]
pub struct Chip8Builder {
    /// ROM
    rom: Option<Vec<u8>>,
    /// Font sprite
    font: Option<Vec<u8>>,
    // PRNG Seed
    rng_seed: Option<u64>,
    /// Flags
    flags: Chip8Flags,
}

pub struct Chip8 {
    /// General purpose registers
    regs: [u8; 16],
    /// Index register
    index: u16,
    /// Program counter
    pc: u16,
    /// Call stack
    stack: [u16; 16],
    /// Stack pointer
    sp: u8,
    /// Delay Timer
    delay_timer: u8,
    /// Sound Timer
    sound_timer: u8,
    /// Memory
    memory: Vec<u8>,
    /// Display: 64x32 pixels 1 bit monochrome stored as RBG24 for SDL compatibility
    display: Vec<u32>,
    /// Flags
    flags: Chip8Flags,
    /// PRNG Generator
    rng: StdRng,
}

impl<'a> Chip8Builder {
    pub fn new() -> Chip8Builder {
        Chip8Builder {
            rom: None,
            font: None,
            rng_seed: None,
            flags: Chip8Flags {
                quirk_shift: false,
                quirk_jump_with_offset: false,
                quirk_inc_idx_load_store: false,
                debug: false,
            },
        }
    }

    pub fn with_rom(mut self, rom: Vec<u8>) -> Self {
        assert!(rom.len() >= 2, "ROM size must be at least two bytes");
        assert!(rom.len() < 3584, "ROM size must be less then 3584 bytes");
        self.rom = Some(rom);
        self
    }

    pub fn with_font(mut self, font: Vec<u8>) -> Self {
        assert_eq!(font.len(), 80, "Font sprite must 80 bytes");
        self.font = Some(font);
        self
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.flags.debug = debug;
        self
    }

    pub fn with_rng_seed<'b>(mut self, seed: u64) -> Self {
        self.rng_seed = Some(seed);
        self
    }

    pub fn with_mode<'b>(mut self, mode: Chip8Mode) -> Self {
        match mode {
            Chip8Mode::COSMAC_VIP => {
                self.flags.quirk_shift = true;
            }
            _ => {}
        }
        self
    }

    pub fn build(&self) -> Chip8 {
        // Create memory
        let mut memory = vec![0u8; MEMORY_SIZE];
        
        // Copy font to memory
        let font = match &self.font {
            Some(font) => &font[..],
            None => &DEFAULT_FONT[..]
        };
        (&mut memory[0x050..0x0A0]).copy_from_slice(font);

        // Copy rom to memory
        let rom = self.rom.as_ref()
            .expect("A ROM file must be provided");
        (&mut memory[0x200..(0x200 + rom.len())]).copy_from_slice(&rom[..]);

        // Create display buffer
        let display = vec![0u32; SCREEN_WITDH * SCREEN_HEIGHT];

        // Pseudo random number generator
        let rng = match self.rng_seed {
            Some(seed) => StdRng::seed_from_u64(seed),
            None => StdRng::from_entropy(),
        };

        Chip8 {
            regs: [0u8; 16],
            index: 0,
            pc: 0x200,
            stack: [0u16; 16],
            sp: 0,
            delay_timer: 0,
            sound_timer: 0,
            memory: memory,
            display: display,
            flags: self.flags,
            rng: rng,
        }   
    }
}

impl Chip8 {

    pub fn display(&self) -> &[u8] {
        bytemuck::cast_slice(&self.display[..])
    }

    pub fn step(&mut self) {
        // Instruction
        let inst = self.read_u16_be(self.pc);

        // Instruction split into nibbels(4bits) 1-4
        let n1 = (0xf0 & inst[0]) >> 4;
        let n2 = 0x0f & inst[0];
        let n3 = (0xf0 & inst[1]) >> 4;
        let n4 = 0x0f & inst[1];

        match (n1, n2, n3, n4) {
            // 00E0: Clear screen
            (0x0, 0x0, 0xe, 0x0) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: CLEAR", inst[0], inst[1]);
                }

                self.clear_screen(false);
                self.pc += 2;
            }
            // 00EE: Return subroutine from stack
            (0x0, 0x0, 0xe, 0xe) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: POP", inst[0], inst[1]);
                }

                // Pop program counter from stack
                assert!(self.sp > 0, "Return called when stack is empty");
                self.sp -= 1;
                let prev = self.stack[self.sp as usize];
                assert!(prev % 2 == 0, "Program counter must be 2 byte aligned");
                self.pc = prev;
            }
            // 1NNN: Jump to memory location NNN
            (0x1, _, _, _) => {
                let mut addr: [u8; 2] = inst;
                addr[0] &= 0x0f;

                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: JMP 0x{:02x}{:02x}",
                        inst[0], inst[1], addr[0], addr[1]
                    );
                }

                // Set program counter to addr in Instruction
                let pc = u16::from_be_bytes(addr);
                assert!(pc % 2 == 0, "Program counter must be 2 byte aligned");
                self.pc = pc;
            }
            // 2NNN: Call subroutine at memory location NNN
            (0x2, _, _, _) => {
                let mut addr = inst;
                addr[0] &= 0x0f;

                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: CALL 0x{:02x}{:02x}",
                        inst[0], inst[1], addr[0], addr[1]
                    );
                }

                // Push program counter to stack
                assert!(self.sp <= 16, "Stack overflow");
                self.stack[self.sp as usize] = self.pc;
                self.sp += 1;

                // Jump to subroutine at addr in instruction
                self.pc = u16::from_be_bytes(addr);
            }
            // 3XNN: Skip next instruction if VX == NN
            (0x3, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SKIP V{:x}=={:02x}",
                        inst[0], inst[1], n2, inst[1]
                    );
                }

                if self.regs[n2 as usize] == inst[1] {
                    self.pc += 4;
                } else {
                    self.pc += 2;
                }
            }
            // 3XNN: Skip next instruction if VX != NN
            (0x4, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SKIP V{:x}!={:02x}",
                        inst[0], inst[1], n2, inst[1]
                    );
                }

                if self.regs[n2 as usize] != inst[1] {
                    self.pc += 4;
                } else {
                    self.pc += 2;
                }
            }
            // 5XY0: Skip next instruction if VX == VY
            (0x5, _, _, 0x0) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SKIP V{:x}==V{:x}",
                        inst[0], inst[1], n2, n3
                    );
                }

                if self.regs[n2 as usize] == self.regs[n3 as usize] {
                    self.pc += 4;
                } else {
                    self.pc += 2;
                }
            }
            // 6XNN: Set register VX to the value NN.
            (0x6, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SET V{:x} {:02x}",
                        inst[0], inst[1], n2, inst[1]
                    );
                }

                self.regs[n2 as usize] = inst[1];
                self.pc += 2;
            }
            // 7XNN: Add the value NN to VX.
            (0x7, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: ADD V{:x} {:02x}",
                        inst[0], inst[1], n2, inst[1]
                    );
                }

                let res = self.regs[n2 as usize].overflowing_add(inst[1]);
                self.regs[n2 as usize] = res.0;
                self.pc += 2;
            }
            // 8XY0: Set register VX to the value of VY.
            (0x8, _, _, 0x0) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: SET V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }

                self.regs[n2 as usize] = self.regs[n3 as usize];
                self.pc += 2;
            }
            // 8XY1: Binary OR register VX and register VY and store result in VX
            (0x8, _, _, 0x1) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: OR V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }
                self.regs[n2 as usize] |= self.regs[n3 as usize];
                self.pc += 2;
            }
            // 8XY2: Binary AND register VX and register VY and store result in VX
            (0x8, _, _, 0x2) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: AND V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }
                self.regs[n2 as usize] &= self.regs[n3 as usize];
                self.pc += 2;
            }
            // 8XY3: Binary XOR register VX and register VY and store result in VX
            (0x8, _, _, 0x3) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: XOR V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }
                self.regs[n2 as usize] ^= self.regs[n3 as usize];
                self.pc += 2;
            }
            // 8XY4: Add register VX and register VY and store result in VX
            (0x8, _, _, 0x4) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: ADD V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }

                let res = self.regs[n2 as usize].overflowing_add(self.regs[n3 as usize]);
                self.regs[n2 as usize] = res.0;
                self.regs[0xF] = res.1 as u8;
                self.pc += 2;
            }
            // 8XY5: Subtract, sets VX to the result of VX - VY
            (0x8, _, _, 0x5) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: SUB V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }

                let res = self.regs[n2 as usize].overflowing_sub(self.regs[n3 as usize]);
                self.regs[n2 as usize] = res.0;
                self.regs[0xF] = (!res.1) as u8;
                self.pc += 2;
            }
            // 8XY6: Shift VX to the right by 1 bit, optionaly copy VY to VX before shifting
            (0x8, _, _, 0x6) => {
                if self.flags.debug {
                    if self.flags.quirk_shift {
                        println!("0x{:02x}{:02x}: SHR V{:x} V{:x}", inst[0], inst[1], n2, n3);
                    } else {
                        println!("0x{:02x}{:02x}: SHR V{:x}", inst[0], inst[1], n2);
                    }
                }

                if self.flags.quirk_shift {
                    self.regs[n2 as usize] = self.regs[n3 as usize]
                }

                self.regs[0xF] = (self.regs[n2 as usize] & 0x80) >> 7;
                self.regs[n2 as usize] <<= 1;
                self.pc += 2;
            }
            // 8XY7: Subtract, sets VX to the result of VY - VX
            (0x8, _, _, 0x7) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: SUB2 V{:x} V{:x}", inst[0], inst[1], n2, n3);
                }

                let res = self.regs[n3 as usize].overflowing_sub(self.regs[n2 as usize]);
                self.regs[n2 as usize] = res.0;
                self.regs[0xF] = (!res.1) as u8;
                self.pc += 2;
            }
            // 8XYE: Shift VX to the left by 1bit, optionaly copy VY to VX before shifting
            (0x8, _, _, 0xE) => {
                if self.flags.debug {
                    if self.flags.quirk_shift {
                        println!("0x{:02x}{:02x}: SHL V{:x} V{:x}", inst[0], inst[1], n2, n3);
                    } else {
                        println!("0x{:02x}{:02x}: SHL V{:x}", inst[0], inst[1], n2);
                    }
                }

                if self.flags.quirk_shift {
                    self.regs[n2 as usize] = self.regs[n3 as usize]
                }

                self.regs[0xF] = self.regs[n2 as usize] & 0x01;
                self.regs[n2 as usize] >>= 1;
                self.pc += 2;
            }
            // 5XY0: Skip next instruction if VX != VY
            (0x9, _, _, 0x0) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SKIP V{:x}==V{:x}",
                        inst[0], inst[1], n2, n3
                    );
                }

                if self.regs[n2 as usize] != self.regs[n3 as usize] {
                    self.pc += 4;
                } else {
                    self.pc += 2;
                }
            }
            // ANNN: Set index register I to the value NNN
            (0xA, _, _, _) => {
                let mut idx: [u8; 2] = inst;
                idx[0] &= 0x0f;

                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: SET I 0x{:02x}{:02x}",
                        inst[0], inst[1], idx[0], idx[1]
                    );
                }

                self.index = u16::from_be_bytes(idx);
                self.pc += 2;
            }
            // BNNN/BXNN: Jump with offset
            // Jump to address NNN plus the value in the register V0 (COSMAC VIP),
            // (optionally) ump to the address XNN plus the value in the register VX
            (0xB, _, _, _) => {
                let mut addr: [u8; 2] = inst;
                addr[0] &= 0x0f;

                if self.flags.debug {
                    if self.flags.quirk_jump_with_offset {
                        println!(
                            "0x{:02x}{:02x}: JMP 0x{:02x}{:02x} V0",
                            inst[0], inst[1], addr[0], addr[1]
                        );
                    } else {
                        println!(
                            "0x{:02x}{:02x}: JMP 0x{:02x}{:02x} V{:x}",
                            inst[0], inst[1], addr[0], addr[1], n2
                        );
                    }
                }

                let offset = if self.flags.quirk_jump_with_offset {
                    self.regs[n2 as usize]
                } else {
                    self.regs[0]
                };

                // Set program counter to addr in Instruction
                let pc = u16::from_be_bytes(addr) + offset as u16;
                assert!(pc % 2 == 0, "Program counter must be 2 byte aligned");
                self.pc = pc;
            }
            // CXNN: Random - generates a random number and AND it with the value NN, and puts the result in VX
            (0xC, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: RNG V{:x} 0x{:02x}",
                        inst[0], inst[1], n2, inst[1]
                    );
                }

                let n = self.rng.next_u32() as u8;
                self.regs[n2 as usize] = n & inst[1];
                self.pc += 2;
            }
            // DXYN: Display - draw an N pixels tall sprite from the memory location that the I index register
            // is holding to the screen, at the horizontal X coordinate in VX and the Y coordinate in VY
            (0xD, _, _, _) => {
                if self.flags.debug {
                    println!(
                        "0x{:02x}{:02x}: Display V{:x} V{:x} {:x}",
                        inst[0], inst[1], n2, n3, n4
                    );
                }

                // Origin where we start to draw
                let oy: u8 = self.regs[n3 as usize] % SCREEN_HEIGHT as u8;
                let ox: u8 = self.regs[n2 as usize] % SCREEN_WITDH as u8;

                // Reset carry flag
                self.regs[0xF] = 0;

                // Draw N rows 
                for row in 0..n4 {
                    let y =  oy + row;
                    if y >= SCREEN_HEIGHT as u8 {
                        break;
                    } 

                    // Read row(8-bit) of sprite data from memory
                    let addr = self.index + row as u16;
                    let data = self.read_u8(addr);

                    for column in 0..8 {
                        let x = ox + column;
                        if x >= SCREEN_WITDH as u8 {
                            break;
                        } 

                        // bitmask git single bit
                        let mask = 1 << (7 - column);

                        // If sprite bit for column is on
                        if (data & mask) > 0 {
                            // if pixel is on, turn it of and set carry flag
                            if self.get_pixel_xy(x, y ) {
                                self.set_pixel_xy(x, y, false);
                                self.regs[0xF] = 1;
                            } else {
                                // else turn it on 
                                self.set_pixel_xy(x, y, true);
                            }
                        }
                    }
                }

                self.pc += 2;
            }
            // FX55: Store - Store of each register from V0-VX at memory addresses starting at I until I + X
            (0xF, _, 0x5, 0x5) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: STORE V{:x}", inst[0], inst[1], n2,);
                }

                for i in 0..=n2 {
                    let addr = self.index + i as u16;
                    self.write_u8(addr, self.regs[i as usize]);
                }

                if self.flags.quirk_inc_idx_load_store {
                    self.index += n2 as u16;
                }

                self.pc += 2;
            }
            // FX65: Load - Load of each register from V0-VX from memory addresses starting at I until I + X
            (0xF, _, 0x6, 0x5) => {
                if self.flags.debug {
                    println!("0x{:02x}{:02x}: LOAD V{:x}", inst[0], inst[1], n2,);
                }

                for i in 0..=n2 {
                    let addr: u16 = self.index + i as u16;
                    self.regs[i as usize] = self.read_u8(addr);
                }

                if self.flags.quirk_inc_idx_load_store {
                    self.index += n2 as u16;
                }

                self.pc += 2;
            }
            _ => panic!(
                "Invalid instruction 0x{:02x}{:02x} at 0x{:04x}",
                inst[0], inst[1], self.pc
            ),
        }
    }

    pub fn step_timer(&mut self) {
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }

        if self.sound_timer > 0 {
            self.sound_timer -= 1;
        }
    }

    fn read_u8(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn read_u16_be(&self, addr: u16) -> [u8; 2] {
        assert!(addr % 2 == 0, "Address must 2 byte aligned");
        let mut buffer = [0u8; 2];
        buffer.copy_from_slice(&&self.memory[(addr as usize)..(addr as usize + 2)]);
        buffer
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn write_u16_be(&mut self, addr: u16, data: [u8; 2]) {
        assert!(addr % 2 == 0, "Address must 2 byte aligned");
        (&mut self.memory[(addr as usize)..(addr as usize + 2)]).copy_from_slice(&data[..]);
    }

    fn get_pixel_xy(&self, x: u8, y: u8) -> bool {
        let idx = (y as usize) * SCREEN_WITDH + (x as usize);
        self.get_pixel_idx(idx)
    }

    fn get_pixel_idx(&self, idx: usize) -> bool {
        self.display[idx] > 0
    }

    fn set_pixel_xy(&mut self, x: u8, y: u8, on_off: bool) {
        let idx = (y as usize) * SCREEN_WITDH + (x as usize);
        self.set_pixel_idx(idx, on_off);
    }

    fn set_pixel_idx(&mut self, idx: usize, on_off: bool) {
        let pxl = if on_off { PIXEL_ON } else { PIXEL_OFF };
        self.display[idx] = pxl;
    }

    fn clear_screen(&mut self, on_off: bool) {
        let pxl = if on_off { PIXEL_ON } else { PIXEL_OFF };
        self.display.iter_mut().for_each(|p| *p = pxl);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_stack(chip: &Chip8, expected: &[u16]) {
        assert_eq!(chip.sp as usize, expected.len(), "Unexpected stack size");
        assert_eq!(
            &chip.stack[0..expected.len()],
            expected,
            "Unexpected stack content"
        );
    }

    fn assert_regs(chip: &Chip8, non_zero_regs: &[(u8, u8)]) {
        for reg in 0..16u8 {
            let expected = non_zero_regs
                .iter()
                .find(|v| v.0 == reg)
                .map(|v| v.1)
                .unwrap_or(0);
            assert_eq!(
                chip.regs[reg as usize], expected,
                "Expected register 0x{:x} to contain 0x{:02x}",
                reg, expected
            );
        }
    }

    fn setup(rom: &[u8]) -> Chip8 {
        Chip8Builder::new()
            .with_rom(rom.to_vec())
            .with_rng_seed(310349960114u64)
            .with_debug(true)
            .build()
    }

    #[test]
    fn test_timers_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x1F, 0xFF]);
        chip.delay_timer = 10;
        chip.sound_timer = 18;

        // Act: Step timers
        chip.step_timer();

        // Assert: CPU state
        assert_eq!(chip.delay_timer, 9);
        assert_eq!(chip.sound_timer, 17);
    }

    #[test]
    fn test_timers_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x1F, 0xFF]);

        // Act: Step timers
        chip.step_timer();

        // Assert: CPU state
        assert_eq!(chip.delay_timer, 0);
        assert_eq!(chip.sound_timer, 0);
    }

    #[test]
    fn test_clear_screen() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x00, 0xe0]);

        // Enable all pixels
        chip.clear_screen(true);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);

        // Assert: Screen clear
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WITDH {
                assert!(
                    !chip.get_pixel_xy(x as u8, y as u8),
                    "pxl at x:{} and y:{} should be off",
                    x,
                    y
                );
            }
        }
    }

    #[test]
    fn test_jump_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x1F, 0xF0]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU state
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0xFF0);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_jump_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x1A, 0xB0]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0xAB0);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_return_subroutine() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x00, 0xEE]);

        // Setup stack
        chip.sp = 1;
        chip.stack[0] = 0xabba;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0xabba);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_call_subroutine() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x2A, 0xBA]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0ABA);
        assert_stack(&chip, &[0x200]);
    }

    #[test]
    fn test_skip_eq_immediate_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x30, 0xAA]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_eq_immediate_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x30, 0xAA]);
        chip.regs[0x0] = 0xAA;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x0, 0xAA)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0204);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_eq_reg_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x5A, 0xB0]);
        chip.regs[0xA] = 0xDD;
        chip.regs[0xB] = 0xDD;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0xA, 0xDD), (0xB, 0xDD)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0204);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_eq_reg_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x5A, 0xB0]);
        chip.regs[0xA] = 0xCC;
        chip.regs[0xB] = 0xDD;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0xA, 0xCC), (0xB, 0xDD)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_neq_immediate_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x41, 0xAA]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0204);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_neq_immediate_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x41, 0xAA]);
        chip.regs[0x1] = 0xAA;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xAA)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_set_immediate() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x64, 0xAB]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x4, 0xAB)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_add_immediate_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x70, 0xAB]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x0, 0xAB)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_add_immediate_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x79, 0xBA]);
        chip.regs[0x9] = 0xAB;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x9, 0x65)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_binary_or() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x21]);
        chip.regs[0x1] = 0xF0;
        chip.regs[0x2] = 0x0F;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xFF), (0x2, 0x0F)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_binary_and() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x82, 0x32]);
        chip.regs[0x2] = 0xF1;
        chip.regs[0x3] = 0x0F;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x2, 0x01), (0x3, 0x0F)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_binary_xor() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x83, 0x43]);
        chip.regs[0x3] = 0xAB;
        chip.regs[0x4] = 0xBA;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x3, 0x11), (0x4, 0xBA)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_add_reg_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x84, 0x54]);
        chip.regs[0x4] = 0x8B;
        chip.regs[0x5] = 0x4F;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x4, 0xDA), (0x5, 0x4F)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_add_reg_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x84, 0x54]);
        chip.regs[0x4] = 0xFF;
        chip.regs[0x5] = 0x02;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x4, 0x01), (0x5, 0x02), (0xF, 0x01)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_xy_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x25]);
        chip.regs[0x1] = 0xFF;
        chip.regs[0x2] = 0xEE;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x11), (0x2, 0xEE), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_xy_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x25]);
        chip.regs[0x1] = 0xFF;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x0), (0x2, 0xFF), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_xy_3() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x25]);
        chip.regs[0x1] = 0xEE;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xEF), (0x2, 0xFF), (0xF, 0x0)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shr_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x26]);
        chip.regs[0x1] = 0x2;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x4)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shr_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x26]);
        chip.regs[0x1] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xFE), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shr_3() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x26]);
        chip.flags.quirk_shift = true;
        chip.regs[0x1] = 0x44;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xFE), (0x2, 0xFF), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_yx_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x27]);
        chip.regs[0x1] = 0xEE;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x11), (0x2, 0xFF), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_yx_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x27]);
        chip.regs[0x1] = 0xFF;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x0), (0x2, 0xFF), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_sub_reg_yx_3() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x27]);
        chip.regs[0x1] = 0xFF;
        chip.regs[0x2] = 0xEE;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0xEF), (0x2, 0xEE), (0xF, 0x0)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shl_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x2E]);
        chip.regs[0x1] = 0x2;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shl_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x2E]);
        chip.regs[0x1] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x7F), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_shl_3() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x81, 0x2E]);
        chip.flags.quirk_shift = true;
        chip.regs[0x1] = 0x44;
        chip.regs[0x2] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x1, 0x7F), (0x2, 0xFF), (0xF, 0x1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_neq_reg_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x9A, 0xB0]);
        chip.regs[0xA] = 0xDD;
        chip.regs[0xB] = 0xDD;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0xA, 0xDD), (0xB, 0xDD)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_skip_neq_reg_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0x9A, 0xB0]);
        chip.regs[0xA] = 0xCC;
        chip.regs[0xB] = 0xDD;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0xA, 0xCC), (0xB, 0xDD)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x0204);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_set_index_immediate() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xAB, 0xCD]);

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[]);
        assert_eq!(chip.index, 0x0BCD);
        assert_eq!(chip.pc, 0x0202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_jmp_with_offset_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xB4, 0x00]);
        chip.regs[0x0] = 0xF0;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x0, 0xF0)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x04F0);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_jmp_with_offset_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xB4, 0x00]);
        chip.flags.quirk_jump_with_offset = true;
        chip.regs[0x4] = 0xF0;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(&chip, &[(0x4, 0xF0)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x04F0);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_rng() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xC0, 0xFF, 0xC1, 0xFF, 0xC2, 0xFF, 0xC3, 0x0]);

        // Act: Step CPU Instruction
        for _ in 0..4 {
            chip.step();
        }

        // Assert: CPU State
        assert_regs(&chip, &[(0x0, 0x30), (0x1, 0xDB), (0x2, 0xA1)]);
        assert_eq!(chip.index, 0);
        assert_eq!(chip.pc, 0x208);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_store_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xFF, 0x55]);
        chip.index = 0x400;
        chip.regs[0x2] = 0x22;
        chip.regs[0x4] = 0x44;
        chip.regs[0xA] = 0xAA;
        chip.regs[0xF] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(
            &chip,
            &[
                (0x2, 0x22),
                (0x4, 0x44),
                (0xA, 0xAA),
                (0xF, 0xFF),
            ],
        );
        assert_eq!(chip.index, 0x400);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);

        // Assert memory
        assert_eq!(0x22, chip.read_u8(0x402));
        assert_eq!(0x44, chip.read_u8(0x404));
        assert_eq!(0xAA, chip.read_u8(0x40A));
        assert_eq!(0xFF, chip.read_u8(0x40F));
    }

    #[test]
    fn test_store_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xFF, 0x55]);
        chip.flags.quirk_inc_idx_load_store = true;
        chip.index = 0x400;
        chip.regs[0x2] = 0x22;
        chip.regs[0x4] = 0x44;
        chip.regs[0xA] = 0xAA;
        chip.regs[0xF] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(
            &chip,
            &[
                (0x2, 0x22),
                (0x4, 0x44),
                (0xA, 0xAA),
                (0xF, 0xFF),
            ],
        );
        assert_eq!(chip.index, 0x40F);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);

        // Assert memory
        assert_eq!(0x22, chip.read_u8(0x402));
        assert_eq!(0x44, chip.read_u8(0x404));
        assert_eq!(0xAA, chip.read_u8(0x40A));
        assert_eq!(0xFF, chip.read_u8(0x40F));
    }

    #[test]
    fn test_load_1() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xFF, 0x65]);
        chip.flags.quirk_inc_idx_load_store = true;
        chip.index = 0x400;
        chip.memory[0x402] = 0x22;
        chip.memory[0x404] = 0x44;
        chip.memory[0x40A] = 0xAA;
        chip.memory[0x40F] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(
            &chip,
            &[
                (0x2, 0x22),
                (0x4, 0x44),
                (0xA, 0xAA),
                (0xF, 0xFF),
            ],
        );
        assert_eq!(chip.index, 0x40F);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }

    #[test]
    fn test_load_2() {
        // Arrange: Setup chip8 emulator
        let mut chip = setup(&[0xFF, 0x65]);
        chip.index = 0x400;
        chip.memory[0x402] = 0x22;
        chip.memory[0x404] = 0x44;
        chip.memory[0x40A] = 0xAA;
        chip.memory[0x40F] = 0xFF;

        // Act: Step CPU Instruction
        chip.step();

        // Assert: CPU State
        assert_regs(
            &chip,
            &[
                (0x2, 0x22),
                (0x4, 0x44),
                (0xA, 0xAA),
                (0xF, 0xFF),
            ],
        );
        assert_eq!(chip.index, 0x400);
        assert_eq!(chip.pc, 0x202);
        assert_stack(&chip, &[]);
    }
}
