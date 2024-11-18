use std::{path::PathBuf, time::{Duration, Instant}};

use chip_8_core::{Chip8Builder, Chip8Color, SCREEN_HEIGHT, SCREEN_WITDH};
use clap::Parser;
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
};

/// CHIP-8 Emulator
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Filepath to Chip-8 ROM file that will be executed
    #[clap(index = 1)]
    rom: PathBuf,

    /// Filepath to font file
    #[clap(long)]
    font: Option<PathBuf>,

    /// Background Color as HEX 0xAABBFF [default: 0x000000]
    #[clap(long)]
    background: Option<Chip8Color>,

    /// Foreground Color as HEX 0xAABBFF [default: 0xFFFFFF]
    #[clap(long)]
    foreground: Option<Chip8Color>,

    /// Display scaling factor
    #[clap(short, long, default_value_t = 10)]
    scale: u32,

    /// Instructions per second
    #[clap(short, long, default_value_t = 700)]
    ips: u32,

    /// PRNG seed
    #[clap(long)]
    seed: Option<u64>,

    /// Print debug information
    #[clap(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    let mut builder = Chip8Builder::new();

    let rom_data = std::fs::read(args.rom).expect("Failed to read ROM file");
    builder = builder.with_rom(rom_data);

    if let Some(font) = args.font {
        let font_data: Vec<u8> = std::fs::read(font).expect("Failed to read font file");
        builder = builder.with_font(font_data);
    }

    if let Some(foreground) = args.foreground {
        builder = builder.with_foreground(foreground);
    }

    if let Some(background) = args.background {
        builder = builder.with_background(background);
    }

    if let Some(seed) = args.seed {
        builder = builder.with_rng_seed(seed);
    }

    if args.debug {
        builder = builder.with_debug(true);
    }

    if args.scale == 0 || args.scale > 100 {
        panic!("Display scaling factor must be between [1-100]")
    }

    if args.ips == 0 || args.scale > 1_000_000 {
        panic!("Instructions per second [1-1000000]")
    }

    let mut chip = builder.build();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("chip8-emulator", 64 * args.scale, 32 * args.scale)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    //TODO: use background color    
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let texture_creator = canvas.texture_creator();
    let mut texture = texture_creator
        .create_texture_streaming(
            PixelFormatEnum::RGBX8888,
            SCREEN_WITDH as u32,
            SCREEN_HEIGHT as u32,
        )
        .unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let delta_update = Duration::new(0, 1_000_000_000u32 / args.ips);
    let mut next_update = Instant::now();
    let mut next_sec = next_update;

    'running: loop {
        // Wait until next update
        let now = Instant::now();
        if let Some(delay) = next_update.checked_duration_since(now) {
            ::std::thread::sleep(delay);
        }
        next_update += delta_update;

        // If one second has passed then step chip8 timers
        let now = Instant::now();
        if let Some(overdue) = now.checked_duration_since(next_sec) {
            next_sec += overdue + Duration::from_secs(1);
            chip.step_timer();
        }

        // Process events
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        // Execute one CHIP-8 instruction
        chip.step();
        
        // If display buffer was changed then draw changes on canvas
        let display = chip.display();
        if display.dirty() {
            // Copy CHIP-8 display buffer into GPU texture
            texture
                .update(None, display.buffer(), SCREEN_WITDH * 4)
                .unwrap();

            // Copy texture to Canvas
            canvas.copy(&texture, None, None).unwrap();

            // present canvas on screen
            canvas.present();
        }
    }
}
