use std::{path::PathBuf, time::{Duration, Instant}};

use chip_8_core::{Chip8Builder, SCREEN_HEIGHT, SCREEN_WITDH};
use clap::Parser;
use sdl2::{event::Event, keyboard::Keycode, pixels::{Color, PixelFormatEnum}};


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

    let rom_data = std::fs::read(args.rom)
        .expect("Failed to read ROM file");
    builder = builder.with_rom(rom_data);

    if let Some(font) = args.font {
        let font_data: Vec<u8> = std::fs::read(font)
            .expect("Failed to read font file");
        builder = builder.with_font(font_data);
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

    let window = video_subsystem.window("chip8-emulator", 64 * args.scale, 32 * args.scale)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let texture_creator = canvas.texture_creator();
    let mut texture = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGBX8888, SCREEN_WITDH as u32, SCREEN_HEIGHT as u32)
        .unwrap();


    let mut event_pump = sdl_context.event_pump().unwrap();

    let delta = Duration::new(0, 1_000_000_000u32 / args.ips);
    let mut next_frame = Instant::now();

    'running: loop {
        sleep_until(next_frame);
        next_frame += delta;

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => {}
            }
        }

        chip.step();

        texture.update(None, chip.display(), SCREEN_WITDH * 4 ).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();
    }
}

pub fn sleep_until(deadline: Instant) {
    let now = Instant::now();

    if let Some(delay) = deadline.checked_duration_since(now) {
        ::std::thread::sleep(delay);
    }
}