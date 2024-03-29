// Copyright foobles 2023.
//
// This work is licensed under the Creative Commons Attribution-NonCommercial 4.0
// International License. To view a copy of this license, visit
// http://creativecommons.org/licenses/by-nc/4.0/ or send a letter to Creative
// Commons, PO Box 1866, Mountain View, CA 94042, USA.

//! A NES graphics emulator with a Rust interface.
//!
//! This library emulates the NTSC NES PPU (2c02), and provides a generic interface mimicking the
//! interface that exists on actual NES hardware. In addition, the interface also contains some
//! conveniences not available on the NES in order to make programming more ergonomic.
//!
//! ## Features
//! * `no_std` support
//! * Cycle-based emulation, including accurate timings for sprite processing and mapper accesses
//! * Emulation of all PPU registers (0x2000-0x2007)
//! * Support for arbitrary custom memory mappers and output formats
//! * Most system quirks are properly emulated:
//!   * Garbage nametable fetches
//!   * Different total cycle counts on even vs. odd frames
//!   * Buggy overflow flag behavior
//!   * Reading OAMDATA during rendering snoops on internal sprite processing state
//!   * Incorrect color output during forced blanking when vram address indexes palette ram
//!   * Etc.
//!
//! ## Limitations
//! * PPU register accesses happen "instantaneously," and do not cause the PPU to tick forwards
//!   despite reads/writes taking multiple cycles on real hardware
//!     * For truly accurate graphics, users must therefore be careful to weigh how much work is
//!       being done relative to when/how often they tick the PPU
//! * No support for PAL or Dendy PPUs
//! * No emulation of open bus behavior
//! * Mapper reads take 1 cycle to resolve, instead of 2 like on real hardware (the timings of when
//!   the reads start are still accurate)
//!
//! ## Example
//! ```
//! use nes_ppu::{Ppu, Mapper, PixelBuffer, Color, ColorEmphasis};
//!
//! struct ExamplePixelBuffer {
//!     /* ... */
//! }
//!
//! impl ExamplePixelBuffer {
//!     fn render_to_screen(&self) {
//!         /* ... */
//!     }
//! }
//!
//! impl PixelBuffer for ExamplePixelBuffer {
//!     fn set_color(&mut self, x: u8, y: u8, color: Color, emphasis: ColorEmphasis) {
//!         // write color information into internal screen buffer to be rendered when the frame
//!         // is complete
//!         /* ... */
//!     }
//! }
//!
//! struct ExampleMapper {
//!     /* ... */
//! }
//!
//! impl Mapper for ExampleMapper {
//!     fn read(&mut self, addr: u16) -> u8 {
//!         /* ... */
//! #       0
//!     }
//!
//!     fn write(&mut self, addr: u16, value: u8) {
//!         /* ... */
//!     }
//! }
//!
//! fn main_loop() {
//!     let mut ppu = Ppu::new();
//!     let mut mapper = ExampleMapper { /* ... */ };
//!     let mut buf = ExamplePixelBuffer { /* ... */ };
//!
//!     loop {
//!         // run main game logic here
//!
//!         ppu.tick_to_next_sprite_0_hit(&mut mapper, &mut buf);
//!         // add sprite 0 hit raster effect here
//!
//!         ppu.tick_to_next_vblank(&mut mapper, &mut buf);
//!         buf.render_to_screen();
//!
//!         // modify vram and set up scroll position for next frame here
//!     }
//! }
//! ```

#![no_std]
#[cfg(test)]
mod tests;
use bytemuck::{Pod, Zeroable};

/// A number corresponding to a color.
///
/// Each color the [`Ppu`] can output corresponds to a 6-bit `Color` value.
/// The low 4 bits specify the hue, and the high 2 bits specify the brightness.
///
/// Color values are only valid in the range `0x00-0x3F`. Any value outside that range will
/// be truncated to fit when writing to [`Ppu`] color palette memory.
///
/// Warning: using color value `13`/`0x0d` on actual Nintendo hardware generates
/// invalid NTSC signals that can cause televisions to display the frame incorrectly.
/// Avoid using color value `13`, though it is still allowed for authenticity.
///
/// Here is an example palette demonstrating what color each number should correspond to
/// (numbers given in hex):
/// ![Table showing 64 colored squares with overlayed hex values](https://i.imgur.com/4JRZBye.png)
///
/// Note that different TVs display NES output differently, so this palette is not exact. This
/// image is just a rough example.
pub type Color = u8;

/// A set of colors usable by tiles and sprites.
///
/// Tiles and sprites each have a bit depth of 2 bits per pixel, meaning that any given
/// tile or sprite can only use 4 total colors. For both tiles and sprites, the color index 0
/// always corresponds to transparency. The remaining indicies (1, 2, and 3) correspond to
/// the colors in a `Palette`.
///
/// The [`Ppu`] supports 4 different palettes for tiles, and a separate 4 palettes for sprites.
#[derive(Debug, Copy, Clone, Default)]
pub struct Palette {
    /// The array of 3 colors in the palette.
    pub colors: [Color; 3],
}

const SPRITE_SIZE: u8 = 4;

/// A floating graphic is rendered separately from tiles.
///
/// A sprite can be drawn to any position on the screen, and is either 8x8px or 8x16px in size.
/// Each sprite uses one of 4 sprite palettes for color, which are separate from the 4 tile
/// palettes.
///
/// [Read more about sprites on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_OAM)
#[derive(Debug, Copy, Clone, Default, Pod, Zeroable)]
#[repr(C)]
pub struct Sprite {
    /// Y coordinate of the sprite's top-left corner.
    pub y: u8,
    /// The index for the sprite's graphics. In 8x8px mode, this is relative to the
    /// sprite pattern table selected in the PPU [ctrl](Ppu::write_ctrl) register. In 8x16px mode,
    /// The least significant bit sets the bank, and the most significant 7 bits index into pairs
    /// of graphics within that bank.
    pub pattern_index: u8,
    /// A bit field configuring whether the sprite is flipped along the x or y axes,
    /// whether it is rendered in front of or behind tiles, and the sprite's palette index.
    pub attributes: u8,
    /// X coordinate of the sprite's top-left corner.
    pub x: u8,
}

/// The NES Picture Processing Unit.
///
/// The Picture Processing Unit, or PPU, renders each frame of video line-by-line,
/// pixel-by-pixel. It starts in the top left corner of the screen, moves to the right, and then
/// continues from the leftmost pixel of the next row underneath the previous.
///
/// ## Scanlines
/// Each row of pixels is referred to as a scanline. After a full row of 256 pixels is rendered,
/// the PPU has to wait a short amount of time (85 ticks) before it can begin outputting pixels for
/// the next scanline. During this waiting period, the PPU fetches graphics data from memory
/// that it will need for the next scanline. This waiting period at the end of each scanline is
/// referred to as **horizontal blanking** or **hblank**.
///
/// Because the PPU is not rendering during hblank, and thus some parts of its operation are idle,
/// hblank provides a short window for programs to interfere with the state of rendering in the
/// middle of a frame, albeit in a limited capacity.
///
/// ## Frames
/// Each frame of video the NES outputs is 256x240 pixels. After the 240th scanline is complete,
/// the PPU enters a new blanking period called **vertical blanking** or **vblank**, which lasts
/// for about 20 scanlines' worth of time (depending on how you count). During this period, the
/// PPU is completely idle, meaning this is the span of time where programs should write data
/// into VRAM, copy new sprite information into OAM, etc.
///
/// ## OAM
/// Object Attribute Memory, or OAM, is a 256 byte array stored internally within the PPU
/// containing information about all the sprites currently on screen. Each sprite takes up 4 bytes,
/// so OAM can alternately be thought of as an array of 64 sprites.
///
/// Each scanline, the PPU reads through OAM to see which sprites should be rendered on that line,
/// which it determines by comparing their y-coordinates, the sprite size configured in the
/// [ctrl](Ppu::write_ctrl) register, and the current scanline number. It picks the first 8
/// candidates it finds, and renders them on the next scanline.
///
/// This means that there are two important things to consider when placing sprite data into OAM:
/// * Sprites are rendered one pixel lower than their specified y-coordinate.
/// * Only 8 sprites are rendered per scanline, with ones coming later in OAM being ignored.
///
/// A common strategy on the NES to prevent sprites from "disappearing" when too many appear
/// on a single row of pixels is to shuffle the order that sprites appear in OAM every frame.
/// This way, instead of some sprites disappearing, all the sprites on that row "flicker," because
/// the sprites that get ignored are different every frame.
///
/// There is no way to disable a sprite; all 64 sprites are always active at once.
/// However, you can hide a sprite by setting its y-coordinate below the visible area of the
/// screen, i.e., to any value ≥ 240, most commonly 0xFF.
#[derive(Debug)]
pub struct Ppu {
    oam: [Sprite; 64],
    sprite_palettes: [Palette; 4],
    tile_palettes: [Palette; 4],
    zero_colors: [Color; 4],

    ctrl: u8,
    mask: u8,
    status: u8,

    t_reg: u16,
    v_reg: u16,
    fine_x_scroll: u8, // 3 bits,
    w_latch: bool,
    read_buffer: u8,

    sprite_0_cur_line: bool,
    sprite_0_next_line: bool,

    secondary_oam: [Sprite; 8],
    sprite_render_states: [SpriteRenderState; 8],
    oam_mdr: u8,
    oam_evaluation_index: u8,
    secondary_oam_evaluation_index: u8,
    sprite_evaluation_state: SpriteEvaluationState,
    oam_evaluation_index_overflow: bool,
    temp_sprite_pattern_lo: u8,

    tile_pattern_shift_reg: u32,   // four 8-bit shift registers
    tile_attribute_shift_reg: u16, // two 8-bit shift registers
    tile_attribute_latch: u8,      // 2 bits

    temp_tile_pattern_index: u8,
    temp_tile_attribute: u8,
    temp_tile_pattern_lo: u8,
    temp_tile_pattern_hi: u8,

    cur_scanline: u16,
    cur_dot: u16,
    is_even_frame: bool,
}

#[derive(Debug, Copy, Clone, Default)]
struct SpriteRenderState {
    pattern_shift_reg: u16, // two 8-bit shift registers
    x_counter: u8,
    attributes: u8,
}

#[derive(Eq, PartialEq)]
enum RenderMode {
    Normal,
    PreRender,
}

#[derive(Debug, Eq, PartialEq)]
enum SpriteEvaluationState {
    CheckingNormal,
    CheckingOverflow,
    CopyingNormal(u8),
    CopyingOverflow(u8),
    Complete,
}

/// Memory map used by the PPU to access video memory.
///
/// This trait enables the [`Ppu`] to read from video memory while rendering, in order to fetch
/// tile and pattern data. In addition, internal palette data is accessible to the user by
/// using certain address ranges.
///
/// The PPU uses the following address ranges to look up the corresponding data:
/// * `0x0000..=0x0FFF`: Pattern table 1
/// * `0x1000..=0x1FFF`: Pattern table 2
/// * `0x2000..=0x23FF`: Nametable 1
/// * `0x2400..=0x27FF`: Nametable 2
/// * `0x2800..=0x2BFF`: Nametable 3
/// * `0x2C00..=0x2FFF`: Nametable 4
///
/// Addresses outside of these ranges can be mapped to anything. Furthermore, you may
/// map addresses to overlapping regions of memory. For example, it is common to map the
/// addresses for nametables 1 and 2 to the same memory that the addresses for nametables 3 and 4
/// are mapped to.
///
/// The address range `0x3F00..=0x3F1F` and all subsequent addresses up to `0x3FFF` always
/// map to internal palette memory. On its own, the PPU will never access the mapper to read or
/// write to addresses in this range. The only situation where the mapper can be accessed with an
/// address in `0x3F00..=0x3FFF` is via a call to [`Ppu::read_data()`].
///
/// Read more on the NESdev Wiki:
/// * [PPU memory map](https://www.nesdev.org/wiki/PPU_memory_map)
/// * [Pattern tables](https://www.nesdev.org/wiki/PPU_pattern_tables)
/// * [Nametables](https://www.nesdev.org/wiki/PPU_nametables)
/// * [Palette memory](https://www.nesdev.org/wiki/PPU_palettes#Memory_Map)
pub trait Mapper {
    /// Returns the value mapped to the provided 14-bit address.
    ///
    /// This function is allowed to have side effects and consecutive reads of the same
    /// address do not need to return the same value.
    ///
    /// A memory read does not actually need to occur.
    fn read(&mut self, addr: u16) -> u8;

    /// Writes to the value mapped to the provided 14-bit address.
    ///
    /// This function is allowed to have side effects beyond the expressed memory write,
    /// and subsequent reads from the same address do not need to return `value`.
    ///
    /// A memory write does not actually need to occur.
    fn write(&mut self, addr: u16, value: u8);
}

/// Receives pixel information from the PPU as it draws.
///
/// This trait allows for the [`Ppu`] to be used with any graphical (or otherwise) frontend.
pub trait PixelBuffer {
    /// Sets the color of the pixel at the given x/y coordinate to that specified by `color`,
    /// and modulated by `emphasis`.
    ///
    /// `x` is always in the range `0..=255` and specifies distance from the left side of the screen.
    /// `y` is always in the range `0..=239` and specifies distance from the top of the screen.
    fn set_color(&mut self, x: u8, y: u8, color: Color, emphasis: ColorEmphasis);
}

/// Automatic increment of VRAM addr (0: 1; 1: 32).
pub const PPUCTRL_ADDR_INC: u8 = 1 << 2;
/// Sprite pattern table (0: 0x0000; 1: 0x1000). Ignored if sprite size is 8x16px.
pub const PPUCTRL_SPRITE_PATTERN_TABLE: u8 = 1 << 3;
/// Tile pattern table (0: 0x0000; 1: 0x1000).
pub const PPUCTRL_TILE_PATTERN_TABLE: u8 = 1 << 4;
/// Sprite size (0: 8x8px; 1: 8x16px).
pub const PPUCTRL_SPRITE_SIZE: u8 = 1 << 5;
/// EXT pin behavior (0: background read from EXT; 1: color output on EXT) - DO NOT USE.
///
/// Note that setting this bit in the [ctrl] register causes a short circuit on NES hardware,
/// which this library emulates via panicking.
///
/// [ctrl]: Ppu::write_ctrl
pub const PPUCTRL_MSS: u8 = 1 << 6;
/// Interrupt when PPU enters vblank (0: off; 1: on).
///
/// This emulator does not emulate interrupts, so this is ignored.
pub const PPUCTRL_NMI_ENABLE: u8 = 1 << 7;

/// Greyscale color.
pub const PPUMASK_GREYSCALE: u8 = 1 << 0;
/// Show tiles in the leftmost 8 pixels of the screen.
pub const PPUMASK_SHOW_COLUMN_0_TILES: u8 = 1 << 1;
/// Show sprites in the leftmost 8 pixels of the screen.
pub const PPUMASK_SHOW_COLUMN_0_SPRITES: u8 = 1 << 2;
/// Show tiles.
pub const PPUMASK_SHOW_TILES: u8 = 1 << 3;
/// Show sprites.
pub const PPUMASK_SHOW_SPRITES: u8 = 1 << 4;
/// Emphasize red color output.
pub const PPUMASK_EMPH_RED: u8 = 1 << 5;
/// Emphasize green color output.
pub const PPUMASK_EMPH_GREEN: u8 = 1 << 6;
/// Emphasize blue color output.
pub const PPUMASK_EMPH_BLUE: u8 = 1 << 7;

/// Sprite dropout has occurred this frame (bugged).
pub const PPUSTATUS_OVERFLOW: u8 = 1 << 5;
/// A non-transparent pixel of sprite 0 has overlapped a non-transparent pixel of a tile this frame.
pub const PPUSTATUS_SPRITE_0_HIT: u8 = 1 << 6;
/// The PPU is in vblank.
pub const PPUSTATUS_VBLANK: u8 = 1 << 7;

/// Mask for sprite palette index bits.
pub const SPRITE_PALETTE_MASK: u8 = 0b00000011;
/// Render sprite in front or behind tiles (0: in front; 1: behind).
pub const SPRITE_PRIORITY: u8 = 0b00100000;
/// Flip sprite along the x axis.
pub const SPRITE_FLIP_X: u8 = 0b01000000;
/// Flip sprite along the y axis.
pub const SPRITE_FLIP_Y: u8 = 0b10000000;

const X_SCROLL_MASK: u16 = 0b000_01_00000_11111;
const Y_SCROLL_MASK: u16 = 0b111_10_11111_00000;

const COARSE_X_SCROLL_MASK: u16 = 0b000_00_00000_11111;
const FINE_Y_SCROLL_MASK: u16 = 0b111_00_00000_00000;
const COARSE_Y_SCROLL_MASK: u16 = 0b000_00_11111_00000;

const COARSE_X_OFFSET: u16 = 0;
const FINE_Y_OFFSET: u16 = 12;
const COARSE_Y_OFFSET: u16 = 5;

const NAMETABLE_SELECT_MASK: u16 = 0b000_11_00000_00000;
// const NAMETABLE_X_SELECT_MASK: u16 = 0b000_01_00000_00000;
const NAMETABLE_Y_SELECT_MASK: u16 = 0b000_10_00000_00000;

const NAMETABLE_SELECT_OFFSET: u16 = 10;

const NAMETABLE_BASE_ADDRESS: u16 = 0b10_00_0000_000000;
const ATTRIBUTE_TABLE_OFFSET: u16 = 0b00_00_1111_000000;

/// Red, green, and/or blue color emphasis information.
///
/// This type is only used to communicate how a [`PixelBuffer`] should modulate
/// its color output when it receives pixel information.
///
/// The the `bits` field is a bitset with 3 fields in its least significant bits:
/// ```text
/// 7 6 5 4 3 2 1 0
/// x x x x x B G R
///           | | |
///           | | +- Red color emphasis.
///           | +--- Blue color emphasis.
///           +----- Green color emphasis.
/// ```
/// The high 5 bits will never be set.
#[derive(Debug, Copy, Clone)]
pub struct ColorEmphasis {
    pub bits: u8,
}
#[repr(u16)]
enum BitPlane {
    Lo = 0,
    Hi = 8,
}

#[derive(Debug, Copy, Clone)]
struct PixelInfo {
    color: Color,
    sprite_0_hit: bool,
}

fn pattern_table_base(b: u8) -> u16 {
    match b {
        0 => 0x0000,
        _ => 0x1000,
    }
}

// Used for combining two 8-bit shift register values into a single
// 16 bit value that is shifted down 2 bits at a time.
fn morton_encode_16(lo: u8, hi: u8) -> u16 {
    let mut x = u16::from(lo);
    x = (x | (x << 4)) & 0x0F0F;
    x = (x | (x << 2)) & 0x3333;
    x = (x | (x << 1)) & 0x5555;

    let mut y = u16::from(hi);
    y = (y | (y << 4)) & 0x0F0F;
    y = (y | (y << 2)) & 0x3333;
    y = (y | (y << 1)) & 0x5555;

    x | (y << 1)
}

fn palette_address_color_index(addr: u16) -> usize {
    (addr & 0b11) as usize
}

fn palette_address_palette_index(addr: u16) -> usize {
    ((addr >> 2) & 0b11) as usize
}

fn is_palette_address_tile_color(addr: u16) -> bool {
    (addr & 0b10000) == 0
}

impl Ppu {
    /// Creates a new PPU instance in an unspecified state.
    pub fn new() -> Self {
        Ppu {
            oam: [Sprite::default(); 64],
            sprite_palettes: [Palette::default(); 4],
            tile_palettes: [Palette::default(); 4],
            zero_colors: [0; 4],
            ctrl: 0,
            mask: 0,
            status: 0,
            t_reg: 0,
            v_reg: 0,
            fine_x_scroll: 0,
            w_latch: false,
            read_buffer: 0,
            sprite_0_cur_line: false,
            sprite_0_next_line: false,
            secondary_oam: [Sprite::default(); 8],
            sprite_render_states: [SpriteRenderState::default(); 8],
            oam_mdr: 0,
            oam_evaluation_index: 0,
            secondary_oam_evaluation_index: 0,
            sprite_evaluation_state: SpriteEvaluationState::CheckingNormal,
            oam_evaluation_index_overflow: false,
            temp_sprite_pattern_lo: 0,
            tile_pattern_shift_reg: 0,
            tile_attribute_shift_reg: 0,
            tile_attribute_latch: 0,
            temp_tile_pattern_index: 0,
            temp_tile_attribute: 0,
            temp_tile_pattern_lo: 0,
            temp_tile_pattern_hi: 0,
            cur_scanline: 261,
            cur_dot: 0,
            is_even_frame: false,
        }
    }

    /// Run the PPU for 1 cycle. This may induce memory accesses through the `mapper`, as well as
    /// outputting pixel information to the `buffer`.
    pub fn tick<M: Mapper, B: PixelBuffer>(&mut self, mapper: &mut M, buffer: &mut B) {
        match self.cur_scanline {
            0..=239 => self.tick_render(mapper, buffer, RenderMode::Normal), // render
            240..=260 => self.tick_vblank(),                                 // vblank
            261 => self.tick_render(mapper, buffer, RenderMode::PreRender),  // pre render line
            _ => unreachable!(),
        }

        match self.cur_dot {
            339 if self.cur_scanline == 261 => {
                if !self.is_even_frame && self.is_rendering_enabled() {
                    self.cur_dot = 0;
                    self.cur_scanline = 0;
                } else {
                    self.cur_dot += 1;
                }
                self.is_even_frame = !self.is_even_frame;
            }
            0..=339 => {
                self.cur_dot += 1;
            }
            340 => {
                self.cur_dot = 0;
                self.cur_scanline = (self.cur_scanline + 1) % 262;
            }
            _ => unreachable!(),
        }
    }

    /// Run the PPU until it would emit an interrupt signalling that it has entered the vertical
    /// blanking period. If the PPU is already in vblank, this will tick until the
    /// vertical blank of the next frame.
    ///
    /// Just like [`Ppu::tick()`], this function may induce memory accesses through `mapper` and
    /// output pixel information to `buffer`.
    ///
    /// Once this function returns, a full frame will have been rendered to `buffer`, which is a
    /// good time to output its contents to the screen.
    pub fn tick_to_next_vblank<M: Mapper, B: PixelBuffer>(
        &mut self,
        mapper: &mut M,
        buffer: &mut B,
    ) {
        while self.status & PPUSTATUS_VBLANK != 0 {
            self.tick(mapper, buffer);
        }
        while self.status & PPUSTATUS_VBLANK == 0 {
            self.tick(mapper, buffer);
        }
    }

    /// Run the PPU until the sprite-0-hit flag is set in the status register. If the flag is
    /// already set, this will run until it is cleared and then set again.
    ///
    /// Just like [`Ppu::tick()`], this function may induce memory accesses through `mapper` and
    /// output pixel information to `buffer`.
    ///
    /// Once this function returns, the PPU will have just output the first pixel where a
    /// non-transparent pixel of the sprite in slot 0 overlaps a non-transparent pixel of a
    /// tile. This is a good way to time mid-frame raster effects like split scrolling.
    ///
    pub fn tick_to_next_sprite_0_hit<M: Mapper, B: PixelBuffer>(
        &mut self,
        mapper: &mut M,
        buffer: &mut B,
    ) {
        while self.status & PPUSTATUS_SPRITE_0_HIT != 0 {
            self.tick(mapper, buffer);
        }
        while self.status & PPUSTATUS_SPRITE_0_HIT == 0 {
            self.tick(mapper, buffer);
        }
    }

    /// Overwrites the contents of [OAM] with the given sprite array.
    ///
    /// This is a convenience utility meant to partially replicate the behavior of OAM DMA on the
    /// actual NES, but it is not emulated precisely.
    ///
    /// [OAM]: Ppu#oam
    pub fn set_oam(&mut self, sprites: [Sprite; 64]) {
        self.oam = sprites;
    }

    /// Overwrites the contents of [OAM] with the given byte array.
    ///
    /// This is a convenience utility meant to partially replicate the behavior of OAM DMA on the
    /// actual NES, but it is not emulated precisely.
    ///
    /// [OAM]: Ppu#oam
    pub fn set_oam_bytes(&mut self, bytes: [u8; 256]) {
        self.oam_bytes_mut().copy_from_slice(&bytes);
    }

    /// Sets the high or low byte of the address for VRAM data accesses.
    ///
    /// Calling `write_addr()` the first time will set the high 6 bits of the VRAM address (the
    /// high 2 bits of the input are ignored),
    /// and calling it again will set the low 8 bits. Thus, `write_addr()` is usually called
    /// twice in succession.
    ///
    /// Writing to either half of the address writes to the internal T register.
    /// When writing to the low 8 bits, the internal V register is set to the
    /// new value of T afterwards. This means that the effective address that VRAM data accesses
    /// will use is only updated after setting the low bits.
    /// ```
    /// # use nes_ppu::*;
    /// let mut ppu = Ppu::new();
    /// ppu.write_addr(0x20);       // sets the high 6 bits of the address
    /// ppu.write_addr(0x01);       // sets the low 8 bits of the address & updates effective address
    /// // the address now contained in T and V is 0x2001.
    /// ```
    /// Whether or not `write_addr()` updates the high or low bits of the address is dependent
    /// on the internal W latch, which is also modified by [`Ppu::write_scroll()`] and
    /// [`Ppu::read_status()`]. Here is an example of interfering with W in the middle of writing
    /// an address:
    /// ```
    /// # use nes_ppu::*;
    /// let mut ppu = Ppu::new();   // W latch = 0
    /// ppu.write_addr(0x20);       // W = 0, so sets the high bits of the address (now W = 1)
    /// _ = ppu.read_status();      // clears W, now W = 0
    /// ppu.write_addr(0x24);       // W = 0, so sets the high bits again (now W = 1)
    /// ppu.write_addr(0x00);       // W = 1, so sets the low bits and sets V to T (now W = 0)
    /// ```
    ///
    /// [Read more about the addr register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUADDR)
    ///
    /// [Read more about how setting the address interacts with W, T, and V on the NESdev wiki.](https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers)
    pub fn write_addr(&mut self, b: u8) {
        let b = u16::from(b);
        if !self.w_latch {
            self.t_reg &= 0x00FF;
            self.t_reg |= (b & 0x3F) << 8;
        } else {
            self.t_reg &= 0xFF00;
            self.t_reg |= b;
            self.v_reg = self.t_reg;
        }

        self.w_latch = !self.w_latch;
    }

    /// Sets the value of x or y scrolling relative to the current nametable selected in the
    /// [ctrl](Ppu::write_ctrl) register.
    ///
    /// Calling `write_scroll()` the first time will set the x scroll, and calling it again
    /// will set the y scroll. Thus, `write_scroll()` is usually called twice in succession:
    /// ```
    /// # use nes_ppu::*;
    /// let mut ppu = Ppu::new();
    /// ppu.write_scroll(100);      // sets the x scroll
    /// ppu.write_scroll(50);       // sets the y scroll
    /// ```
    /// Note that x and y scroll values modify the value of the internal T register. Also,
    /// nametables are only 240 pixels tall, so setting the y scroll to a number ≥240 will cause
    /// garbage tiles to be displayed.
    ///
    /// Whether or not this function updates the x or y scroll is dependent on the internal W latch.
    /// Both [`Ppu::write_addr()`] and [`Ppu::read_status()`] also affect this latch.
    ///
    /// Here is an example where W is interfered with between the two writes:
    /// ```
    /// # use nes_ppu::*;
    /// let mut ppu = Ppu::new();   // W latch = 0
    /// ppu.write_scroll(100);      // because W = 0, sets the x scroll (now W = 1)
    /// _ = ppu.read_status();      // clears W, now W = 0
    /// ppu.write_scroll(100);      // because W = 0, sets the x scroll again (now W = 1)
    /// ppu.write_scroll(50);       // because W = 1, sets the y scroll (now W = 0 again)
    ///```
    ///
    /// [Read more about the scroll register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUSCROLL)
    ///
    /// [Read about how scroll interacts with W and T on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers)
    pub fn write_scroll(&mut self, b: u8) {
        let fine = b & 0b00000111;
        let coarse = b >> 3;
        if !self.w_latch {
            self.fine_x_scroll = fine;
            self.t_reg &= !COARSE_X_SCROLL_MASK | NAMETABLE_SELECT_MASK;
            self.t_reg |= u16::from(coarse) << COARSE_X_OFFSET;
        } else {
            self.t_reg &= !Y_SCROLL_MASK | NAMETABLE_SELECT_MASK;
            self.t_reg |= u16::from(fine) << FINE_Y_OFFSET;
            self.t_reg |= u16::from(coarse) << COARSE_Y_OFFSET;
        }

        self.w_latch = !self.w_latch;
    }

    /// Sets the value of the ctrl register.
    ///
    /// This updates the current nametable, the sprite pattern table, the tile pattern table,
    /// sprite sizes, and the automatic address increment. The nametable select bits are also
    /// copied into the internal T register.
    ///
    /// Note that, to emulate the short-circuiting behavior of the NES when bit 6 of ctrl
    /// is set, calling `write_ctrl()` with a value that has bit 6 set will induce a panic.
    ///
    /// [Read more about the ctrl register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUCTRL)
    pub fn write_ctrl(&mut self, b: u8) {
        // TODO: bit 0 race condition
        assert_eq!((b & PPUCTRL_MSS), 0, "PPUCTRL bit 6 set; system short");
        self.ctrl = b;
        let nametable_bits = u16::from(b & 0b00000011);
        self.t_reg &= !NAMETABLE_SELECT_MASK;
        self.t_reg |= nametable_bits << NAMETABLE_SELECT_OFFSET;
    }

    /// Sets the value of the mask register.
    ///
    /// This controls greyscale, sprite rendering, tile rendering, sprite
    /// rendering in the leftmost 8 pixels, tile rendering in the leftmost 8 pixels,
    /// and color emphasis.
    ///
    /// [Read more about the mask register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUMASK)
    pub fn write_mask(&mut self, b: u8) {
        self.mask = b;
    }

    /// Get info about the current PPU status for the frame.
    ///
    /// Returns a bitset containing 3 flags. The remaining 5 bits have unspecified values.
    /// The flags are as follows:
    /// * Bit 5: Supposed to indicate that this frame, the PPU has evaluated a scanline where
    /// more than 8 sprites would have to be drawn, resulting in dropout. This flag is bugged and
    /// does not work intuitively, which this library emulates.
    /// * Bit 6: Indicates that this frame, a non-transparent pixel of the sprite in OAM index 0
    /// has overlapped with a non-transparent pixel of a tile.
    /// * Bit 7: Indicates whether the PPU currently in vblank. This flag is cleared after calling
    /// `read_status()`. Additionally, due to race conditions, this flag is bugged on actual NES
    /// hardware. For authenticity, try to rely on [`Ppu::tick_to_next_vblank()`] instead.
    ///
    /// Calling `read_status()` also clears the W latch, which affects future calls to
    /// [`Ppu::write_addr()`] and [`Ppu::write_scroll()`].
    ///
    /// [Read more about the status register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUSTATUS)
    ///
    /// [Read more about the bugged sprite overflow flag on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_sprite_evaluation#Cause_of_the_sprite_overflow_bug)
    ///
    /// [Read more about the W latch on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers)
    pub fn read_status(&mut self) -> u8 {
        let ret = self.status;
        self.status &= !PPUSTATUS_VBLANK;
        self.w_latch = false;
        ret
    }

    /// Reads the value at the currently stored address.
    ///
    /// This function reads the value at the location specified by the current address stored
    /// in the PPU, but does not necessarily return that value. The address is usually
    /// set up by calling [`Ppu::write_addr()`]. If the current address is in the
    /// range 0x0000..=0x3EFF, then this function will return the value stored in an internal read
    /// buffer, then perform the memory fetch through the [`Mapper::read()`] method of `mapper`,
    /// and update the internal read buffer with the result.
    ///
    /// After the read is performed, the current address will automatically increment by either
    /// 1 or 32, depending on the value in the [ctrl](Ppu::write_ctrl) register.
    ///
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu, mut mapper: impl Mapper) {
    /// ppu.write_ctrl(0); // set automatic increment to 1
    /// // set address to 0x2000
    /// ppu.write_addr(0x20);
    /// ppu.write_addr(0x00);
    ///
    /// // Discard whatever is currently in the read buffer.
    /// // Then update read buffer with value in 0x2000 and increment address to 0x2001.
    /// _ = ppu.read_data(&mut mapper);
    ///
    /// // Set n to the value fetched from 0x2000 in the previous read.
    /// // Then update read buffer with value in 0x2001 and increment address to 0x2002
    /// let n = ppu.read_data(&mut mapper);
    ///
    /// // Set m to the value fetched from 0x2001 in the previous read.
    /// // Then update read buffer with value in 0x2002 and increment address to 0x2003
    /// let m = ppu.read_data(&mut mapper);
    /// # }
    /// ```
    ///
    /// If the address is in the range 0x3F00..=0x3FFF, then this function will directly
    /// return the value stored within internal palette memory at that address. However, the
    /// internal read buffer is still updated with the value obtained from [`Mapper::read()`]
    /// on `mapper`.
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu, mut mapper: impl Mapper) {
    /// ppu.write_ctrl(0); // set automatic increment to 1
    /// // set address to 0x3F00
    /// ppu.write_addr(0x3F);
    /// ppu.write_addr(0x00);
    ///
    /// // Set n to value in palette memory at 0x3F00.
    /// // Update read buffer to mapper.read(0x3F00), and increment address to 0x3F01.
    /// let n = ppu.read_data(&mut mapper);
    /// # }
    /// ```
    /// The address that is actually accessed is the low 14 bits of the internal V register.
    /// The 15th bit is completely ignored by `read_data()`, and the automatic address increment
    /// will not carry into the 15th bit.
    ///
    /// [Read more about the data register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUDATA)
    ///
    /// [Read more about the internal V register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers)
    pub fn read_data<M: Mapper>(&mut self, mapper: &mut M) -> u8 {
        let effective_addr = self.v_reg & 0x3FFF;
        self.increment_address();
        let data = core::mem::replace(&mut self.read_buffer, mapper.read(effective_addr));
        self.access_palette_ram(effective_addr).unwrap_or(data)
    }

    /// Writes to the value at the currently stored address.
    ///
    /// This function will write `value` to the location specified by the current address
    /// stored in the PPU. The address is usually set up using [`Ppu::write_addr()`].
    /// If the current address is in the range 0x0000..=0x3EFF, the write will invoke
    /// the [`Mapper::write()`] method of `mapper`. Otherwise, if the address is in the range
    /// 0x3F00..=0x3FFF, the mapper will not be accessed and the write will instead be directed
    /// into internal palette memory.
    ///
    /// After the write is performed, the current address will automatically increment by either
    /// 1 or 32, depending on the value in the [ctrl](Ppu::write_ctrl) register.
    ///
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu, mut mapper: impl Mapper) {
    /// ppu.write_ctrl(0); // set automatic increment to 1
    /// // set address to 0x2000
    /// ppu.write_addr(0x20);
    /// ppu.write_addr(0x00);
    ///
    /// ppu.write_data(&mut mapper, 10); // write 10 at address 0x2000
    /// ppu.write_data(&mut mapper, 15); // write 15 at address 0x2001
    /// # }
    /// ```
    /// The address that is actually accessed is the low 14 bits of the internal V register.
    /// The 15th bit is completely ignored by `write_data()`, and the automatic address increment
    /// will not carry into the 15th bit.
    ///
    /// [Read more about the data register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#PPUDATA)
    ///
    /// [Read more about the internal V register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers)
    pub fn write_data<M: Mapper>(&mut self, mapper: &mut M, value: u8) {
        let effective_addr = self.v_reg & 0x3FFF;
        self.increment_address();
        if let Some(color) = self.access_palette_ram_mut(effective_addr) {
            *color = value & 0x3F;
        } else {
            mapper.write(effective_addr, value);
        }
    }

    /// Writes a sequence of bytes starting at the current address.
    ///
    /// Repeatedly calls [`Ppu::write_data()`] for each element of `values`. Because `write_data()`
    /// automatically increments the current address, this will write each value into sequential
    /// memory locations. Note that depending on the value in the [ctrl](Ppu::write_ctrl) register,
    /// the values may be written consecutively or spaced apart by 32 bytes each.
    ///
    /// This is a convenience utility not actually present when programming for the NES.
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu, mut mapper: impl Mapper) {
    /// ppu.write_ctrl(0b100); // set automatic increment to 32
    /// // set address to 0x2000
    /// ppu.write_addr(0x20);
    /// ppu.write_addr(0x00);
    ///
    /// // Write to addresses 0x2000, 0x2020, 0x2040, 0x2060, and 0x2080.
    /// ppu.write_data_iter(&mut mapper, [1, 2, 3, 4, 5]);
    /// # }
    /// ```
    pub fn write_data_iter<M: Mapper, I>(&mut self, mapper: &mut M, values: I)
    where
        I: IntoIterator<Item = u8>,
    {
        for value in values {
            self.write_data(mapper, value);
        }
    }

    /// Sets the current oam address.
    ///
    /// This function sets the PPU's current 8-bit address into [OAM](Ppu#oam).
    /// Since this address is used internally during rendering to evaluate which sprites are
    /// visible, writing to the OAM address in the middle of rendering can corrupt which sprites are
    /// drawn. The current OAM address is also automatically reset to 0 towards the end of vblank.
    ///
    /// Due to internal hardware issues, writing to the OAM address can corrupt the
    /// contents of OAM itself, which this library emulates. As a result, this function is useless
    /// in a majority of cases.
    ///
    /// This function is most commonly used in conjunction with [`Ppu::write_oam_data()`] or
    /// [`Ppu::read_oam_data()`] to access the contents of OAM. However, due to the aforementioned
    /// issue where calling this function corrupts OAM, this is rarely practical.
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu) {
    /// ppu.write_oam_addr(0x80);
    /// ppu.write_oam_data(0xFF); // write value 0xFF to address 0x80
    /// let n = ppu.read_oam_data(); // read value at address 0x81
    /// # }
    /// ```
    /// In almost all cases, to write to OAM, it is better to use [`Ppu::set_oam()`] or
    /// [`Ppu::set_oam_bytes()`] instead.
    ///
    /// [Read about the OAM addr register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#OAMADDR)
    ///
    /// [Read about the OAM memory layout on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_OAM)
    pub fn write_oam_addr(&mut self, addr: u8) {
        // Corrupt OAM unconditionally.
        // While these corruptions don't happen 100% of the time on real hardware,
        // this should force users to be careful.
        for i in 0..8 {
            let src_index = (0x20 + i) as usize;
            let dest_index = self.oam_evaluation_index.wrapping_add(i) as usize;
            let val = self.oam_bytes()[src_index];
            self.oam_bytes_mut()[dest_index] = val;
        }

        self.oam_evaluation_index = addr;
    }

    /// Reads the value pointed to by the current OAM address.
    ///
    /// If the PPU is in vblank or rendering is disabled, then this function returns the value
    /// referred to by the currently stored OAM address. Otherwise, the PPU is in a busy state
    /// and this function will return the value most recently loaded by the PPU during
    /// its internal sprite processing routine. Unlike [`Ppu::write_oam_data()`], this function
    /// does not automatically increment the OAM address.
    ///
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu) {
    /// // assume ppu is in vblank
    ///
    /// // set current address to 0x55 (possibly causing OAM corruption)
    /// ppu.write_oam_addr(0x55);
    /// // read the value in OAM at address 0x55
    /// let n = ppu.read_oam_data();
    /// # }
    /// ```
    ///
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu) {
    /// // assume ppu is currently rendering
    ///
    /// // snoop the value the PPU most recently read from OAM during internal sprite processing
    /// let n = ppu.read_oam_data();
    /// # }
    /// ```
    ///
    /// See also: [`Ppu::write_oam_addr()`], [`Ppu::write_oam_data()`].
    ///
    /// [Read more about the OAM data register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#OAMDATA)
    pub fn read_oam_data(&mut self) -> u8 {
        if self.is_rendering() {
            self.oam_mdr
        } else {
            self.cur_oam_byte()
        }
    }

    /// Writes data into OAM at the current address.
    ///
    /// If the PPU is in vblank or rendering is disabled, this function writes `value` into OAM
    /// at the current OAM address, then increments the current address by 1. This increment may
    /// cause an overflow from 0xFF back to address 0x00.
    /// If the PPU is currently rendering, then no write is performed, and the current OAM address
    /// is instead incremented by 4.
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu) {
    /// // assume ppu is in vblank
    ///
    /// // set current address to 0x55 (possibly causing OAM corruption)
    /// ppu.write_oam_addr(0x55);
    /// // set the value in OAM at address 0x55 to 0xAA
    /// ppu.write_oam_data(0xAA);
    /// // set the value in OAM at address 0x56 to 0xBB
    /// ppu.write_oam_data(0xBB);
    /// # }
    /// ```
    ///
    /// ```
    /// # use nes_ppu::*;
    /// # fn example(mut ppu: Ppu) {
    /// // assume ppu is currently rendering
    ///
    /// // perform no write and increment OAM address by 4 (this will interfere with sprite processing)
    /// ppu.write_oam_data(0xAA);
    /// # }
    /// ```
    ///
    /// See also: [`Ppu::write_oam_addr()`], [`Ppu::read_oam_data()`].
    ///
    /// [Read more about the OAM data register on the NESdev Wiki.](https://www.nesdev.org/wiki/PPU_registers#OAMDATA)
    pub fn write_oam_data(&mut self, value: u8) {
        if self.is_rendering() {
            self.increment_oam_evaluation_index(SPRITE_SIZE);
        } else {
            let index = self.oam_evaluation_index as usize;
            self.oam_bytes_mut()[index] = value;
            self.increment_oam_evaluation_index(1);
        }
    }

    fn access_palette_ram(&mut self, addr: u16) -> Option<u8> {
        if addr < 0x3F00 {
            return None;
        }

        let color_index = palette_address_color_index(addr);
        let palette_index = palette_address_palette_index(addr);
        Some(if color_index == 0 {
            self.zero_colors[palette_index]
        } else if is_palette_address_tile_color(addr) {
            self.tile_palettes[palette_index].colors[color_index - 1]
        } else {
            self.sprite_palettes[palette_index].colors[color_index - 1]
        })
    }

    fn access_palette_ram_mut(&mut self, addr: u16) -> Option<&mut u8> {
        if addr < 0x3F00 {
            return None;
        }

        let color_index = palette_address_color_index(addr);
        let palette_index = palette_address_palette_index(addr);
        Some(if color_index == 0 {
            &mut self.zero_colors[palette_index]
        } else if is_palette_address_tile_color(addr) {
            &mut self.tile_palettes[palette_index].colors[color_index - 1]
        } else {
            &mut self.sprite_palettes[palette_index].colors[color_index - 1]
        })
    }

    fn increment_address(&mut self) {
        let new_addr = self.v_reg
            + match self.ctrl & PPUCTRL_ADDR_INC {
                0 => 1,
                _ => 32,
            };

        self.v_reg ^= new_addr;
        self.v_reg &= 0xC000;
        self.v_reg ^= new_addr;
    }

    fn sprite_height(&self) -> u8 {
        match self.ctrl & PPUCTRL_SPRITE_SIZE {
            0 => 8,
            _ => 16,
        }
    }

    fn oam_bytes(&self) -> &[u8; 256] {
        <&[u8; 256]>::try_from(bytemuck::bytes_of(&self.oam)).unwrap()
    }

    fn oam_bytes_mut(&mut self) -> &mut [u8; 256] {
        <&mut [u8; 256]>::try_from(bytemuck::bytes_of_mut(&mut self.oam)).unwrap()
    }

    fn secondary_oam_bytes(&self) -> &[u8; 32] {
        <&[u8; 32]>::try_from(bytemuck::bytes_of(&self.secondary_oam)).unwrap()
    }

    fn secondary_oam_bytes_mut(&mut self) -> &mut [u8; 32] {
        <&mut [u8; 32]>::try_from(bytemuck::bytes_of_mut(&mut self.secondary_oam)).unwrap()
    }

    fn sprite_pattern_table_base(&self) -> u16 {
        pattern_table_base(self.ctrl & PPUCTRL_SPRITE_PATTERN_TABLE)
    }

    fn tile_pattern_table_base(&self) -> u16 {
        pattern_table_base(self.ctrl & PPUCTRL_TILE_PATTERN_TABLE)
    }

    fn increment_coarse_x(&mut self) {
        let coarse_x = (self.v_reg & X_SCROLL_MASK).wrapping_add(!X_SCROLL_MASK + 1);
        self.v_reg ^= coarse_x;
        self.v_reg &= !X_SCROLL_MASK;
        self.v_reg ^= coarse_x;
    }

    fn increment_y(&mut self) {
        if self.v_reg & FINE_Y_SCROLL_MASK != FINE_Y_SCROLL_MASK {
            self.v_reg += 1 << FINE_Y_OFFSET;
        } else {
            self.v_reg &= !FINE_Y_SCROLL_MASK;
            let coarse_y = (self.v_reg & COARSE_Y_SCROLL_MASK) >> COARSE_Y_OFFSET;
            self.v_reg = match coarse_y {
                29 => (self.v_reg & !COARSE_Y_SCROLL_MASK) ^ NAMETABLE_Y_SELECT_MASK,
                31 => self.v_reg & !COARSE_Y_SCROLL_MASK,
                _ => self.v_reg + (1 << COARSE_Y_OFFSET),
            }
        }
    }

    fn nametable_address(&self) -> u16 {
        NAMETABLE_BASE_ADDRESS | (self.v_reg & 0b000_11_11111_11111)
    }

    fn attribute_table_address(&self) -> u16 {
        // move high 3 bits of coarse x scroll into bits 0-2
        let attr_x_component = (self.v_reg >> 2) & 0b000_00_0000_000111;
        // move high 3 bits of coarse y scroll into bits 3-5
        let attr_y_component = (self.v_reg >> 4) & 0b000_00_0000_111000;
        // nametable base address + nametable select + attribute table offset
        NAMETABLE_BASE_ADDRESS
            | (self.v_reg & NAMETABLE_SELECT_MASK)
            | ATTRIBUTE_TABLE_OFFSET
            | attr_x_component
            | attr_y_component
    }

    fn fine_y_scroll(&self) -> u8 {
        (self.v_reg >> FINE_Y_OFFSET) as u8
    }

    fn tile_pattern_address(&self, index: u8, plane: BitPlane) -> u16 {
        let tile_offset = u16::from(index) * 16;
        let row_offset = u16::from(self.fine_y_scroll());
        self.tile_pattern_table_base() + tile_offset + row_offset + plane as u16
    }

    fn sprite_pattern_address(&self, sprite: Sprite, plane: BitPlane) -> u16 {
        // wrapping arithmetic so that empty Secondary OAM slots with unexpected Y values
        // don't trigger unexpected overflows.
        let distance_from_sprite_top = if sprite.attributes & SPRITE_FLIP_Y == 0 {
            self.cur_scanline.wrapping_sub(u16::from(sprite.y))
        } else {
            (u16::from(sprite.y) + u16::from(self.sprite_height() - 1))
                .wrapping_sub(self.cur_scanline)
        };

        if self.ctrl & PPUCTRL_SPRITE_SIZE == 0 {
            let tile_offset = u16::from(sprite.pattern_index) * 16;
            let row_offset = distance_from_sprite_top & 0b111;
            self.sprite_pattern_table_base() + tile_offset + row_offset + plane as u16
        } else {
            let table_base = pattern_table_base(sprite.pattern_index & 1);
            let tile_pair_offset = u16::from(sprite.pattern_index & 0b11111110) * 16;
            let tile_select = (distance_from_sprite_top & 0b1000) << 1;
            let row_offset = distance_from_sprite_top & 0b111;
            table_base + tile_pair_offset + tile_select + row_offset + plane as u16
        }
    }

    fn flush_horizontal_scroll(&mut self) {
        self.v_reg ^= self.t_reg;
        self.v_reg &= Y_SCROLL_MASK;
        self.v_reg ^= self.t_reg;
    }

    fn flush_vertical_scroll(&mut self) {
        self.v_reg ^= self.t_reg;
        self.v_reg &= X_SCROLL_MASK;
        self.v_reg ^= self.t_reg;
    }

    fn tile_attribute_bits_from_temp(&self) -> u8 {
        let x_bit = (self.v_reg >> 4) & 0b100;
        let y_bit = self.v_reg & 0b010;
        self.temp_tile_attribute >> (x_bit | y_bit) as u8
    }

    fn are_sprites_visible(&self) -> bool {
        let in_column_0 = self.cur_dot < 8;
        (self.mask & PPUMASK_SHOW_SPRITES != 0)
            && !(in_column_0 && self.mask & PPUMASK_SHOW_COLUMN_0_SPRITES == 0)
    }

    fn are_tiles_visible(&self) -> bool {
        let in_column_0 = self.cur_dot < 8;
        (self.mask & PPUMASK_SHOW_TILES != 0)
            && !(in_column_0 && self.mask & PPUMASK_SHOW_COLUMN_0_TILES == 0)
    }

    fn is_rendering_enabled(&self) -> bool {
        self.mask & PPUMASK_SHOW_TILES != 0 || self.mask & PPUMASK_SHOW_SPRITES != 0
    }

    fn is_rendering(&self) -> bool {
        self.is_rendering_enabled() && (self.cur_scanline < 240 || self.cur_scanline == 261)
    }

    fn greyscale_mask(&self) -> u8 {
        if self.mask & PPUMASK_GREYSCALE == 0 {
            0x3F
        } else {
            0x30
        }
    }

    fn background_color(&self) -> Color {
        self.zero_colors[0]
    }

    fn calculate_cur_pixel(&self) -> PixelInfo {
        let tile_attribute = (self.tile_attribute_shift_reg >> (2 * self.fine_x_scroll)) & 0b11;
        let tile_color_index = if self.are_tiles_visible() {
            (self.tile_pattern_shift_reg >> (2 * self.fine_x_scroll)) & 0b11
        } else {
            0
        };
        let tile_attribute = tile_attribute as usize;
        let tile_color_index = tile_color_index as usize;

        let (visible_sprite_index, visible_sprite) = self
            .sprite_render_states
            .iter()
            .enumerate()
            .filter(|&(_, s)| {
                self.are_sprites_visible() && s.x_counter == 0 && s.pattern_shift_reg & 0b11 != 0
            })
            .next()
            .unzip();

        let color = match (visible_sprite, tile_color_index) {
            (Some(s), _) if tile_color_index == 0 || s.attributes & SPRITE_PRIORITY == 0 => {
                let sprite_palette_index = (s.attributes & SPRITE_PALETTE_MASK) as usize;
                let sprite_color_index = (s.pattern_shift_reg & 0b11) as usize;
                self.sprite_palettes[sprite_palette_index].colors[sprite_color_index - 1]
            }
            (_, 0) => self.background_color(),
            (_, _) => self.tile_palettes[tile_attribute].colors[tile_color_index - 1],
        };
        let sprite_0_hit = self.sprite_0_cur_line
            && visible_sprite_index == Some(0)
            && tile_color_index != 0
            && self.cur_dot < 255;

        PixelInfo {
            color,
            sprite_0_hit,
        }
    }

    fn output_pixel<B: PixelBuffer>(&self, buffer: &mut B, color: Color) {
        let color = color & self.greyscale_mask();
        let emphasis = ColorEmphasis {
            bits: self.mask >> 5,
        };
        buffer.set_color(self.cur_dot as u8, self.cur_scanline as u8, color, emphasis);
    }

    fn tick_tile_pipeline<M: Mapper>(&mut self, mapper: &mut M) {
        match (self.cur_dot - 1) & 0b111 {
            0b000 => {
                self.temp_tile_pattern_index = mapper.read(self.nametable_address());
            }
            0b010 => {
                self.temp_tile_attribute = mapper.read(self.attribute_table_address());
            }
            0b100 => {
                let tile_pattern = self.temp_tile_pattern_index;
                self.temp_tile_pattern_lo = mapper
                    .read(self.tile_pattern_address(tile_pattern, BitPlane::Lo))
                    .reverse_bits();
            }
            0b110 => {
                let tile_pattern = self.temp_tile_pattern_index;
                self.temp_tile_pattern_hi = mapper
                    .read(self.tile_pattern_address(tile_pattern, BitPlane::Hi))
                    .reverse_bits();
            }
            0b111 => {
                let packed_pattern =
                    morton_encode_16(self.temp_tile_pattern_lo, self.temp_tile_pattern_hi);
                self.tile_pattern_shift_reg |= u32::from(packed_pattern) << 16;
                self.tile_attribute_latch = self.tile_attribute_bits_from_temp();
                self.increment_coarse_x();
            }
            _ => {}
        }
    }

    fn cur_oam_byte(&self) -> u8 {
        self.oam_bytes()[self.oam_evaluation_index as usize]
    }

    fn cur_secondary_oam_byte_mut(&mut self) -> &mut u8 {
        let i = self.secondary_oam_evaluation_index as usize;
        &mut self.secondary_oam_bytes_mut()[i]
    }

    fn is_secondary_oam_full(&self) -> bool {
        self.secondary_oam_evaluation_index as usize == self.secondary_oam_bytes().len()
    }

    fn increment_oam_evaluation_index(&mut self, n: u8) {
        (
            self.oam_evaluation_index,
            self.oam_evaluation_index_overflow,
        ) = self.oam_evaluation_index.overflowing_add(n);
    }

    fn is_sprite_y_in_range(&self, y: u8) -> bool {
        let sprite_height = self.sprite_height();
        let cur_y = self.cur_scanline as u8;
        y <= cur_y && (cur_y - y) < sprite_height
    }

    fn tick_clear_secondary_oam(&mut self) {
        if self.cur_dot & 1 == 0 {
            // writes only occur on even cycles, starting on cycle 1.
            // Therefore, cycle 2 is the first cycle where a write occurs.
            // So we can calculate the index by dividing cur_dot by 2 and subtracting 1.
            let i = ((self.cur_dot - 1) >> 1) as usize;
            self.secondary_oam_bytes_mut()[i] = self.oam_mdr;
        } else {
            self.oam_mdr = 0xFF;
        }
    }

    fn complete_sprite_evaluation(&mut self) -> SpriteEvaluationState {
        self.oam_evaluation_index &= !0b11;
        SpriteEvaluationState::Complete
    }

    fn tick_sprite_evaluation_check_normal(&mut self) {
        let sprite_y = self.oam_mdr;
        let sprite_in_range = self.is_sprite_y_in_range(sprite_y);
        *self.cur_secondary_oam_byte_mut() = sprite_y;
        if sprite_in_range {
            self.sprite_0_next_line |= self.oam_evaluation_index == 0;
            self.sprite_evaluation_state = SpriteEvaluationState::CopyingNormal(2);
            self.secondary_oam_evaluation_index += 1;
            self.increment_oam_evaluation_index(1);
        } else {
            self.increment_oam_evaluation_index(SPRITE_SIZE);
            if self.oam_evaluation_index_overflow {
                self.sprite_evaluation_state = self.complete_sprite_evaluation();
            }
        }
    }

    fn tick_sprite_evaluation_check_overflow(&mut self) {
        let sprite_y = self.oam_mdr;
        let sprite_in_range = self.is_sprite_y_in_range(sprite_y);
        if sprite_in_range {
            self.status |= PPUSTATUS_OVERFLOW;
            self.sprite_evaluation_state = SpriteEvaluationState::CopyingOverflow(2);
            self.increment_oam_evaluation_index(1);
        } else {
            // emulate overflow increment bug
            let inc = 0b101 - (((self.oam_evaluation_index & 0b11) + 1) & 0b100);
            self.increment_oam_evaluation_index(inc);
            if self.oam_evaluation_index_overflow {
                self.sprite_evaluation_state = self.complete_sprite_evaluation();
            }
        }
    }

    fn tick_sprite_evaluation_copy_normal(&mut self, remaining: u8) {
        if !self.is_secondary_oam_full() {
            *self.cur_secondary_oam_byte_mut() = self.oam_mdr;
            self.secondary_oam_evaluation_index += 1;
        }
        self.increment_oam_evaluation_index(1);

        self.sprite_evaluation_state = if remaining > 0 {
            SpriteEvaluationState::CopyingNormal(remaining - 1)
        } else if self.oam_evaluation_index_overflow {
            self.complete_sprite_evaluation()
        } else if self.is_secondary_oam_full() {
            SpriteEvaluationState::CheckingOverflow
        } else {
            SpriteEvaluationState::CheckingNormal
        };
    }

    fn tick_sprite_evaluation_copy_overflow(&mut self, remaining: u8) {
        self.increment_oam_evaluation_index(1);

        self.sprite_evaluation_state = if remaining > 0 {
            SpriteEvaluationState::CopyingOverflow(remaining - 1)
        } else {
            self.complete_sprite_evaluation()
        };
    }

    fn tick_sprite_evaluation_complete(&mut self) {
        self.increment_oam_evaluation_index(SPRITE_SIZE);
    }

    fn tick_sprite_evaluation(&mut self) {
        use SpriteEvaluationState::*;
        if self.cur_dot & 1 == 0 {
            match self.sprite_evaluation_state {
                CheckingNormal => self.tick_sprite_evaluation_check_normal(),
                CheckingOverflow => self.tick_sprite_evaluation_check_overflow(),
                CopyingNormal(remaining) => self.tick_sprite_evaluation_copy_normal(remaining),
                CopyingOverflow(remaining) => self.tick_sprite_evaluation_copy_overflow(remaining),
                Complete => self.tick_sprite_evaluation_complete(),
            }
        } else {
            self.oam_mdr = self.cur_oam_byte();
        }
    }

    fn fetch_sprite_pattern<M: Mapper>(
        &self,
        mapper: &mut M,
        sprite: Sprite,
        plane: BitPlane,
    ) -> u8 {
        let addr = self.sprite_pattern_address(sprite, plane);
        let pattern = mapper.read(addr);

        if !self.is_sprite_y_in_range(sprite.y) {
            0x00
        } else if sprite.attributes & SPRITE_FLIP_X == 0 {
            pattern.reverse_bits()
        } else {
            pattern
        }
    }

    fn tick_sprite_fetches<M: Mapper>(&mut self, mapper: &mut M) {
        // each sprite fetch takes a total of 8 cycles
        // sprite fetches begin on cycle 257, so subtract 1 and shift down 3
        // to get index of current sprite.
        let sprite_index = (((self.cur_dot - 1) >> 3) & 0b111) as usize;
        let sprite = self.secondary_oam[sprite_index];

        // default read value used for cycles 4-8
        self.oam_mdr = sprite.x;

        match (self.cur_dot - 1) & 0b111 {
            0b000 => {
                // dummy read
                mapper.read(self.nametable_address());
                self.oam_mdr = sprite.y;
            }
            0b001 => {
                self.oam_mdr = sprite.pattern_index;
            }
            0b010 => {
                // dummy read
                mapper.read(self.nametable_address());
                self.oam_mdr = sprite.attributes;
                self.sprite_render_states[sprite_index].attributes = sprite.attributes;
            }
            0b011 => {
                self.sprite_render_states[sprite_index].x_counter = sprite.x;
            }
            0b100 => {
                self.temp_sprite_pattern_lo =
                    self.fetch_sprite_pattern(mapper, sprite, BitPlane::Lo);
            }
            0b110 => {
                let pattern_hi = self.fetch_sprite_pattern(mapper, sprite, BitPlane::Hi);
                let pattern = morton_encode_16(self.temp_sprite_pattern_lo, pattern_hi);
                self.sprite_render_states[sprite_index].pattern_shift_reg = pattern;
            }
            _ => {}
        }
    }

    fn tick_sprites<M: Mapper>(&mut self, mapper: &mut M) {
        match self.cur_dot {
            0 => {
                self.secondary_oam_evaluation_index = 0;
                self.sprite_evaluation_state = SpriteEvaluationState::CheckingNormal;
                self.oam_evaluation_index_overflow = false;
                self.sprite_0_cur_line = self.sprite_0_next_line;
                self.sprite_0_next_line = false;
            }
            1..=64 => {
                self.tick_clear_secondary_oam();
            }
            65..=256 => {
                self.tick_sprite_evaluation();
            }
            257..=320 => {
                self.oam_evaluation_index = 0;
                self.tick_sprite_fetches(mapper);
            }
            321..=340 => {
                self.oam_mdr = self.secondary_oam_bytes()[0];
            }
            _ => {}
        }
    }

    fn update_tile_shift_regs(&mut self) {
        self.tile_pattern_shift_reg >>= 2;
        self.tile_attribute_shift_reg >>= 2;
        self.tile_attribute_shift_reg |= u16::from(self.tile_attribute_latch) << 14;
    }

    fn update_sprite_counters_and_shift_regs(&mut self) {
        for render_state in &mut self.sprite_render_states {
            if render_state.x_counter > 0 {
                render_state.x_counter -= 1;
            } else {
                render_state.pattern_shift_reg >>= 2;
            }
        }
    }

    fn tick_render<M: Mapper, B: PixelBuffer>(
        &mut self,
        mapper: &mut M,
        buffer: &mut B,
        mode: RenderMode,
    ) {
        if !self.is_rendering_enabled() {
            if self.cur_dot < 256 && mode == RenderMode::Normal {
                let color = self
                    .access_palette_ram(self.v_reg & 0x3FFF)
                    .unwrap_or(self.background_color());
                self.output_pixel(buffer, color);
            }
            return;
        }

        if matches!(self.cur_dot, 1..=256 | 321..=336) {
            self.update_tile_shift_regs();
            self.tick_tile_pipeline(mapper);
        }

        if mode == RenderMode::Normal {
            self.tick_sprites(mapper);
            if self.cur_dot < 256 {
                let pixel_info = self.calculate_cur_pixel();
                if pixel_info.sprite_0_hit {
                    self.status |= PPUSTATUS_SPRITE_0_HIT;
                }
                self.output_pixel(buffer, pixel_info.color);
                self.update_sprite_counters_and_shift_regs();
            }
        } else if self.cur_dot > 256 {
            // garbage sprite fetching during pre-render scanline
            self.tick_sprites(mapper);
        }

        match self.cur_dot {
            1 if mode == RenderMode::PreRender => {
                self.status &= !(PPUSTATUS_SPRITE_0_HIT | PPUSTATUS_OVERFLOW | PPUSTATUS_VBLANK);
            }
            256 => {
                self.increment_y();
            }
            257 => {
                self.flush_horizontal_scroll();
            }
            280..=304 if mode == RenderMode::PreRender => {
                self.flush_vertical_scroll();
            }
            337 | 339 => {
                // dummy reads
                mapper.read(self.nametable_address());
            }
            _ => {}
        }
    }

    fn tick_vblank(&mut self) {
        if self.cur_scanline == 241 && self.cur_dot == 1 {
            self.status |= PPUSTATUS_VBLANK;
        }
    }
}
