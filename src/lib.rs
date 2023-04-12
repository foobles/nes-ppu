// Copyright foobles 2023.
//
// This work is licensed under the Creative Commons Attribution-NonCommercial 4.0
// International License. To view a copy of this license, visit
// http://creativecommons.org/licenses/by-nc/4.0/ or send a letter to Creative
// Commons, PO Box 1866, Mountain View, CA 94042, USA.

use bytemuck::{Pod, Zeroable};

pub type Color = u8;

#[derive(Debug, Copy, Clone, Default)]
pub struct Palette {
    pub colors: [Color; 3],
}

const SPRITE_SIZE: u8 = 4;

#[derive(Debug, Copy, Clone, Default, Pod, Zeroable)]
#[repr(C)]
pub struct Sprite {
    pub y: u8,
    pub pattern_index: u8,
    pub attributes: u8,
    pub x: u8,
}

#[derive(Debug)]
pub struct Ppu {
    oam: [Sprite; 64],
    pub sprite_palettes: [Palette; 4],
    pub tile_palettes: [Palette; 4],
    pub background_color: Color,

    ctrl: u8,
    mask: u8,

    t_reg: u16,
    v_reg: u16,
    fine_x_scroll: u8, // 3 bits,
    w_latch: bool,

    secondary_oam: [Sprite; 8],
    sprite_render_states: [SpriteRenderState; 8],
    oam_evaluation_index: u8,
    secondary_oam_evaluation_index: u8,
    is_sprite_evaluation_complete: bool,
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

pub trait Mapper {
    fn read(&mut self, addr: u16) -> u8; // yes this needs to be mut
    fn write(&mut self, addr: u16, value: u8);
}

pub trait PpuPixelBuffer {
    fn set_color(&mut self, x: u8, y: u8, c: u8);
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

pub const PPUCTRL_ADDR_INC: u8 = 1 << 2;
pub const PPUCTRL_SPRITE_PATTERN_TABLE: u8 = 1 << 3;
pub const PPUCTRL_TILE_PATTERN_TABLE: u8 = 1 << 4;
pub const PPUCTRL_SPRITE_SIZE: u8 = 1 << 5;
pub const PPUCTRL_MSS: u8 = 1 << 6;
pub const PPUCTRL_NMI_ENABLE: u8 = 1 << 7;

pub const SPRITE_PALETTE_MASK: u8 = 0b00000011;
pub const SPRITE_PRIORITY: u8 = 0b00100000;
pub const SPRITE_FLIP_X: u8 = 0b01000000;
pub const SPRITE_FLIP_Y: u8 = 0b10000000;

pub const X_SCROLL_MASK: u16 = 0b000_01_00000_11111;
pub const Y_SCROLL_MASK: u16 = 0b111_10_11111_00000;

pub const COARSE_X_SCROLL_MASK: u16 = 0b000_00_00000_11111;
pub const FINE_Y_SCROLL_MASK: u16 = 0b111_00_00000_00000;
pub const COARSE_Y_SCROLL_MASK: u16 = 0b000_00_11111_00000;

pub const COARSE_X_OFFSET: u16 = 0;
pub const FINE_Y_OFFSET: u16 = 12;
pub const COARSE_Y_OFFSET: u16 = 5;

pub const NAMETABLE_SELECT_MASK: u16 = 0b000_11_00000_00000;
pub const NAMETABLE_X_SELECT_MASK: u16 = 0b000_01_00000_00000;
pub const NAMETABLE_Y_SELECT_MASK: u16 = 0b000_10_00000_00000;

pub const NAMETABLE_SELECT_OFFSET: u16 = 10;

pub const NAMETABLE_BASE_ADDRESS: u16 = 0b10_00_0000_000000;
pub const ATTRIBUTE_TABLE_OFFSET: u16 = 0b00_00_1111_000000;

#[repr(u16)]
pub enum BitPlane {
    Lo = 0,
    Hi = 8,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            oam: [Sprite::default(); 64],
            sprite_palettes: [Palette::default(); 4],
            tile_palettes: [Palette::default(); 4],
            background_color: 0,
            ctrl: 0,
            mask: 0,
            t_reg: 0,
            v_reg: 0,
            fine_x_scroll: 0,
            w_latch: false,
            secondary_oam: [Sprite::default(); 8],
            sprite_render_states: [SpriteRenderState::default(); 8],
            oam_evaluation_index: 0,
            secondary_oam_evaluation_index: 0,
            is_sprite_evaluation_complete: false,
            temp_sprite_pattern_lo: 0,
            tile_pattern_shift_reg: 0,
            tile_attribute_shift_reg: 0,
            tile_attribute_latch: 0,
            temp_tile_pattern_index: 0,
            temp_tile_attribute: 0,
            temp_tile_pattern_lo: 0,
            temp_tile_pattern_hi: 0,
            cur_scanline: 0,
            cur_dot: 0,
        }
    }

    pub fn oam_dma<M: Mapper>(&mut self, mapper: &mut M, page: u8) {
        let base_addr = u16::from(page) << 8;
        for (dest, addr) in self
            .oam_bytes_mut()
            .iter_mut()
            .zip(base_addr..=base_addr + 255)
        {
            *dest = mapper.read(addr);
        }
    }

    pub fn set_oam(&mut self, sprites: [Sprite; 64]) {
        self.oam = sprites;
    }

    pub fn set_oam_bytes(&mut self, bytes: [u8; 256]) {
        self.oam_bytes_mut().copy_from_slice(&bytes);
    }

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

    pub fn set_ctrl(&mut self, b: u8) {
        // TODO: bit 0 race condition
        assert_eq!((b & PPUCTRL_MSS), 0, "PPUCTRL bit 6 set; system short");
        self.ctrl = b;
        let nametable_bits = u16::from(b & 0b00000011);
        self.t_reg &= !NAMETABLE_SELECT_MASK;
        self.t_reg |= nametable_bits << NAMETABLE_SELECT_OFFSET;
    }

    pub fn set_mask(&mut self, b: u8) {
        self.mask = b;
    }

    fn address_increment(&self) -> u16 {
        match self.ctrl & PPUCTRL_ADDR_INC {
            0 => 1,
            _ => 32,
        }
    }

    fn sprite_height(&self) -> u8 {
        match self.ctrl & PPUCTRL_SPRITE_SIZE {
            0 => 8,
            _ => 16,
        }
    }

    fn is_nmi_enabled(&self) -> bool {
        self.ctrl & PPUCTRL_NMI_ENABLE != 0
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

    pub fn output_pixel<B: PpuPixelBuffer>(&self, buffer: &mut B) {
        let tile_attribute = (self.tile_attribute_shift_reg >> (2 * self.fine_x_scroll)) & 0b11;
        let tile_color_index = (self.tile_pattern_shift_reg >> (2 * self.fine_x_scroll)) & 0b11;

        let visible_sprite = self
            .sprite_render_states
            .iter()
            .filter(|&s| s.x_counter == 0 && s.pattern_shift_reg & 0b11 != 0)
            .next();

        let cur_pixel_color = match (visible_sprite, tile_color_index) {
            (Some(s), i) if i == 0 || s.attributes & SPRITE_PRIORITY == 0 => {
                let sprite_palette_index = (s.attributes & SPRITE_PALETTE_MASK) as usize;
                let sprite_color_index = ((s.pattern_shift_reg & 0b11) - 1) as usize;
                self.sprite_palettes[sprite_palette_index].colors[sprite_color_index]
            }
            (_, 0) => self.background_color,
            (_, i) => self.tile_palettes[tile_attribute as usize].colors[(i - 1) as usize],
        };

        buffer.set_color(self.cur_dot as u8, self.cur_scanline as u8, cur_pixel_color);
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
            self.is_sprite_evaluation_complete,
        ) = self.oam_evaluation_index.overflowing_add(n);
    }

    fn is_sprite_y_in_range(&self, y: u8) -> bool {
        let sprite_height = self.sprite_height();
        let cur_y = self.cur_scanline as u8;
        y <= cur_y && (cur_y - y) < sprite_height
    }

    fn tick_clear_secondary_oam(&mut self) {
        *self.cur_secondary_oam_byte_mut() = 0xFF;
        self.secondary_oam_evaluation_index += 1;
        self.secondary_oam_evaluation_index &= 0b00011111;
    }

    fn tick_sprite_evaluation(&mut self) {
        // todo: sprite overflow checking
        if self.is_secondary_oam_full() || self.is_sprite_evaluation_complete {
            return;
        }

        match self.oam_evaluation_index & 0b11 {
            0b00 => {
                let sprite_y = self.cur_oam_byte();
                *self.cur_secondary_oam_byte_mut() = sprite_y;

                if self.is_sprite_y_in_range(sprite_y) {
                    self.increment_oam_evaluation_index(1);
                    self.secondary_oam_evaluation_index += 1;
                } else {
                    self.increment_oam_evaluation_index(SPRITE_SIZE);
                }
            }
            _ => {
                *self.cur_secondary_oam_byte_mut() = self.cur_oam_byte();
                self.increment_oam_evaluation_index(1);
                self.secondary_oam_evaluation_index += 1;
            }
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

        match (self.cur_dot - 1) & 0b110 {
            0b000 => {
                // dummy nametable fetch 1
            }
            0b010 => {
                // dummy nametable fetch 2
            }
            0b100 => {
                self.temp_sprite_pattern_lo =
                    self.fetch_sprite_pattern(mapper, sprite, BitPlane::Lo);

                let cur_render_state = &mut self.sprite_render_states[sprite_index];
                cur_render_state.attributes = sprite.attributes;
            }
            0b110 => {
                let pattern_hi = self.fetch_sprite_pattern(mapper, sprite, BitPlane::Hi);
                let pattern = morton_encode_16(self.temp_sprite_pattern_lo, pattern_hi);

                let cur_render_state = &mut self.sprite_render_states[sprite_index];
                cur_render_state.pattern_shift_reg = pattern;
                cur_render_state.x_counter = sprite.x;
            }
            _ => {}
        }
    }

    fn tick_sprites<M: Mapper>(&mut self, mapper: &mut M) {
        let even_cycle = self.cur_dot & 1 == 0;

        match self.cur_dot {
            0 => {
                self.oam_evaluation_index = 0;
                self.secondary_oam_evaluation_index = 0;
                self.is_sprite_evaluation_complete = false;
            }
            1..=64 if even_cycle => {
                self.tick_clear_secondary_oam();
            }
            65..=256 if even_cycle => {
                self.tick_sprite_evaluation();
            }
            257..=320 => {
                self.tick_sprite_fetches(mapper);
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

    fn tick_render<M: Mapper, B: PpuPixelBuffer>(
        &mut self,
        mapper: &mut M,
        buffer: &mut B,
        mode: RenderMode,
    ) {
        if mode == RenderMode::Normal {
            if self.cur_dot < 256 {
                self.output_pixel(buffer);
                self.update_sprite_counters_and_shift_regs();
            }
            self.tick_sprites(mapper);
        }

        match self.cur_dot {
            0 => {
                self.update_tile_shift_regs();
            }
            1..=256 | 321..=335 => {
                self.tick_tile_pipeline(mapper);
                self.update_tile_shift_regs();
            }
            336 => {
                self.tick_tile_pipeline(mapper);
            }
            _ => {}
        }

        match self.cur_dot {
            256 => {
                self.increment_y();
            }
            257 => {
                self.flush_horizontal_scroll();
            }
            280..=304 if mode == RenderMode::PreRender => {
                self.flush_vertical_scroll();
            }
            _ => {}
        }
    }

    fn tick_vblank(&mut self) {}

    pub fn tick<M: Mapper, B: PpuPixelBuffer>(&mut self, mapper: &mut M, buffer: &mut B) {
        match self.cur_scanline {
            0..=239 => self.tick_render(mapper, buffer, RenderMode::Normal), // render
            240..=260 => self.tick_vblank(),                                 // vblank
            261 => self.tick_render(mapper, buffer, RenderMode::PreRender),  // pre render line
            _ => unreachable!(),
        }

        if self.cur_dot < 340 {
            self.cur_dot += 1;
        } else {
            self.cur_dot = 0;
            self.cur_scanline = (self.cur_scanline + 1) % 262;
        }
    }
}
