// Copyright foobles 2023.
//
// This work is licensed under the Creative Commons Attribution-NonCommercial 4.0
// International License. To view a copy of this license, visit
// http://creativecommons.org/licenses/by-nc/4.0/ or send a letter to Creative
// Commons, PO Box 1866, Mountain View, CA 94042, USA.

extern crate alloc;
use super::*;
use alloc::vec::Vec;
use core::ops::Range;

#[test]
fn test_tile_accesses_nametable_0() {
    let mut ppu = Ppu::new();
    let mut logs = AccessLogMapper::new();
    ppu.write_scroll(64);
    ppu.write_scroll(35);
    ppu.write_ctrl(0b00_0_00);
    ppu.write_mask(0b000_11_11_0);

    logs.record_accesses(&mut ppu, 0, 1..9);
    logs.assert_reads_eq(&[
        (1, 0b10_00_00100_01010),
        (3, 0b10_00_1111_001_010),
        (5, 0x0003),
        (7, 0x000B),
    ]);
}

#[test]
fn test_tile_accesses_nametable_1() {
    let mut ppu = Ppu::new();
    let mut logs = AccessLogMapper::new();
    ppu.write_scroll(64);
    ppu.write_scroll(35);
    ppu.write_ctrl(0b10_0_01);
    ppu.write_mask(0b000_11_11_0);

    logs.record_accesses(&mut ppu, 0, 33..41);
    logs.assert_reads_eq(&[
        (33, 0b10_01_00100_01110),
        (35, 0b10_01_1111_001_011),
        (37, 0x1003),
        (39, 0x100B),
    ]);
}

#[test]
fn test_y_increment_and_x_scroll_reload() {
    let mut ppu = Ppu::new();
    let mut logs = AccessLogMapper::new();
    ppu.write_scroll(64);
    ppu.write_scroll(35);
    ppu.write_ctrl(0b10_0_11);
    ppu.write_mask(0b000_11_11_0);

    logs.record_accesses(&mut ppu, 4, 1..9);
    logs.record_accesses(&mut ppu, 5, 1..9);
    logs.assert_reads_eq(&[
        (1, 0b10_11_00100_01010),
        (3, 0b10_11_1111_001_010),
        (5, 0x1007),
        (7, 0x100F),
        (1, 0b10_11_00101_01010),
        (3, 0b10_11_1111_001_010),
        (5, 0x1000),
        (7, 0x1008),
    ]);
}

#[derive(PartialEq, Eq)]
struct ReadEntry {
    dot: u16,
    address: u16,
}

#[derive(PartialEq, Eq)]
struct WriteEntry {
    dot: u16,
    address: u16,
    value: u8,
}

#[derive(Default)]
struct AccessLogMapper {
    writes: Vec<WriteEntry>,
    reads: Vec<ReadEntry>,
    dot: u16,
}

impl AccessLogMapper {
    fn new() -> Self {
        AccessLogMapper::default()
    }

    fn record_accesses(&mut self, ppu: &mut Ppu, scanline: u16, dots: Range<u16>) {
        assert!(scanline <= 261);
        assert!(dots.start <= dots.end);
        assert!(dots.start < 341 && dots.end < 341);
        let mut dummy_buf = DummyPixelBuffer {};
        let mut dummy_mapper = DummyMapper {};
        while ppu.cur_scanline != scanline || ppu.cur_dot < dots.start {
            ppu.tick(&mut dummy_mapper, &mut dummy_buf);
        }
        while ppu.cur_dot < dots.end {
            self.dot = ppu.cur_dot;
            ppu.tick(self, &mut dummy_buf);
        }
    }

    fn assert_reads_eq(&self, expectations: &[(u16, u16)]) {
        assert_eq!(
            self.reads.len(),
            expectations.len(),
            "Expected number of reads must match recorded number of reads."
        );
        for (log, expectation) in self.reads.iter().zip(expectations) {
            let (expected_dot, expected_address) = *expectation;
            assert_eq!(log.dot, expected_dot, "Read dot mismatch");
            assert_eq!(
                log.address, expected_address,
                "Address mismatch at dot {}: got ${:04X}, expected ${:04X}",
                log.dot, log.address, expected_address
            );
        }
    }
}

impl Mapper for AccessLogMapper {
    fn read(&mut self, address: u16) -> u8 {
        self.reads.push(ReadEntry {
            dot: self.dot,
            address,
        });
        0
    }

    fn write(&mut self, address: u16, value: u8) {
        self.writes.push(WriteEntry {
            dot: self.dot,
            address,
            value,
        })
    }
}

struct DummyPixelBuffer {}

impl DummyPixelBuffer {
    fn tick_to_position(ppu: &mut Ppu, mapper: &mut impl Mapper, dot: u16, scanline: u16) {
        assert!(dot < 341);
        assert!(scanline < 262);
        let mut buf = DummyPixelBuffer {};
        while ppu.cur_dot != dot || ppu.cur_scanline != scanline {
            ppu.tick(mapper, &mut buf);
        }
    }
}

impl PixelBuffer for DummyPixelBuffer {
    fn set_color(&mut self, _: u8, _: u8, _: Color, _: ColorEmphasis) {}
}

struct DummyMapper {}

impl Mapper for DummyMapper {
    fn read(&mut self, _: u16) -> u8 {
        0
    }
    fn write(&mut self, _: u16, _: u8) {}
}
