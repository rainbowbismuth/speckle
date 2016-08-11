// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum Size {
    Byte = 0,
    Word,
    Double,
    Quad
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum Scale {
    One = 0,
    Two,
    Four,
    Eight
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum Mod {
    Indirect = 0,
    OneByteDisp,
    FourByteDisp,
    Register,
}

#[derive(Copy, Clone, Debug)]
pub struct RegisterInfo {
    pub size: Size,
    pub code: u8,
    pub needs_extension: bool,
    pub needs_any_rex: bool,
}

pub struct InstructionBuilder {
    cur: usize,
    buf: [u8; 16],
}

impl InstructionBuilder {
    pub fn new() -> Self {
        use std::mem;
        InstructionBuilder {
            cur: 0,
            buf: unsafe { mem::uninitialized() }
        }
    }

    pub fn build(&self) -> &[u8] {
        &self.buf[0..self.cur]
    }

    pub fn emit(&mut self, byte: u8) {
        self.buf[self.cur] = byte;
        self.cur += 1;
    }

    pub fn emit_operand_size_prefix(&mut self) {
        self.emit(0x66);
    }

    pub fn emit_rex(&mut self, rm_ext: bool, sib_ext: bool, reg_ext: bool, r64: bool) {
        self.emit(0x40 + rm_ext as u8 + (sib_ext as u8) << 1 + (reg_ext as u8) << 2 + (r64 as u8) << 3);
    }

    pub fn emit_mod_reg_rm(&mut self, modb: Mod, reg: u8, rm: u8) {
        assert!(reg <= 0b111);
        assert!(rm <= 0b111);
        self.emit(rm + reg << 3 + (modb as u8) << 6);
    }

    pub fn emit_sib(&mut self, scale: Scale, index: u8, base: u8) {
        assert!(index <= 0b111);
        assert!(base <= 0b111);
        self.emit(base + index << 3 + (scale as u8) << 6);
    }

    pub fn emit_imm_i8(&mut self, imm: i8) {
        self.emit(imm as u8);
    }

    pub fn emit_imm_i16(&mut self, imm: i16) {
        self.emit((imm & 0xFF) as u8);
        self.emit(((imm >> 8) & 0xFF) as u8);
    }

    pub fn emit_imm_i32(&mut self, imm: i32) {
        self.emit((imm & 0xFF) as u8);
        self.emit(((imm >> 8) & 0xFF) as u8);
        self.emit(((imm >> 16) & 0xFF) as u8);
        self.emit(((imm >> 24) & 0xFF) as u8);
    }
}

pub struct Assembler<'a> {
    cur: usize,
    buffer: &'a mut [u8]
}

impl<'a> Assembler<'a> {
    pub fn new(buffer: &'a mut [u8]) -> Self {
        Assembler { cur: 0, buffer: buffer }
    }

    pub fn code(self) -> &'a mut [u8] {
        &mut self.buffer[0..self.cur]
    }

    pub fn emit(&mut self, byte: u8) {
        self.buffer[self.cur] = byte;
        self.cur += 1;
    }

    pub fn append(&mut self, ib: &InstructionBuilder) {
        for byte in ib.build() {
            self.emit(*byte);
        }
    }
}

pub trait RegCode : Copy {
    fn code(self) -> u8;
    fn needs_ext(self) -> bool;
    fn needs_rex(self) -> bool;
}

pub trait Reg8 : RegCode {}

pub trait Reg16 : RegCode {}

pub trait Reg32 : RegCode {}

pub trait Reg64 : RegCode {}
