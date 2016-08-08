// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::ptr;
use std::mem;
use libc;
use std::ops::{Add, Sub};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Reg64 {
    RAX = 0,
    RCX, // 1
    RDX, // 2
    RBX, // 3
    RSP, // 4 Makes SIB happen except when Mod == 0b11
    RBP, // 5 Is RIP when Mod == 0b00
    RSI, // 6
    RDI, // 7
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Clone, Copy)]
pub struct RIP; // Can't leave out this guy

impl Reg64 {
    pub fn requires_ext(self) -> bool {
        (self as u8) > 7
    }

    pub fn code(self) -> u8 {
        (self as u8) & 0b111
    }

    pub fn wants_sib(self, modb: Mod) -> bool {
        // RSP and R12 make SIB happen unless Mod::Register
        self.code() == Reg64::RSP.code() && modb != Mod::Register
    }

    pub fn is_rip(self, modb: Mod) -> bool {
        // Is RIP when RBP or R13, and Mod::Indirect
        self.code() == Reg64::RBP.code() && modb == Mod::Indirect
    }

    pub fn ptr(self) -> Reg64Deref {
        Reg64Deref(self)
    }

    pub fn x1(self) -> Reg64IdxScale {
        Reg64IdxScale(self, Scale::One)
    }

    pub fn x2(self) -> Reg64IdxScale {
        Reg64IdxScale(self, Scale::Two)
    }

    pub fn x4(self) -> Reg64IdxScale {
        Reg64IdxScale(self, Scale::Four)
    }

    pub fn x8(self) -> Reg64IdxScale {
        Reg64IdxScale(self, Scale::Eight)
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Scale {
    One = 0,
    Two,
    Four,
    Eight,
}

impl Scale {
    pub fn code(self) -> u8 {
        self as u8
    }
}

#[derive(Clone, Copy)]
pub struct Reg64Deref(Reg64);

#[derive(Clone, Copy)]
pub struct Reg64Displacement(Reg64, i32);

#[derive(Clone, Copy)]
pub struct Reg64IdxScale(Reg64, Scale);

#[derive(Clone, Copy)]
pub struct Reg64BaseIdxScale(Reg64, Reg64, Scale);

#[derive(Clone, Copy)]
pub struct Reg64BaseIdxScaleDisplacement(Reg64, Reg64, Scale, i32);

impl Add<i32> for Reg64 {
    type Output = Reg64Displacement;
    fn add(self, rhs: i32) -> Self::Output {
        Reg64Displacement(self, rhs)
    }
}

impl Sub<i32> for Reg64 {
    type Output = Reg64Displacement;
    fn sub(self, rhs: i32) -> Self::Output {
        Reg64Displacement(self, -rhs)
    }
}

impl Add<Reg64IdxScale> for Reg64 {
    type Output = Reg64BaseIdxScale;

    fn add(self, rhs: Reg64IdxScale) -> Self::Output {
        let Reg64IdxScale(idx, scale) = rhs;
        Reg64BaseIdxScale(self, idx, scale)
    }
}

impl Add<i32> for Reg64BaseIdxScale {
    type Output = Reg64BaseIdxScaleDisplacement;

    fn add(self, rhs: i32) -> Self::Output {
        let Reg64BaseIdxScale(base, idx, scale) = self;
        Reg64BaseIdxScaleDisplacement(base, idx, scale, rhs)
    }
}

impl Sub<i32> for Reg64BaseIdxScale {
    type Output = Reg64BaseIdxScaleDisplacement;

    fn sub(self, rhs: i32) -> Self::Output {
        let Reg64BaseIdxScale(base, idx, scale) = self;
        Reg64BaseIdxScaleDisplacement(base, idx, scale, -rhs)
    }
}

#[derive(Clone, Copy)]
pub enum ArithQSrc {
    Imm(i32),
    Disp(i32),
    Reg64(Reg64),
    Reg64Deref(Reg64Deref),
    Reg64Displacement(Reg64Displacement),
    Reg64BaseIdxScale(Reg64BaseIdxScale),
    Reg64BaseIdxScaleDisplacement(Reg64BaseIdxScaleDisplacement),
}

#[derive(Clone, Copy)]
pub struct Disp(i32);

#[derive(Clone, Copy)]
pub enum ArithQDest {
    Disp(i32),
    Reg64(Reg64),
    Reg64Deref(Reg64Deref),
    Reg64Displacement(Reg64Displacement),
    Reg64BaseIdxScale(Reg64BaseIdxScale),
    Reg64BaseIdxScaleDisplacement(Reg64BaseIdxScaleDisplacement),
}

impl From<i32> for ArithQSrc {
    fn from(imm: i32) -> Self {
        ArithQSrc::Imm(imm)
    }
}

impl From<Disp> for ArithQSrc {
    fn from(disp: Disp) -> Self {
        ArithQSrc::Disp(disp.0)
    }
}

impl From<Reg64> for ArithQSrc {
    fn from(reg: Reg64) -> Self {
        ArithQSrc::Reg64(reg)
    }
}

impl From<Reg64Deref> for ArithQSrc {
    fn from(reg: Reg64Deref) -> Self {
        ArithQSrc::Reg64Deref(reg)
    }
}

impl From<Reg64Displacement> for ArithQSrc {
    fn from(reg: Reg64Displacement) -> Self {
        ArithQSrc::Reg64Displacement(reg)
    }
}

impl From<Reg64BaseIdxScale> for ArithQSrc {
    fn from(reg: Reg64BaseIdxScale) -> Self {
        ArithQSrc::Reg64BaseIdxScale(reg)
    }
}

impl From<Reg64BaseIdxScaleDisplacement> for ArithQSrc {
    fn from(reg: Reg64BaseIdxScaleDisplacement) -> Self {
        ArithQSrc::Reg64BaseIdxScaleDisplacement(reg)
    }
}

impl From<Reg64> for ArithQDest {
    fn from(reg: Reg64) -> Self {
        ArithQDest::Reg64(reg)
    }
}

impl From<Reg64Deref> for ArithQDest {
    fn from(reg: Reg64Deref) -> Self {
        ArithQDest::Reg64Deref(reg)
    }
}

impl From<Reg64Displacement> for ArithQDest {
    fn from(reg: Reg64Displacement) -> Self {
        ArithQDest::Reg64Displacement(reg)
    }
}

impl From<Reg64BaseIdxScale> for ArithQDest {
    fn from(reg: Reg64BaseIdxScale) -> Self {
        ArithQDest::Reg64BaseIdxScale(reg)
    }
}

impl From<Reg64BaseIdxScaleDisplacement> for ArithQDest {
    fn from(reg: Reg64BaseIdxScaleDisplacement) -> Self {
        ArithQDest::Reg64BaseIdxScaleDisplacement(reg)
    }
}

impl From<Disp> for ArithQDest {
    fn from(disp: Disp) -> Self {
        ArithQDest::Disp(disp.0)
    }
}

// For instructions like 0x80, 0x81, 0x83
#[derive(Copy, Clone)]
#[repr(u8)]
pub enum ArithInstr {
    ADD = 0,
    OR,
    ADC,
    SBB,
    AND,
    SUB,
    XOR,
    CMP,
}

impl ArithInstr {
    pub fn code(self) -> u8 {
        self as u8
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Mod {
    Indirect = 0,
    OneByteDisp,
    FourByteDisp,
    Register,
}

impl Mod {
    pub fn code(self) -> u8 {
        self as u8
    }
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum RegDir {
    Src = 0b00,
    Dest = 0b10,
}

impl RegDir {
    pub fn code(self) -> u8 {
        self as u8
    }
}

pub trait ArithQCompatible<Dest> {}
impl<T> ArithQCompatible<Reg64> for T where T: Into<ArithQSrc> {}
impl ArithQCompatible<Disp> for i32 {}
impl ArithQCompatible<Reg64Deref> for i32 {}
impl ArithQCompatible<Reg64Displacement> for i32 {}
impl ArithQCompatible<Reg64BaseIdxScale> for i32 {}
impl ArithQCompatible<Reg64BaseIdxScaleDisplacement> for i32 {}
impl ArithQCompatible<Disp> for Reg64 {}
impl ArithQCompatible<Reg64Deref> for Reg64 {}
impl ArithQCompatible<Reg64Displacement> for Reg64 {}
impl ArithQCompatible<Reg64BaseIdxScale> for Reg64 {}
impl ArithQCompatible<Reg64BaseIdxScaleDisplacement> for Reg64 {}


struct InstructionBuilder {
    cur: usize,
    buf: [u8; 16],
}

impl InstructionBuilder {
    fn new() -> Self {
        InstructionBuilder {
            cur: 0,
            buf: unsafe { mem::uninitialized() }
        }
    }

    fn build(&self) -> &[u8] {
        &self.buf[0..self.cur]
    }

    fn emit(&mut self, byte: u8) {
        self.buf[self.cur] = byte;
        self.cur += 1;
    }

    fn emit_rex_w(&mut self, rm_extension: bool, sib_extension: bool, reg_extension: bool) {
        self.emit(0x48 + rm_extension as u8 + ((sib_extension as u8) << 1) +
                  ((reg_extension as u8) << 2));
    }

    fn emit_mod_reg_rm(&mut self, modb: Mod, reg: u8, rm: u8) {
        assert!(reg <= 0b111);
        assert!(rm <= 0b111);
        self.emit(rm + (reg << 3) + (modb.code() << 6));
    }

    fn emit_sib(&mut self, scale: Scale, index: u8, base: u8) {
        assert!(index <= 0b111);
        assert!(base <= 0b111);
        self.emit(base + (index << 3) + (scale.code() << 6));
    }

    fn emit_imm_i32(&mut self, imm: i32) {
        self.emit((imm & 0xFF) as u8);
        self.emit(((imm >> 8) & 0xFF) as u8);
        self.emit(((imm >> 16) & 0xFF) as u8);
        self.emit(((imm >> 24) & 0xFF) as u8);
    }
}

pub struct CodeEmitter<'a> {
    cur: usize,
    buffer: &'a mut [u8]
}

impl<'a> CodeEmitter<'a> {
    pub fn new(buffer: &'a mut [u8]) -> Self {
        CodeEmitter { cur: 0, buffer: buffer }
    }

    pub fn code(self) -> &'a mut [u8] {
        &mut self.buffer[0..self.cur]
    }

    fn emit(&mut self, byte: u8) {
        self.buffer[self.cur] = byte;
        self.cur += 1;
    }

    fn append(&mut self, ib: &InstructionBuilder) {
        for byte in ib.build() {
            self.emit(*byte);
        }
    }

    fn arithq_rax_imm(&mut self, arith: ArithInstr, imm: i32) {
        let mut builder = InstructionBuilder::new();
        builder.emit_rex_w(false, false, false);
        builder.emit(8 * arith.code() + 0b101);
        builder.emit_imm_i32(imm);
        self.append(&builder);
    }

    fn arithq_disp_only_reg(&mut self, arith: ArithInstr, disp: i32, reg: Reg64, dir: RegDir) {
        let mut builder = InstructionBuilder::new();
        builder.emit_rex_w(false, false, reg.requires_ext());
        builder.emit(8 * arith.code() + dir.code() + 1);
        builder.emit_mod_reg_rm(Mod::Indirect, reg.code(), 0b100);
        builder.emit_sib(Scale::One, 0b100, 0b101);
        builder.emit_imm_i32(disp);
        self.append(&builder);
    }

    fn arithq_reg64_reg64(&mut self, arith: ArithInstr, dst: Reg64, src: Reg64) {
        let mut builder = InstructionBuilder::new();
        builder.emit_rex_w(src.requires_ext(), false, dst.requires_ext());
        builder.emit(8 * arith.code() + 0b01);
        builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
        self.append(&builder);
    }

    fn arithq_reg64_reg64_deref(&mut self,
                                arith: ArithInstr,
                                reg1: Reg64,
                                dir: RegDir,
                                reg2: Reg64Deref) {
        let mut builder = InstructionBuilder::new();
        let Reg64Deref(deref_reg) = reg2;

        if deref_reg.is_rip(Mod::Indirect) {
            // avoid RIP relative addressing with deref_reg+0
            self.arithq_reg64_reg64_disp_no_zero_check(arith, reg1, dir, deref_reg + 0);
            return;
        }

        builder.emit_rex_w(deref_reg.requires_ext(), false, reg1.requires_ext());
        builder.emit(8 * arith.code() + dir.code() + 1);
        builder.emit_mod_reg_rm(Mod::Indirect, reg1.code(), deref_reg.code());

        if deref_reg.wants_sib(Mod::Indirect) {
            builder.emit_sib(Scale::One, deref_reg.code(), deref_reg.code());
        }
        self.append(&builder);
    }

    fn arithq_reg64_reg64_disp_no_zero_check(&mut self,
                                             arith: ArithInstr,
                                             reg1: Reg64,
                                             dir: RegDir,
                                             reg2: Reg64Displacement) {
        let mut builder = InstructionBuilder::new();
        let Reg64Displacement(dis_reg, disp) = reg2;
        builder.emit_rex_w(dis_reg.requires_ext(), false, reg1.requires_ext());
        builder.emit(8 * arith.code() + dir.code() + 1);

        let one_byte = (disp as i8 as i32) == disp;
        let modb = if one_byte {
            Mod::OneByteDisp
        } else {
            Mod::FourByteDisp
        };
        builder.emit_mod_reg_rm(modb, reg1.code(), dis_reg.code());

        if dis_reg.wants_sib(modb) {
            builder.emit_sib(Scale::One, dis_reg.code(), dis_reg.code());
        }

        if one_byte {
            builder.emit(disp as i8 as u8);
        } else {
            builder.emit_imm_i32(disp);
        }
        self.append(&builder);
    }

    fn arithq_reg64_reg64_disp(&mut self,
                               arith: ArithInstr,
                               reg1: Reg64,
                               dir: RegDir,
                               reg2: Reg64Displacement) {
        let Reg64Displacement(dis_reg, disp) = reg2;

        if disp == 0 {
            self.arithq_reg64_reg64_deref(arith, reg1, dir, dis_reg.ptr());
        } else {
            self.arithq_reg64_reg64_disp_no_zero_check(arith, reg1, dir, reg2);
        }
    }

    fn inst_0x81_reg64_imm(&mut self, arith: ArithInstr, dst: Reg64, imm: i32) {
        let mut builder = InstructionBuilder::new();
        builder.emit_rex_w(dst.requires_ext(), false, false);
        if (imm as i8 as i32) != imm {
            builder.emit(0x81);
            builder.emit_mod_reg_rm(Mod::Register, arith.code(), dst.code());
            builder.emit_imm_i32(imm);
        } else {
            builder.emit(0x83);
            builder.emit_mod_reg_rm(Mod::Register, arith.code(), dst.code());
            builder.emit(imm as i8 as u8);
        }
        self.append(&builder);
    }

    fn arithq_qword_disp_imm(&mut self, arith: ArithInstr, disp: i32, imm: i32) {
        let mut builder = InstructionBuilder::new();
        builder.emit_rex_w(false, false, false);
        if (imm as i8 as i32) != imm {
            builder.emit(0x81);
            builder.emit_mod_reg_rm(Mod::Indirect, arith.code(), 0b100);
            builder.emit(0x25); // our SIB byte is always 0x25
            builder.emit_imm_i32(disp);
            builder.emit_imm_i32(imm);
        } else {
            builder.emit(0x83);
            builder.emit_mod_reg_rm(Mod::Indirect, arith.code(), 0b100);
            builder.emit(0x25); // our SIB byte is always 0x25
            builder.emit_imm_i32(disp);
            builder.emit(imm as i8 as u8);
        }
        self.append(&builder);
    }

    pub fn arithq<Src, Dest>(&mut self, arith: ArithInstr, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        use self::Reg64::*;
        match src.into() {
            ArithQSrc::Disp(disp) => match dest.into() {
                ArithQDest::Reg64(dst_reg) => {
                    self.arithq_disp_only_reg(arith, disp, dst_reg, RegDir::Dest);
                },
                _ => { panic!("Unhandled dest for displacement-only src") }
            },
            ArithQSrc::Imm(imm) => match dest.into() {
                ArithQDest::Disp(disp) => {
                    self.arithq_qword_disp_imm(arith, disp, imm);
                },
                ArithQDest::Reg64(RAX) => {
                    if (imm as i8 as i32) != imm {
                        self.arithq_rax_imm(arith, imm);
                    } else {
                        self.inst_0x81_reg64_imm(arith, RAX, imm);
                    }
                },
                ArithQDest::Reg64(reg) => {
                    self.inst_0x81_reg64_imm(arith, reg, imm);
                },
                _ => { panic!("Unhandled dest for arithq") }
            },
            ArithQSrc::Reg64(src_reg) => match dest.into() {
                ArithQDest::Disp(disp) => {
                    self.arithq_disp_only_reg(arith, disp, src_reg, RegDir::Src);
                },
                ArithQDest::Reg64(dst_reg) => {
                    self.arithq_reg64_reg64(arith, src_reg, dst_reg);
                },
                ArithQDest::Reg64Deref(dst_reg) => {
                    self.arithq_reg64_reg64_deref(arith, src_reg, RegDir::Src, dst_reg);
                },
                ArithQDest::Reg64Displacement(reg_d) => {
                    self.arithq_reg64_reg64_disp(arith, src_reg, RegDir::Src, reg_d);
                },
                _ => { panic!("Unhandled dest for arithq") }
            },
            ArithQSrc::Reg64Deref(src_reg) => match dest.into() {
                ArithQDest::Reg64(dst_reg) => {
                    self.arithq_reg64_reg64_deref(arith, dst_reg, RegDir::Dest, src_reg);
                },
                _ => { panic!("Not implemented") }
            },
            ArithQSrc::Reg64Displacement(reg_d) => match dest.into() {
                ArithQDest::Reg64(dst_reg) => {
                    self.arithq_reg64_reg64_disp(arith, dst_reg, RegDir::Dest, reg_d);
                },
                _ => { panic!("unhandled") }
            },
            ArithQSrc::Reg64BaseIdxScale(reg_bis) => /*match dest.into()*/ {
                panic!("unhandled") // TODO: here
            },
            ArithQSrc::Reg64BaseIdxScaleDisplacement(reg_bis_d) => /*match dest.into()*/ {
                panic!("unhandled") // TODO: here
            }
        }
    }

    pub fn add<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::ADD, dest, src);
    }

    pub fn or<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::OR, dest, src);
    }
 
    pub fn adc<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::ADC, dest, src);
    }

    pub fn sbb<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::SBB, dest, src);
    }

    pub fn and<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::AND, dest, src);
    }

    pub fn sub<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::SUB, dest, src);
    }

    pub fn xor<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::XOR, dest, src);
    }

    pub fn cmp<Src, Dest>(&mut self, dest: Dest, src: Src)
        where Src: Into<ArithQSrc> + ArithQCompatible<Dest>,
              Dest: Into<ArithQDest>
    {
        self.arithq(ArithInstr::CMP, dest, src);
    }

    pub fn ret(&mut self) {
        self.emit(0xC3);
    }
}

pub struct Code {
    ptr: *const u8,
    size: usize,
}

impl Code {
    pub unsafe fn call(&self) {
        mem::transmute::<*const u8, extern "C" fn()>(self.ptr)();
    }

    pub unsafe fn call1(&self, arg: u64) -> u64 {
        mem::transmute::<*const u8, extern "C" fn(u64) -> u64>(self.ptr)(arg)
    }
}

impl Drop for Code {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            unsafe {
                libc::munmap(self.ptr as *mut libc::c_void, self.size);
            }
            self.ptr = ptr::null();
        }
    }
}

pub fn alloc_exec(code: &[u8]) -> Code {
    unsafe {
        let res = libc::mmap(ptr::null_mut(),
                            code.len(),
                            libc::PROT_READ | libc::PROT_WRITE,
                            libc::MAP_PRIVATE | libc::MAP_ANON,
                            -1,
                            0);
        if res.is_null() {
            panic!("Could not allocate enough memory for new executable page");
        }
        ptr::copy_nonoverlapping(code.as_ptr(), res as *mut u8, code.len());
        libc::mprotect(res, code.len(), libc::PROT_READ | libc::PROT_EXEC);

        Code {
            ptr: res as *const u8,
            size: code.len(),
        }
    }
}

#[cfg(test)]
mod test {
    extern crate test;
    extern crate regex;
    use super::*;
    use std::mem;
    use self::regex::Regex;
    use self::test::Bencher;

    #[test]
    #[cfg(all(any(unix, macos), target_arch="x86_64"))]
    fn test_calling() {
        use super::Reg64::*;
        let mut buf: [u8; 64] = unsafe { mem::uninitialized() };
        let mut c = CodeEmitter::new(&mut buf);
        c.xor(RAX, RAX);
        c.add(RAX, RDI);
        c.add(RAX, RDI);
        c.ret();
        let code = alloc_exec(c.code());
        let res = unsafe { code.call1(100) };
        assert_eq!(res, 200);
    }

    #[bench]
    fn emit_10_arith_instructions(b: &mut Bencher) {
        use super::Reg64::*;
        b.iter(|| {
            let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
            let mut c = CodeEmitter::new(&mut buf);
            c.add(RAX, RCX);
            c.sub(RSP.ptr(), R10);
            c.and(RSP, 0xFF);
            c.or(RBX, R9.ptr());
            c.sbb(RSI, R9+0x200);
            c.xor(R8+0x20, R9);
            c.and(RSP, R9);
            c.or(RBX, R9.ptr());
            c.sbb(RSI, R9+0x200);
            c.xor(R8+0x20, R9);
            test::black_box(c.code());
        });
    }

    fn str_to_reg(reg: &str) -> Reg64 {
        use super::Reg64::*;
        match reg {
            "RAX" => RAX,
            "RCX" => RCX,
            "RDX" => RDX,
            "RBX" => RBX,
            "RBP" => RBP,
            "RSI" => RSI,
            "RDI" => RDI,
            "RSP" => RSP,
            "R8" => R8,
            "R9" => R9,
            "R10" => R10,
            "R11" => R11,
            "R12" => R12,
            "R13" => R13,
            "R14" => R14,
            "R15" => R15,
            _ => panic!("bad register"),
        }
    }

    fn str_to_arith(arith: &str) -> ArithInstr {
        use super::ArithInstr::*;
        match arith {
            "ADD" => ADD,
            "OR" => OR,
            "ADC" => ADC,
            "SBB" => SBB,
            "AND" => AND,
            "SUB" => SUB,
            "XOR" => XOR,
            "CMP" => CMP,
            _ => panic!("bad arith instruction"),
        }
    }

    fn hexstr_to_vec_u8(hex: &str) -> Vec<u8> {
        let mut out = vec![];
        for i in 0..hex.len() / 2 {
            let byte = u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16).unwrap();
            out.push(byte);
        }
        out
    }

    #[test]
    fn test_arith_reg64_reg64() {
        let test_data = include_str!("x64_tests/arith_reg64_reg64");
        let reg64_reg64_re = Regex::new(r"([A-Z]+) (R[A-Z0-9]+), (R[A-Z0-9]+) \| ([a-f0-9]+)")
            .unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            let caps = reg64_reg64_re.captures(line).unwrap();
            let op = str_to_arith(caps.at(1).unwrap());
            let reg1 = str_to_reg(caps.at(2).unwrap());
            let reg2 = str_to_reg(caps.at(3).unwrap());
            let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

            let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
            let mut c = CodeEmitter::new(&mut buf);
            c.arithq(op, reg1, reg2);
            assert_eq!(c.code(), &hex[..])
        }
    }

    #[test]
    fn test_arith_reg64_imm() {
        let test_data = include_str!("x64_tests/arith_reg64_imm");
        let reg64_imm_re = Regex::new(r"([A-Z]+) (R[A-Z0-9]+), 0([a-f0-9]+)h \| ([a-f0-9]+)")
            .unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            let caps = reg64_imm_re.captures(line).unwrap();
            let op = str_to_arith(caps.at(1).unwrap());
            let reg = str_to_reg(caps.at(2).unwrap());
            let imm = i32::from_str_radix(caps.at(3).unwrap(), 16).unwrap();
            let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

            let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
            let mut c = CodeEmitter::new(&mut buf);
            c.arithq(op, reg, imm);
            assert_eq!(c.code(), &hex[..]);
        }
    }

    #[test]
    fn test_arith_qword_disp_imm() {
        let test_data = include_str!("x64_tests/arith_qword_disp_imm");
        // CMP qword ptr [07fffffffh], 0ffh | 48813c25ffffff7fff000000
        let qword_disp_imm_re =
            Regex::new(r"([A-Z]+) qword ptr \[0([a-f0-9]+)h\], 0([a-f0-9]+)h \| ([a-f0-9]+)")
                .unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            let caps = qword_disp_imm_re.captures(line).unwrap();
            let op = str_to_arith(caps.at(1).unwrap());
            let disp = i32::from_str_radix(caps.at(2).unwrap(), 16).unwrap();
            let imm = i32::from_str_radix(caps.at(3).unwrap(), 16).unwrap();
            let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

            let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
            let mut c = CodeEmitter::new(&mut buf);
            c.arithq(op, Disp(disp), imm);
            assert_eq!(c.code(), &hex[..]);
        }
    }

    #[test]
    fn test_arith_reg64_disp() {
        let test_data = include_str!("x64_tests/arith_reg64_disp");
        let reg64_disp_re = Regex::new(r"([A-Z]+) (R[A-Z0-9]+), \[0([a-f0-9]+)h\] \| ([a-f0-9]+)")
            .unwrap();
        let disp_reg64_re = Regex::new(r"([A-Z]+) \[0([a-f0-9]+)h\], (R[A-Z0-9]+) \| ([a-f0-9]+)")
            .unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            if let Some(caps) = reg64_disp_re.captures(line) {
                let op = str_to_arith(caps.at(1).unwrap());
                let reg = str_to_reg(caps.at(2).unwrap());
                let disp = i32::from_str_radix(caps.at(3).unwrap(), 16).unwrap();
                let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, reg, Disp(disp));
                assert_eq!(c.code(), &hex[..]);
            } else {
                let caps = disp_reg64_re.captures(line).unwrap();
                let op = str_to_arith(caps.at(1).unwrap());
                let disp = i32::from_str_radix(caps.at(2).unwrap(), 16).unwrap();
                let reg = str_to_reg(caps.at(3).unwrap());
                let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, Disp(disp), reg);
                assert_eq!(c.code(), &hex[..]);
            }
        }
    }

    #[test]
    fn test_arith_reg64_reg64_deref() {
        let test_data = include_str!("x64_tests/arith_reg64_reg64_deref");
        let reg64_reg64_deref_re =
            Regex::new(r"([A-Z]+) ([A-Z0-9]+), \[([A-Z0-9]+)\] \| ([a-f0-9]+)").unwrap();
        let reg64_deref_reg64_re =
            Regex::new(r"([A-Z]+) \[([A-Z0-9]+)\], ([A-Z0-9]+) \| ([a-f0-9]+)").unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            if let Some(caps) = reg64_reg64_deref_re.captures(line) {
                let op = str_to_arith(caps.at(1).unwrap());
                let dst = str_to_reg(caps.at(2).unwrap());
                let src = str_to_reg(caps.at(3).unwrap());
                let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, dst, src.ptr());
                assert_eq!(c.code(), &hex[..]);
            } else {
                let caps = reg64_deref_reg64_re.captures(line).unwrap();
                let op = str_to_arith(caps.at(1).unwrap());
                let dst = str_to_reg(caps.at(2).unwrap());
                let src = str_to_reg(caps.at(3).unwrap());
                let hex = hexstr_to_vec_u8(caps.at(4).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, dst.ptr(), src);
                assert_eq!(c.code(), &hex[..]);
            }
        }
    }

    #[test]
    fn test_arith_reg64_reg64_disp() {
        let test_data = include_str!("x64_tests/arith_reg64_reg64_disp");
        let reg64_reg64_disp_re = Regex::new(
            r"([A-Z]+) (R[A-Z0-9]+), \[(R[A-Z0-9]+) ([-+]) 0([a-f0-9]+)h\] \| ([a-f0-9]+)").unwrap();
        let reg64_disp_reg64_re = Regex::new(
            r"([A-Z]+) \[(R[A-Z0-9]+) ([-+]) 0([a-f0-9]+)h\], (R[A-Z0-9]+) \| ([a-f0-9]+)").unwrap();

        for line in test_data.lines() {
            println!("Testing, {}", line);
            if let Some(caps) = reg64_reg64_disp_re.captures(line) {
                let op = str_to_arith(caps.at(1).unwrap());
                let reg = str_to_reg(caps.at(2).unwrap());
                let base = str_to_reg(caps.at(3).unwrap());
                let pm = if caps.at(4).unwrap() == "+" {
                    1
                } else {
                    -1
                };
                let disp = i32::from_str_radix(caps.at(5).unwrap(), 16).unwrap();
                let hex = hexstr_to_vec_u8(caps.at(6).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, reg, base + (pm * disp));
                assert_eq!(c.code(), &hex[..]);
            } else {
                let caps = reg64_disp_reg64_re.captures(line).unwrap();
                let op = str_to_arith(caps.at(1).unwrap());
                let base = str_to_reg(caps.at(2).unwrap());
                let pm = if caps.at(3).unwrap() == "+" {
                    1
                } else {
                    -1
                };
                let disp = i32::from_str_radix(caps.at(4).unwrap(), 16).unwrap();
                let reg = str_to_reg(caps.at(5).unwrap());
                let hex = hexstr_to_vec_u8(caps.at(6).unwrap());

                let mut buf: [u8; 128] = unsafe { mem::uninitialized() };
                let mut c = CodeEmitter::new(&mut buf);
                c.arithq(op, base + (pm * disp), reg);
                assert_eq!(c.code(), &hex[..]);
            }
        }
    }
}
