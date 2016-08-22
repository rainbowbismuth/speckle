// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

mod register;
pub use self::register::{Register, Size};

#[cfg(test)]
mod test;

use std::convert::{From, Into};
use std::ops::{Add, Sub};
use std::result;
use std::io;

/// The amounts you can scale an index register by.
#[derive(Copy, Clone)]
pub enum Scale {
    One,
    Two,
    Four,
    Eight
}

/// Represents the `index * scale` part of an operand.
#[derive(Copy, Clone)]
pub struct Index(Register, Scale);

/// Methods to easily construct an `Index`.
pub trait IndexConstructors {
    /// Scale a register by the amount given.
    fn x(self, scale: Scale) -> Index;
    /// Scale a register by 1.
    fn x1(self) -> Index;
    /// Scale a register by 2.
    fn x2(self) -> Index;
    /// Scale a register by 4.
    fn x4(self) -> Index;
    /// Scale a register by 8.
    fn x8(self) -> Index;
}

impl IndexConstructors for Register {
    fn x(self, scale: Scale) -> Index {
        Index(self, scale)
    }

    fn x1(self) -> Index {
        Index(self, Scale::One)
    }

    fn x2(self) -> Index {
        Index(self, Scale::Two)
    }

    fn x4(self) -> Index {
        Index(self, Scale::Four)
    }

    fn x8(self) -> Index {
        Index(self, Scale::Eight)
    }
}

/// Represents a `[register]` operand.
#[derive(Copy, Clone)]
pub struct Deref(Register);

/// A method to easily construct a `Deref`.
pub trait DerefConstructor {
    /// Use the value in the register as an address.
    fn ptr(self) -> Deref;
}

impl DerefConstructor for Register {
    fn ptr(self) -> Deref {
        Deref(self)
    }
}

/// Represents both `[base+index]` and `[base+index*scale]` operands.
#[derive(Copy, Clone)]
pub struct BaseIndex(Register, Register, Scale);

impl Add<Register> for Register {
    type Output = BaseIndex;

    fn add(self, rhs: Register) -> BaseIndex {
        BaseIndex(self, rhs, Scale::One)
    }
}

impl Add<Index> for Register {
    type Output = BaseIndex;

    fn add(self, rhs: Index) -> BaseIndex {
        BaseIndex(self, rhs.0, rhs.1)
    }
}

/// Represents a `[displacement]` operands.
#[derive(Copy, Clone)]
pub struct Displacement(i64);

/// A method to easily construct a `Displacement`.
pub trait DisplacementConstructor {
    /// Use this value as an absolute displacement.
    fn ptr(self) -> Displacement;
}

impl<T: Into<i64>> DisplacementConstructor for T {
    fn ptr(self) -> Displacement {
        Displacement(self.into())
    }
}

/// Represents a `[base+displacement]` operand.
#[derive(Copy, Clone)]
pub struct BaseDisplacement(Register, i64);

impl <T: Into<i64>> Add<T> for Register {
    type Output = BaseDisplacement;

    fn add(self, rhs: T) -> BaseDisplacement {
        BaseDisplacement(self, rhs.into())
    }
}

impl <T: Into<i64>> Sub<T> for Register {
    type Output = BaseDisplacement;

    fn sub(self, rhs: T) -> BaseDisplacement {
        BaseDisplacement(self, -rhs.into())
    }
}

/// Represents a `[base+index*scale+displacement]` operand.
#[derive(Copy, Clone)]
pub struct BaseIndexDisplacement(Register, Register, Scale, i64);

impl <T: Into<i64>> Add<T> for BaseIndex {
    type Output = BaseIndexDisplacement;

    fn add(self, rhs: T) -> BaseIndexDisplacement {
        BaseIndexDisplacement(self.0, self.1, self.2, rhs.into())
    }
}

impl <T: Into<i64>> Sub<T> for BaseIndex {
    type Output = BaseIndexDisplacement;

    fn sub(self, rhs: T) -> BaseIndexDisplacement {
        BaseIndexDisplacement(self.0, self.1, self.2, -rhs.into())
    }
}

/// Represents any general purpose operand.
#[derive(Copy, Clone)]
pub enum Operand {
    /// An immediate value.
    Imm(i64),
    /// A register.
    Reg(Register),
    /// An absolute displacement.
    Disp(Displacement),
    /// An `[index*scale]` operand.
    Idx(Index),
    /// A `[base+displacement]` operand. 
    BaseDisp(BaseDisplacement),
    /// A `[base+index*scale+displacement]` operand.
    BaseIdxDisp(BaseIndexDisplacement)
}

impl<T: Into<i64>> From<T> for Operand {
    fn from(t: T) -> Self {
        Operand::Imm(t.into())
    }
}

impl From<Register> for Operand {
    fn from(reg: Register) -> Self {
        Operand::Reg(reg)
    }
}

impl From<Displacement> for Operand {
    fn from(disp: Displacement) -> Self {
        Operand::Disp(disp)
    }
}

impl From<Index> for Operand {
    fn from(idx: Index) -> Self {
        Operand::Idx(idx)
    }
}

impl From<BaseDisplacement> for Operand {
    fn from(base_disp: BaseDisplacement) -> Self {
        Operand::BaseDisp(base_disp)
    }
}

impl From<BaseIndexDisplacement> for Operand {
    fn from(base_idx_disp: BaseIndexDisplacement) -> Self {
        Operand::BaseIdxDisp(base_idx_disp)
    }
}

/// An error that might result from encoding an instruction
#[derive(Debug)]
pub enum Error {
    /// The underlying `std::io::Write` encountered this error.
    IO(io::Error),
    /// This instruction may or may not be valid, but it is currently not
    /// implemented.
    NotImplemented,
    /// You tried to construct a memory to memory operation.
    ///
    /// This ISA only support memory to register, register to memory,
    /// and register to register operations.
    MemoryToMemoryOp,
    /// You tried to combine operands of different sizes.
    ///
    /// # Examples
    ///
    /// ```
    /// use speckle::x64::Assembler;
    /// use speckle::x64::Register::*;
    /// let mut asm = Assembler::new(vec![]);
    /// assert_eq!(format!("{:?}", asm.add(AL, AX)), "Err(MismatchedOperandSizes)");
    /// ```
    MismatchedOperandSizes,
    /// You tried to use an immediate value too large for the instruction.
    ImmediateTooLarge,
    /// You tried to use a displacement too large for the instruction.
    DisplacementTooLarge,
    /// You tried to use an invalid combination of same sized registers
    InvalidRegisterCombination,
}

impl From<io::Error> for Error {
    fn from(io_err: io::Error) -> Self {
        Error::IO(io_err)
    }
}

/// A specialized `Result` type for instruction encoding.
pub type Result<T> = result::Result<T, Error>;

/// Wraps a `io::Write` to provide an assembly DSL.
pub struct Assembler<Writer: io::Write> {
    /// The writer that the Assembler writes machine code to.
    pub writer: Writer
}

fn size_check(r1: Register, r2: Register) -> Result<()> {
    if r1.size() != r2.size() {
        Err(Error::MismatchedOperandSizes)
    } else {
        if r1.size() == Size::B8 && ((r1.needs_rex() && r2.never_rex()) || (r1.never_rex() && r2.needs_rex())) {
            Err(Error::InvalidRegisterCombination)
        } else {
            Ok(())
        }
    }
}

fn rr_mod_rm(dst: Register, src: Register) -> u8 {
    0b11000000 + dst.code() + (src.code() << 3)
}

fn rr_rex_xr(dst: Register, src: Register) -> u8 {
    0x40 + (dst.ext() as u8) + ((src.ext() as u8) << 2)
}

impl<Writer: io::Write> Assembler<Writer> {
    /// Create a new `Assembler` from a `Writer`.
    pub fn new(writer: Writer) -> Self {
        Assembler {
            writer: writer
        }
    }

    /// Flushes and returns the `Writer`.
    pub fn done(mut self) -> io::Result<Writer> {
        try!(self.writer.flush());
        Ok(self.writer)
    }

    /// A convienence method for self.writer.write_all
    fn write(&mut self, buf: &[u8]) -> Result<()> {
        try!(self.writer.write_all(buf));
        Ok(())
    }

    fn rr_arith(&mut self, opcode: u8, dst: Register, src: Register) -> Result<()> {
        try!(size_check(dst, src));
        let modrm = rr_mod_rm(dst, src);
        let rex = rr_rex_xr(dst, src);
        match (dst.size(), dst.needs_rex() || src.needs_rex()) {
            (Size::W16, true) => self.write(&[0x66, rex, opcode, modrm]),
            (Size::W16, false) => self.write(&[0x66, opcode, modrm]),
            (_, true) => self.write(&[rex, opcode, modrm]),
            (_, false) => self.write(&[opcode, modrm])
        }
    }

    pub fn add<Dst, Src>(&mut self, dst: Dst, src: Src) -> Result<()>
        where Dst: Into<Operand>, Src: Into<Operand> {
        use self::Operand::*;
        match (dst.into(), src.into()) {
            (Reg(dst_reg), Reg(src_reg)) => {
                if dst_reg.size() == Size::B8 {
                    self.rr_arith(0, dst_reg, src_reg)
                } else {
                    self.rr_arith(1, dst_reg, src_reg)
                }
            },
            _ => {
                Err(Error::NotImplemented)
            }
        }
    }
}