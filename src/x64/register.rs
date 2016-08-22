// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// DO NOT MODIFY
// This file was generated by tools/gen_registers.pl6

/// A general purpose register.
#[derive(Copy, Clone, Debug)]
#[repr(u16)]
pub enum Register {
  /// A 8-bit register. The low byte of the 16-bit register, `AX`.
	  AL = 0b0000000000,
  /// A 8-bit register. The low byte of the 16-bit register, `CX`.
	  CL = 0b0000000001,
  /// A 8-bit register. The low byte of the 16-bit register, `DX`.
	  DL = 0b0000000010,
  /// A 8-bit register. The low byte of the 16-bit register, `BX`.
	  BL = 0b0000000011,
  /// A 8-bit register. The high byte of the 16-bit register, `AX`.
  ///
  /// Any instruction using a REX prefix CAN NOT access this register. So you can not combine the usage of `AH`
  /// with the `DIL`, `SIL`, `BPL`, or `SPL` registers. Nor the `R8L` to `R15L` registers.
	  AH = 0b0000001100,
  /// A 8-bit register. The low byte of the `SP` register.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 SPL = 0b0000010100,
  /// A 8-bit register. The high byte of the 16-bit register, `CX`.
  ///
  /// Any instruction using a REX prefix CAN NOT access this register. So you can not combine the usage of `CH`
  /// with the `DIL`, `SIL`, `BPL`, or `SPL` registers. Nor the `R8L` to `R15L` registers.
	  CH = 0b0000001101,
  /// A 8-bit register. The low byte of the `BP` register.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 BPL = 0b0000010101,
  /// A 8-bit register. The low byte of the `SI` register.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 SIL = 0b0000010110,
  /// A 8-bit register. The high byte of the 16-bit register, `DX`.
  ///
  /// Any instruction using a REX prefix CAN NOT access this register. So you can not combine the usage of `DH`
  /// with the `DIL`, `SIL`, `BPL`, or `SPL` registers. Nor the `R8L` to `R15L` registers.
	  DH = 0b0000001110,
  /// A 8-bit register. The high byte of the 16-bit register, `BX`.
  ///
  /// Any instruction using a REX prefix CAN NOT access this register. So you can not combine the usage of `BH`
  /// with the `DIL`, `SIL`, `BPL`, or `SPL` registers. Nor the `R8L` to `R15L` registers.
	  BH = 0b0000001111,
  /// A 8-bit register. The low byte of the `DI` register.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 DIL = 0b0000010111,
  /// A 8-bit register. The low byte of the 16-bit register, `R8W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 R8B = 0b0000111000,
  /// A 8-bit register. The low byte of the 16-bit register, `R9W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	 R9B = 0b0000111001,
  /// A 8-bit register. The low byte of the 16-bit register, `R10W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R10B = 0b0000111010,
  /// A 8-bit register. The low byte of the 16-bit register, `R11W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R11B = 0b0000111011,
  /// A 8-bit register. The low byte of the 16-bit register, `R12W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R12B = 0b0000111100,
  /// A 8-bit register. The low byte of the 16-bit register, `R13W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R13B = 0b0000111101,
  /// A 8-bit register. The low byte of the 16-bit register, `R14W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R14B = 0b0000111110,
  /// A 8-bit register. The low byte of the 16-bit register, `R15W`.
  ///
  /// Available only in 64-bit mode.
  ///
  /// Any instruction using this register is encoded with a REX prefix, and so cannot be used with the
  /// `AH`, `CH`, `DH` or `BH` registers.
	R15B = 0b0000111111,
  /// A 16-bit register. The low word of the 32-bit register, `EAX`.
	  AX = 0b0001000000,
  /// A 16-bit register. The low word of the 32-bit register, `ECX`.
	  CX = 0b0001000001,
  /// A 16-bit register. The low word of the 32-bit register, `EDX`.
	  DX = 0b0001000010,
  /// A 16-bit register. The low word of the 32-bit register, `EBX`.
	  BX = 0b0001000011,
  /// A 16-bit register. The low word of the 32-bit register, `ESP`.
	  SP = 0b0001000100,
  /// A 16-bit register. The low word of the 32-bit register, `EBP`.
	  BP = 0b0001000101,
  /// A 16-bit register. The low word of the 32-bit register, `ESI`.
	  SI = 0b0001000110,
  /// A 16-bit register. The low word of the 32-bit register, `EDI`.
	  DI = 0b0001000111,
  /// A 16-bit register. The low word of the 32-bit register, `R8D`.
  ///
  /// Available only in 64-bit mode.
	 R8W = 0b0001111000,
  /// A 16-bit register. The low word of the 32-bit register, `R9D`.
  ///
  /// Available only in 64-bit mode.
	 R9W = 0b0001111001,
  /// A 16-bit register. The low word of the 32-bit register, `R10D`.
  ///
  /// Available only in 64-bit mode.
	R10W = 0b0001111010,
  /// A 16-bit register. The low word of the 32-bit register, `R11D`.
  ///
  /// Available only in 64-bit mode.
	R11W = 0b0001111011,
  /// A 16-bit register. The low word of the 32-bit register, `R12D`.
  ///
  /// Available only in 64-bit mode.
	R12W = 0b0001111100,
  /// A 16-bit register. The low word of the 32-bit register, `R13D`.
  ///
  /// Available only in 64-bit mode.
	R13W = 0b0001111101,
  /// A 16-bit register. The low word of the 32-bit register, `R14D`.
  ///
  /// Available only in 64-bit mode.
	R14W = 0b0001111110,
  /// A 16-bit register. The low word of the 32-bit register, `R15D`.
  ///
  /// Available only in 64-bit mode.
	R15W = 0b0001111111,
  /// A 32-bit register. The low double word of the 64-bit register, `RAX`.
	 EAX = 0b0010000000,
  /// A 32-bit register. The low double word of the 64-bit register, `RCX`.
	 ECX = 0b0010000001,
  /// A 32-bit register. The low double word of the 64-bit register, `RDX`.
	 EDX = 0b0010000010,
  /// A 32-bit register. The low double word of the 64-bit register, `RBX`.
	 EBX = 0b0010000011,
  /// A 32-bit register. The low double word of the 64-bit register, `RSP`.
	 ESP = 0b0010000100,
  /// A 32-bit register. The low double word of the 64-bit register, `RBP`.
	 EBP = 0b0010000101,
  /// A 32-bit register. The low double word of the 64-bit register, `RSI`.
	 ESI = 0b0010000110,
  /// A 32-bit register. The low double word of the 64-bit register, `RDI`.
	 EDI = 0b0010000111,
  /// A 32-bit register. The low double word of the 64-bit register, `R8`.
  ///
  /// Available only in 64-bit mode.
	 R8D = 0b0010111000,
  /// A 32-bit register. The low double word of the 64-bit register, `R9`.
  ///
  /// Available only in 64-bit mode.
	 R9D = 0b0010111001,
  /// A 32-bit register. The low double word of the 64-bit register, `R10`.
  ///
  /// Available only in 64-bit mode.
	R10D = 0b0010111010,
  /// A 32-bit register. The low double word of the 64-bit register, `R11`.
  ///
  /// Available only in 64-bit mode.
	R11D = 0b0010111011,
  /// A 32-bit register. The low double word of the 64-bit register, `R12`.
  ///
  /// Available only in 64-bit mode.
	R12D = 0b0010111100,
  /// A 32-bit register. The low double word of the 64-bit register, `R13`.
  ///
  /// Available only in 64-bit mode.
	R13D = 0b0010111101,
  /// A 32-bit register. The low double word of the 64-bit register, `R14`.
  ///
  /// Available only in 64-bit mode.
	R14D = 0b0010111110,
  /// A 32-bit register. The low double word of the 64-bit register, `R15`.
  ///
  /// Available only in 64-bit mode.
	R15D = 0b0010111111,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RAX = 0b0011010000,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RCX = 0b0011010001,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RDX = 0b0011010010,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RBX = 0b0011010011,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RSP = 0b0011010100,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RBP = 0b0011010101,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RSI = 0b0011010110,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 RDI = 0b0011010111,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	  R8 = 0b0011111000,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	  R9 = 0b0011111001,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R10 = 0b0011111010,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R11 = 0b0011111011,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R12 = 0b0011111100,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R13 = 0b0011111101,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R14 = 0b0011111110,
  /// A 64-bit register.
  ///
  /// Available only in 64-bit mode.
	 R15 = 0b0011111111,
}

/// The size of a general purpose register.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum Size {
    /// A byte, or 8-bits.
    B = 0,
    /// A word, or 16-bits.
    W = 1,
    /// A double word, or 32-bits.
    D = 2,
    /// A quad word, or 64-bits.
    Q = 3
}

impl Register {
    /// How to encode this register in three bits.
    ///
    /// ```
    /// use speckle::x64::Register;
    /// assert_eq!(Register::EAX.code(), 0);
    /// assert_eq!(Register::R9D.code(), 1);
    /// ```
    pub fn code(self) -> u8 {
        ((self as u16) & 0b111) as u8
    }

    /// Can this register never be used with a REX prefix? 
    ///
    /// ```
    /// use speckle::x64::Register;
    /// assert_eq!(Register::AL.never_rex(), false);
    /// assert_eq!(Register::AH.never_rex(), true);
    /// ```
    pub fn never_rex(self) -> bool {
        ((self as u16) >> 3) & 1 != 0
    }

    /// Does this register require some kind of REX prefix to encode?
    ///
    /// ```
    /// use speckle::x64::Register;
    /// assert_eq!(Register::CX.needs_rex(), false);
    /// assert_eq!(Register::SIL.needs_rex(), true);
    /// ```
    pub fn needs_rex(self) -> bool {
        ((self as u16) >> 4) & 1 != 0
    }

    /// Does this register require a REX extension to encode?
    ///
    /// ```
    /// use speckle::x64::Register;
    /// assert_eq!(Register::SIL.ext(), false);
    /// assert_eq!(Register::R8B.ext(), true);
    /// ```
    pub fn ext(self) -> bool {
        ((self as u16) >> 5) & 1 != 0
    }

    /// Returns how big the register is.
    ///
    /// ```
    /// use speckle::x64::{Register, Size};
    /// assert_eq!(Register::RAX.size(), Size::Q);
    /// assert_eq!(Register::R8W.size(), Size::W);
    /// ```
    pub fn size(self) -> Size {
        match (((self as u16) >> 6) & 0b11) as u8 {
            0 => Size::B,
            1 => Size::W,
            2 => Size::D,
            3 => Size::Q,
            _ => unreachable!("Can't be reached because of the bitwise AND'")
        }
    }
}

