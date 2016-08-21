// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Speckle is a library for generating machine code.
//!
//! Currently it only implements a small subsection of the x86 and x64 ISA.

// #![warn(missing_docs)]

extern crate libc;

/// Encode x64 instructions.
pub mod x64;
