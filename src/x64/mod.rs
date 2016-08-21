// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

mod register;
pub use self::register::{Register, Size};

use std::convert::From;
use std::ops::{Add, Sub, Mul};

#[derive(Copy, Clone)]
pub enum Scale {
    One,
    Two,
    Four,
    Eight
}

#[derive(Copy, Clone)]
pub struct Index(Register, Scale);

pub trait RegIndex {
    fn x(self, scale: Scale) -> Index;
    fn x1(self) -> Index;
    fn x2(self) -> Index;
    fn x4(self) -> Index;
    fn x8(self) -> Index;
}

impl RegIndex for Register {
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

#[derive(Copy, Clone)]
pub struct Deref(Register);

pub trait RegDeref {
    fn ptr(self) -> Deref;
}

impl RegDeref for Register {
    fn ptr(self) -> Deref {
        Deref(self)
    }
}
