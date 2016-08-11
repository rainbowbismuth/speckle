# Copyright 2016 Emily A. Bellows
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

use v6;

my @all-registers;
my %registers;

sub register-impl($ty, Int :$bits, Int :$code, Bool :$ext, Bool :$needs_rex=False) {
    @all-registers.push($ty);
    %registers{$ty}<code> = $code;
    %registers{$ty}<ext> = $ext;
    %registers{$ty}<bits> = $bits;
    %registers{$ty}<needs_rex> = $needs_rex;
    say qq:to/END/;
        #[derive(Copy, Clone, Debug)]
        pub struct $ty;

        impl RegCode for $ty \{
            fn code(self) -> u8 \{
                $code
            }

            fn needs_ext(self) -> bool \{
                {$ext.lc}
            }

            fn needs_rex(self) -> bool \{
                self.needs_ext() || {$needs_rex.lc}
            }
        }

        impl Reg{$bits} for $ty \{}
        END
}

#
# impl<Src: EightBit_NoREX> ADD<Src> for AL
# impl<Src: EightBit_REX> ADD<Src> for AL
# impl<Src: EightBit_REX_R> ADD<Src> for AL
# impl<Src: i8> ADD<Src> for AL
#
#
#

say q:to/END/;
// Copyright 2016 Emily A. Bellows
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// DO NOT MODIFY
// This file was generated by tools/gen_x86_x64.pl6

use super::types::*;
END

my @reg8 = <AL CL DL BL AH CH DH BH>;
for @reg8.pairs {
    register-impl(.value, :bits(8), :code(.key), :ext(False));
}

my @reg8-REX = <SPL BPL SIL DIL>;
for @reg8-REX.pairs {
    register-impl(.value, :bits(8), :code(.key + 4), :ext(False), :needs_rex(True));
}

my @reg8-REX-EXT = <R8B R9B R10B R11B R12B R13B R14B R15B>;
for @reg8-REX-EXT.pairs {
    register-impl(.value, :bits(8), :code(.key), :ext(True));
}

my @reg16 = <AX CX DX BX SP BP SI DI>;
for @reg16.pairs {
    register-impl(.value, :bits(16), :code(.key), :ext(False));
}

my @reg16-EXT = <R8W R9W R10W R11W R12W R13W R14W R15W>;
for @reg16-EXT.pairs {
    register-impl(.value, :bits(16), :code(.key), :ext(True));
}

my @reg32 = <EAX ECX EDX EBX ESP EBP ESI EDI>;
for @reg32.pairs {
    register-impl(.value, :bits(32), :code(.key), :ext(False));
}

my @reg32-EXT = <R8D R9D R10D R11D R12D R14D R15D>;
for @reg32-EXT.pairs {
    register-impl(.value, :bits(32), :code(.key), :ext(True));   
} 

my @reg64 = <RAX RCX RDX RBX RSP RBP RSI RDI>;
for @reg64.pairs {
    register-impl(.value, :bits(64), :code(.key), :ext(False));  
} 

my @reg64-EXT = <R8 R9 R10 R11 R12 R13 R14 R15>;
for @reg64-EXT.pairs {
    register-impl(.value, :bits(64), :code(.key), :ext(True));  
} 

say "#[derive(Copy, Clone, Debug)]";
say "#[repr(u8)]";
say "pub enum Register \{";
for @all-registers {
    say "  $_,";
}
say "  RIP";
say "}";

my %size_types;
%size_types{8} = "Size::Byte";
%size_types{16} = "Size::Word";
%size_types{32} = "Size::Double";
%size_types{64} = "Size::Quad";

say "pub fn info(reg: Register) -> RegisterInfo \{";
say "  match reg \{";
for @all-registers {
    say qq:to/END/;
    Register::$_ => RegisterInfo \{
        size: {%size_types{%registers{$_}<bits>}},
        code: {%registers{$_}<code>},
        needs_extension: {%registers{$_}<ext>.lc},
        needs_any_rex: {%registers{$_}<needs_rex>.lc},
    },
END
}
say qq:to/END/;
    Register::RIP => RegisterInfo \{
        size: Size::Quad,
        code: 5,
        needs_extension: false,
        needs_any_rex: false,
    }
  }
}
END

my @arith-ops = <ADD OR ADC SBB AND SUB XOR CMP>;
for @arith-ops {
    say qq:to/END/;
    pub trait $_\<Dst, Src> \{
        fn {$_.lc}(&mut self, dst: Dst, src: Src);
    }
    END
}

for @arith-ops.pairs {
    say qq:to/END/;
    impl<\'a> {.value}<AL, i8> for Assembler<\'a> \{
        fn {.value.lc}(&mut self, _: AL, src: i8) \{
            let mut builder = InstructionBuilder::new();
            builder.emit({4 + .key * 8});
            builder.emit_imm_i8(src);
            self.append(&builder);
        }
    }
    END
}

sub emit-reg-reg-arith(Str $op, Int $op-num, Int $reg-size) {
    say qq:to/END/;
    fn gen_r{$reg-size}_{$op.lc}<\'a, Dst: Reg{$reg-size}, Src: Reg{$reg-size}>(asm: &mut Assembler<\'a>, dst: Dst, src: Src) \{
        let mut builder = InstructionBuilder::new();
    END

    if $reg-size == 16 {
        say "    builder.emit_operand_size_prefix();";
    }

    if $reg-size != 64 {
        say qq:to/END/;
            if dst.needs_rex() \{
                builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), false);
            }
        END
    } else {
        say "    builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), true);";
    }

    my $offset;
    if $reg-size == 8 {
        $offset = 0;
    } else {
        $offset = 1;
    }

    say qq:to/END/;
        builder.emit({$offset + $op-num * 8});
        builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
        asm.append(&builder);
    }
    END 
}

sub emit-reg-reg-arith-impl(Str $op, Str $dst, Int $reg-size) {
    say qq:to/END/;
    impl<\'a, Src: Reg{$reg-size}> {$op}<$dst, Src> for Assembler<\'a> \{
        fn {$op.lc}(&mut self, dst: $dst, src: Src) \{
            gen_r{$reg-size}_{$op.lc}(self, dst, src);
        }
    }
    END
}

for @arith-ops.pairs {
    for 8, 16, 32, 64 -> $reg-size {
        emit-reg-reg-arith(.value, .key, $reg-size);
    }

    for %registers.keys -> $dst {
        emit-reg-reg-arith-impl(.value, $dst, %registers{$dst}<bits>);
    }
}

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r8_{.value.lc}<\'a, Dst: Reg8, Src: Reg8>(asm: &mut Assembler<\'a>, dst: Dst, src: Src) \{
#         let mut builder = InstructionBuilder::new();
#         if dst.needs_rex() || src.needs_rex() \{
#             builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), false);
#         }
#         builder.emit({0 + .key * 8});
#         builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 8}) -> $dst {
#         say qq:to/END/;
#         impl<\'a, Src: Reg8> {.value}<$dst, Src> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: Src) \{
#                 gen_r8_{.value.lc}(self, dst, src);
#             }
#         }        
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r16_{.value.lc}<\'a, Dst: Reg16, Src: Reg16>(asm: &mut Assembler<\'a>, dst: Dst, src: Src) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_operand_size_prefix();
#         if dst.needs_ext() || src.needs_ext() \{
#             builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), false);
#         }
#         builder.emit({1 + .key * 8});
#         builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 16}) -> $dst {
#         say qq:to/END/;
#         impl<\'a, Src: Reg16> {.value}<$dst, Src> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: Src) \{
#                 gen_r16_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r32_{.value.lc}<\'a, Dst: Reg32, Src: Reg32>(asm: &mut Assembler<\'a>, dst: Dst, src: Src) \{
#         let mut builder = InstructionBuilder::new();
#         if dst.needs_ext() || src.needs_ext() \{
#             builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), false);
#         }
#         builder.emit({1 + .key * 8});
#         builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 32}) -> $dst {
#         say qq:to/END/;
#         impl<\'a, Src: Reg32> {.value}<$dst, Src> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: Src) \{
#                 gen_r32_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r64_{.value.lc}<\'a, Dst: Reg64, Src: Reg64>(asm: &mut Assembler<\'a>, dst: Dst, src: Src) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_rex(src.needs_ext(), false, dst.needs_ext(), true);
#         builder.emit({1 + .key * 8});
#         builder.emit_mod_reg_rm(Mod::Register, dst.code(), src.code());
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 64}) -> $dst {
#         say qq:to/END/;
#         impl<\'a, Src: Reg64> {.value}<$dst, Src> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: Src) \{
#                 gen_r64_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

sub emit-reg-imm-arith(Str $op, Int $op-num, Int $reg-size, Str $imm) {
    say qq:to/END/;
    fn gen_r{$reg-size}_{$imm}_{$op.lc}<\'a, Dst: Reg{$reg-size}>(asm: &mut Assembler<\'a>, dst: Dst, src: {$imm}) \{
        let mut builder = InstructionBuilder::new();
    END

    if $reg-size == 16 {
        say "    builder.emit_operand_size_prefix();";
    }

    if $reg-size != 64 {
        say qq:to/END/;
            if dst.needs_rex() \{
                builder.emit_rex(false, false, dst.needs_ext(), false);
            }
        END
    } else {
        say "    builder.emit_rex(false, false, dst.needs_ext(), true);";
    }

    if $reg-size == 8 {
        say "    builder.emit(0x80);";
    } else {
        if $imm eq "i8" {
            say "   builder.emit(0x83);";
        } else {
            say "   builder.emit(0x81);";
        }
    }

    say qq:to/END/;
        builder.emit_mod_reg_rm(Mod::Register, {$op-num}, dst.code());
        builder.emit_imm_{$imm}(src);
        asm.append(&builder);
    }
    END
}

sub emit-reg-imm-arith-impl(Str $op, Str $dst, Int $dst-size, Str $imm) {
     say qq:to/END/;
     impl<\'a> {$op}<{$dst}, {$imm}> for Assembler<\'a> \{
         fn {$op.lc}(&mut self, dst: {$dst}, src: {$imm}) \{
             gen_r{$dst-size}_{$imm}_{$op.lc}(self, dst, src);
         }
     }
     END
 }

 for @arith-ops.pairs {
     emit-reg-imm-arith(.value, .key, 8, "i8");
     for <i8 i16> -> $imm {
         emit-reg-imm-arith(.value, .key, 16, $imm);
     }
     for <i8 i32> -> $imm {
         emit-reg-imm-arith(.value, .key, 32, $imm);
         emit-reg-imm-arith(.value, .key, 64, $imm);
     }
 }

 for @arith-ops -> $op {
     for %registers.pairs.map({.key if .value<bits> == 8 && .key ne "AL"}) -> $dst {
         emit-reg-imm-arith-impl($op, $dst, 8, "i8");
     }
     for %registers.pairs.map({.key if .value<bits> == 16}) -> $dst {
         emit-reg-imm-arith-impl($op, $dst, 16, "i8");
         emit-reg-imm-arith-impl($op, $dst, 16, "i16");
     }
     for %registers.pairs.map({.key if .value<bits> == 32 | 64}) -> $dst {
         emit-reg-imm-arith-impl($op, $dst, %registers{$dst}<bits>, "i8");
         emit-reg-imm-arith-impl($op, $dst, %registers{$dst}<bits>, "i32");
     }
 }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r8_i8_{.value.lc}<\'a, Dst: Reg8>(asm: &mut Assembler<\'a>, dst: Dst, src: i8) \{
#         let mut builder = InstructionBuilder::new();
#         if dst.needs_rex() \{
#             builder.emit_rex(false, false, dst.needs_ext(), false);
#         }
#         builder.emit(0x80);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i8(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 8 && .key ne "AL"}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i8> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i8) \{
#                 gen_r8_i8_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r16_i8_{.value.lc}<\'a, Dst: Reg16>(asm: &mut Assembler<\'a>, dst: Dst, src: i8) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_operand_size_prefix();
#         if dst.needs_ext() \{
#             builder.emit_rex(false, false, dst.needs_ext(), false);
#         }
#         builder.emit(0x83);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i8(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 16}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i8> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i8) \{
#                 gen_r16_i8_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r16_i16_{.value.lc}<\'a, Dst: Reg16>(asm: &mut Assembler<\'a>, dst: Dst, src: i16) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_operand_size_prefix();
#         if dst.needs_ext() \{
#             builder.emit_rex(false, false, dst.needs_ext(), false);
#         }
#         builder.emit(0x81);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i16(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 16}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i16> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i16) \{
#                 gen_r16_i16_{.value.lc}(self, dst, src);
#             }
#         }
#         END       
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r32_i8_{.value.lc}<\'a, Dst: Reg32>(asm: &mut Assembler<\'a>, dst: Dst, src: i8) \{
#         let mut builder = InstructionBuilder::new();
#         if dst.needs_ext() \{
#             builder.emit_rex(false, false, dst.needs_ext(), false);
#         }
#         builder.emit(0x83);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i8(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 32}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i8> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i8) \{
#                 gen_r32_i8_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r32_i32_{.value.lc}<\'a, Dst: Reg32>(asm: &mut Assembler<\'a>, dst: Dst, src: i32) \{
#         let mut builder = InstructionBuilder::new();
#         if dst.needs_ext() \{
#             builder.emit_rex(false, false, dst.needs_ext(), false);
#         }
#         builder.emit(0x81);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i32(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 32}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i32> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i32) \{
#                 gen_r32_i32_{.value.lc}(self, dst, src);
#             }
#         }
#         END        
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r64_i8_{.value.lc}<\'a, Dst: Reg64>(asm: &mut Assembler<\'a>, dst: Dst, src: i8) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_rex(false, false, dst.needs_ext(), true);
#         builder.emit(0x83);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i8(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 64}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i8> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i8) \{
#                 gen_r64_i8_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     fn gen_r64_i32_{.value.lc}<\'a, Dst: Reg64>(asm: &mut Assembler<\'a>, dst: Dst, src: i32) \{
#         let mut builder = InstructionBuilder::new();
#         builder.emit_rex(false, false, dst.needs_ext(), true);
#         builder.emit(0x81);
#         builder.emit_mod_reg_rm(Mod::Register, {.key}, dst.code());
#         builder.emit_imm_i32(src);
#         asm.append(&builder);
#     }
#     END

#     for %registers.pairs.map({.key if .value<bits> == 64}) -> $dst {
#         say qq:to/END/;
#         impl<\'a> {.value}<$dst, i32> for Assembler<\'a> \{
#             fn {.value.lc}(&mut self, dst: $dst, src: i32) \{
#                 gen_r64_i32_{.value.lc}(self, dst, src);
#             }
#         }
#         END
#     }
# }

# It seems my assembler doesn't emit these versions for some reason!
#
# for @arith-ops.pairs {
#     say qq:to/END/;
#     impl<\'a> {.value}<AX, i16> for Assembler<\'a> \{
#         fn {.value.lc}(&mut self, _: AX, src: i16) \{
#             let mut builder = InstructionBuilder::new();
#             builder.emit_operand_size_prefix();
#             builder.emit({5 + .key * 8});
#             builder.emit_imm_i16(src);
#             self.append(&builder);
#         }
#     }
#     END
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     impl<\'a> {.value}<EAX, i32> for Assembler<\'a> \{
#         fn {.value.lc}(&mut self, _: RAX, src: i32) \{
#             let mut builder = InstructionBuilder::new();
#             builder.emit({5 + .key * 8});
#             builder.emit_imm_i32(src);
#             self.append(&builder);
#         }
#     }
#     END
# }

# for @arith-ops.pairs {
#     say qq:to/END/;
#     impl<\'a> {.value}<RAX, i32> for Assembler<\'a> \{
#         fn {.value.lc}(&mut self, _: RAX, src: i32) \{
#             let mut builder = InstructionBuilder::new();
#             builder.emit_rex(false, false, false, /*R64*/ true);
#             builder.emit({5 + .key * 8});
#             builder.emit_imm_i32(src);
#             self.append(&builder);
#         }
#     }
#     END
# }