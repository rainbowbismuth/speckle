# Copyright 2016 Emily A. Bellows
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

use v6;
my @reg64s = 
    <RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15>;
my @arith = <ADD OR ADC SBB AND SUB XOR CMP>;
my @imm32s = <00h 01h 0fh 0ffh 0100h 0ffffh 010000h 0ffffffh 01000000h 07fffffffh>;
my @scale = <1 2 4 8>;
my $header = ".intel_syntax\n_start:\n\t";

my Int:D $gen_amount = 250;

sub dis(Str:D $asm) {
    spurt "delete_me.s", "{$header}{$asm}\n";
    shell "as -arch x86_64 delete_me.s -o delete_me.out";
    q:x/otool -tvj delete_me.out/.lines.Array.pop ~~ m{
        \d ** 16
        \s*
        $<bytes> = <[0..9 a..f A..F]>*
    };
    return ~$<bytes>;
}

sub out($fh, Str:D $instr) {
    $fh.say($instr, ' | ', dis $instr);
}

sub arith-reg64-reg64($fh) {
    for pick $gen_amount, (@arith X @reg64s X @reg64s) {
        my ($op, $reg1, $reg2) = $_;
        out $fh, "$op $reg1, $reg2";
    }
}

sub arith-reg64-reg64-deref($fh) {
    out $fh, "ADD RSP, [RAX]";
    out $fh, "ADD [RSP], RAX";

    for pick $gen_amount, (@arith X @reg64s X @reg64s) {
        my ($op, $reg1, $reg2) = $_;
        out $fh, "$op $reg1, [$reg2]";
        out $fh, "$op [$reg1], $reg2";
    }
}

sub arith-reg64-imm($fh) {
    for pick $gen_amount, (@arith X @reg64s X @imm32s) {
        my ($op, $reg, $imm) = $_;
        out $fh, "$op $reg, $imm";
    }
}

sub arith-reg64-disp($fh) {
    for pick $gen_amount, (@arith X @reg64s X @imm32s) {
        my ($op, $reg, $disp) = $_;
        out $fh, "$op $reg, [$disp]";
        out $fh, "$op [$disp], $reg";
    }
}

sub arith-qword-disp-imm($fh) {
    for pick $gen_amount, (@arith X @imm32s X @imm32s) {
        my ($op, $disp, $imm) = $_;
        out $fh, "$op qword ptr [$disp], $imm";
    }
}

sub arith-reg64-reg64-disp($fh) {
    for ^$gen_amount {
        my ($op, $reg1, $reg2, $offset, $pm) = 
            (@arith, @reg64s, @reg64s, @imm32s, <+ ->)>>.roll;
        out $fh, "$op $reg1, [$reg2 $pm $offset]";
        out $fh, "$op [$reg1 $pm $offset], $reg2";
    }
}

sub arith-reg64-reg64-idx($fh) {
    for ^$gen_amount {
        my ($op, $reg1, $reg2, $index, $scale) =
            (@arith, @reg64s, @reg64s, @reg64s, @scale)>>.roll;
        out $fh, "$op $reg1, [$reg2 + $index * $scale]";
        out $fh, "$op [$reg2+$index*$scale], $reg1";
    }
}

sub arith-reg64-reg64-idx-disp($fh) {
    for ^$gen_amount {
        my ($op, $reg1, $reg2, $index, $scale, $pm, $disp) = 
            (@arith, @reg64s, @reg64s, @reg64s, @scale, <+ ->, @imm32s)>>.roll;
        out $fh, "$op $reg1, [$reg2 + $index * $scale $pm $disp]";
        out $fh, "$op [$reg2 + $index * $scale $pm $disp], $reg1";
    }
}

sub MAIN() {
    arith-reg64-reg64(open "./src/arch/x64_tests/arith_reg64_reg64", :w);
    arith-reg64-reg64-deref(open "./src/arch/x64_tests/arith_reg64_reg64_deref", :w);
    arith-reg64-imm(open "./src/arch/x64_tests/arith_reg64_imm", :w);
    arith-reg64-disp(open "./src/arch/x64_tests/arith_reg64_disp", :w);
    arith-qword-disp-imm(open "./src/arch/x64_tests/arith_qword_disp_imm", :w);
    arith-reg64-reg64-disp(open "./src/arch/x64_tests/arith_reg64_reg64_disp", :w);
    arith-reg64-reg64-idx(open "./src/arch/x64_tests/arith_reg64_reg64_idx", :w);
    arith-reg64-reg64-idx-disp(open "./src/arch/x64_tests/arith_reg64_reg64_idx_disp", :w);
}