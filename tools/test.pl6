#!/usr/local/bin/perl6

use v6;

sub MAIN(Str $asm) {
    LEAVE { 
        "delete_me.s".IO.unlink;
        "delete_me.o".IO.unlink;
        "delete_me.lst".IO.unlink;
    }
    spurt "delete_me.s", "_start:\n\t$asm";
    shell "nasm -f macho64 delete_me.s -l delete_me.lst";
    say "delete_me.lst".IO.slurp.chomp.lines.Array.pop;
}