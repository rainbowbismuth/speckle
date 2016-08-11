my $header = ".intel_syntax\n_start:\n\t";

sub dis(Str:D $asm) {
    spurt "delete_me.s", "{$header}{$asm}\n";
    shell "as -arch x86_64 delete_me.s -o delete_me.out";
    say q:x/otool -tvj delete_me.out/.lines.Array.pop;
}

sub MAIN(Str $s) {
    dis($s)
}