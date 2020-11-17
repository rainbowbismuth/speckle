speckle
=======

This was a small experiment in writing a JIT code generation library in Rust. Partly just to learn how `x64` instructions are encoded in the first place, and how much types could be used to check the form of the instruction at compile time.

At it's simplest, you could use it like this:

```rust
  let mut c = Assembler::new(&mut buf);
  c.xor(RAX, RAX);
  c.add(RAX, RDI);
  c.add(RAX, RDI);
  c.ret();
  let code = alloc_exec(c.code());
  let res = unsafe { code.call1(100) };
  assert_eq!(res, 200);
```

And there are operations on registers for different forms of addressing:

```rust
  c.and(RSP, 0xFF);
  c.or(RBX, R9.ptr());
  c.sbb(RSI, R9+0x200);
  c.xor(R8+0x20, R9);
```
