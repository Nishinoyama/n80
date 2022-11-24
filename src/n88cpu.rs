pub mod cpu {
    use crate::instruction::InstructionDecoder;
    use crate::memory::Memory;
    use crate::n88cpu::alu::ArithmeticLogicUnit88;
    use crate::n88cpu::instruction::{Bit16RegisterCode, Bit8RegisterCode, N88InstructionSet};
    use crate::register::{Register, RegisterDividable};

    pub struct CPU88<M, R, RD, RA> {
        alu: ArithmeticLogicUnit88,
        mem: M,
        a: RA,
        bc: RD,
        de: RD,
        hl: RD,
        sp: R,
        pc: R,
    }

    impl<
            M: Memory<u16, u8>,
            R: Register<u16>,
            RD: RegisterDividable<u16, u8>,
            RA: Register<u8>,
        > CPU88<M, R, RD, RA>
    {
        pub fn fetch(&mut self) -> u8 {
            let t = self.mem.load(self.pc.load());
            self.pc.write(self.pc.load().wrapping_add(1));
            t
        }

        pub fn parse_instruct(&mut self) {
            let mut parser = InstructionDecoder::<
                u8,
                N88InstructionSet<Bit8RegisterCode, Bit16RegisterCode>,
            >::new();
            while parser.inst().is_none() {
                parser.decode(self.fetch());
            }
        }
    }
}

pub mod instruction {
    use crate::instruction::InstructionSet;

    #[repr(u8)]
    pub enum Bit8RegisterCode {
        A = 7,
        B = 0,
        C = 1,
        D = 2,
        E = 3,
        H = 4,
        L = 5,
    }

    impl From<u8> for Bit8RegisterCode {
        fn from(r: u8) -> Self {
            assert!(r != 6 && r < 8);
            unsafe { std::mem::transmute(r) }
        }
    }

    #[repr(u8)]
    pub enum Bit16RegisterCode {
        BC = 0,
        DE = 1,
        HL = 2,
        SP = 3,
    }

    impl From<u8> for Bit16RegisterCode {
        fn from(r: u8) -> Self {
            assert!(r < 4);
            unsafe { std::mem::transmute(r) }
        }
    }

    pub trait RegisterCode {}

    /// All Data and Addr are Little Endian
    pub enum N88InstructionSet<RC8, RC16> {
        /// Move Register 01DDDSSS
        MovRR(RC8, RC8),
        /// Move from memory. 01DDD110
        MovRM(RC8),
        /// Move to memory. 01110SSS
        MovMR(RC8),
        /// Move immediate. 00DDD110 Data
        MviR(RC8, u8),
        /// Move to memory immediate. 00110110 Data
        MviM(u8),
        ///  Move to register pair. 00RP0001 Data
        LxiR(RC16, u16),
        /// Load acc direct. 00111010 Addr
        Lda(u16),
        /// Store acc direct. 00110010 Addr
        Sta(u16),
        /// Load HL direct. 00101010 Addr
        Lhld(u16),
        /// Store HL direct. 00100010 Addr
        Shld(u16),
        /// Load acc indirect. 00RP1010
        Ldax(RC16),
        /// Store acc indirect. 00RP0010
        Stax(RC16),
        /// Exchange H and L with D and E
        Xchg,
        /// Add register, A <- A + r. 10000SSS
        AddR(RC8),
        /// Add memory, A <- A + (HL). 10000110
        AddM,
        /// Add immediate A <- A + d. 11000110 Data
        Adi(u8),
        /// Add register with carry. 10001SSS
        AdcR(RC8),
        /// Add memory with carry. 10001110
        AdcM,
        /// Add immediate with carry. 11001110 Data
        Aci(u8),
        /// Subtract register, A <- A - r. 10010SSS
        SubR(RC8),
        /// Subtract memory, A <- A - (HL). 10010110
        SubM,
        /// Subtract immediate, A <- A - d. 11010110
        Subi(u8),
        SbbR(RC8),
        SbbM,
        Sbi(u8),
        /// Increment register. 00DDD100
        InrR(RC8),
        InrM,
        DcrR(RC8),
        DcrM,
        Inx(RC16),
        Dcx(RC16),
        /// Add register pair to HL. 00RP1001
        Dad(RC16),
        /// Decimal adjust acc. 00100111
        Daa,
        /// AND register. 10100SSS
        AnaR(RC8),
        /// AND memory. 10100110
        AnaM,
        /// AND immediate. 11100110 Data
        Ani(u8),
        /// XOR register. 10101SSS
        XraR(RC8),
        XraM,
        Xri(u8),
        /// OR register. 10110SSS
        OraR(RC8),
        OraM,
        Ori(u8),
        /// Compare register, set flags of A - r. 10111SSS
        CmpR(RC8),
        CmpM,
        Cpi(u8),
        /// Rotate left. 00000111
        Rlc,
        /// Rotate right. 00001111
        Rrc,
        /// Rotate left through carry. 00010111
        Ral,
        /// Rotate right through carry. 00011111
        Rar,
        /// Complement acc. 00101111
        Cma,
        /// Complement carry. 00111111
        Cmc,
        /// Set carry. 00110111
        Stc,
        /// Jump unconditional. 11000011 Addr
        /// Conditional jump that is 11XXX010 Addr is not this code.
        Jmp(u16),
        /// Jump on carry. 11011010
        Jc(u16),
        /// Jump on no carry. 11010010
        Jnc(u16),
        Jz(u16),
        Jnz(u16),
        Jp(u16),
        Jm(u16),
        Jpe(u16),
        Jpo(u16),
        /// Call unconditional. 11000101 Addr
        /// (SP - 1) <- PCH
        /// (SP - 2) <- PCL
        /// SP <- SP - 2
        /// PC <- addr
        /// Conditional call that is 11XXX100 Addr is not this code
        Call(u16),
        /// Call on carry. 11011010 Addr
        Cc(u16),
        Cnc(u16),
        Cz(u16),
        Cnz(u16),
        Cp(u16),
        Cm(u16),
        Cpe(u16),
        Cpo(u16),
        /// Return. 11001001
        /// PCL <- (SP)
        /// PCH <- (SP + 1)
        /// SP <- SP + 2
        /// Conditional return that is 11XXX000 is not this code
        Ret,
        /// Call on carry. 11011000
        Rc(u16),
        Rnc(u16),
        Rz(u16),
        Rnz(u16),
        Rp(u16),
        Rm(u16),
        Rpe(u16),
        Rpo(u16),
        /// Restart. 11NNN111
        /// Equals to Call 00nnn000 00000000.
        /// (SP - 1) <- PCH
        /// (SP - 2) <- PCL
        /// SP <- SP - 2
        /// PC <- 8 * nnn
        Rst(u16),
        /// Jump indirect. 11101001
        /// PC <- HL
        PcHL,
        /// Push. 11RP0101
        /// RP = 11 which is SP is not allowed.
        PushRP(RC16),
        /// Push processor status word. 11110101
        ///
        /// (SP-1) <- A
        /// (SP-2) <- F as bit 1 is set
        /// SP <- SP - 2
        PushPSW,
        /// Pop. 11RP0001
        /// RP = 11 which is SP is not allowed.
        PopRP(RC16),
        /// Push processor status word. 11110001
        PopPSW,
        /// Exchange stack top with HL. 11100011
        XtHL,
        /// Move HL to SP. 11111001
        SpHL,
        /// Input. 11011011 port
        In(u8),
        /// Input. 11010011 port
        Out(u8),
        /// Enable interrupts. 11111011
        EI,
        /// Disable interrupts. 11110011
        DI,
        /// Halt. 01110110
        Hlt,
        /// No op. 00000000
        Nop,
    }

    impl InstructionSet<u8> for N88InstructionSet<Bit8RegisterCode, Bit16RegisterCode> {
        fn decode(codes: &[u8]) -> Option<Self> {
            use N88InstructionSet::*;
            if codes.is_empty() {
                None
            } else {
                let c = codes[0] >> 6;
                let r1 = codes[0] >> 3 & 0b111;
                let r2 = codes[0] & 0b111;
                let d1 = codes.get(1).copied();
                let d2 = codes.get(2).copied();
                let dd = d1.and_then(|d1| d2.map(|d2| u16::from_ne_bytes([d1, d2])));
                match c {
                    0b00 => match (r1, r2) {
                        (0b000, 0b000) => Some(Nop),
                        (rp, 0b001) => dd.map(|dd| LxiR((rp >> 1).into(), dd)),
                        (0b000, 0b010) => Some(Stax(Bit16RegisterCode::BC)),
                        (0b001, 0b010) => Some(Ldax(Bit16RegisterCode::BC)),
                        (0b010, 0b010) => Some(Stax(Bit16RegisterCode::DE)),
                        (0b011, 0b010) => Some(Ldax(Bit16RegisterCode::DE)),
                        (0b100, 0b010) => dd.map(Shld),
                        (0b101, 0b010) => dd.map(Lhld),
                        (0b110, 0b010) => dd.map(Sta),
                        (0b111, 0b010) => dd.map(Lda),
                        (d, 0b011) => Some(match d {
                            d if d % 2 == 0 => Inx((d >> 1).into()),
                            d => Dcx((d >> 1).into()),
                        }),
                        (0b110, 0b100) => Some(InrM),
                        (d, 0b100) => Some(InrR(d.into())),
                        (0b110, 0b101) => Some(DcrM),
                        (d, 0b101) => Some(DcrR(d.into())),
                        (0b110, 0b110) => d1.map(MviM),
                        (d, 0b110) => d1.map(|da| MviR(d.into(), da)),
                        (0b000, 0b111) => Some(Rlc),
                        (0b001, 0b111) => Some(Rrc),
                        (0b010, 0b111) => Some(Ral),
                        (0b011, 0b111) => Some(Rar),
                        (0b101, 0b111) => Some(Cma),
                        (0b110, 0b111) => Some(Stc),
                        (0b111, 0b111) => Some(Cmc),
                        (0b100, 0b111) => Some(Daa),
                        _ => None,
                    },
                    0b01 => match (r1, r2) {
                        (0b110, 0b110) => Some(Hlt),
                        (0b110, r) => Some(MovMR(r.into())),
                        (r, 0b110) => Some(MovRM(r.into())),
                        (d, s) => Some(MovRR(d.into(), s.into())),
                    },
                    0b10 => match (r1, r2) {
                        (0b000, 0b110) => Some(AddM),
                        (0b000, r) => Some(AddR(r.into())),
                        (0b001, 0b110) => Some(AdcM),
                        (0b001, r) => Some(AdcR(r.into())),
                        (0b010, 0b110) => Some(SubM),
                        (0b010, r) => Some(SubR(r.into())),
                        (0b011, 0b110) => Some(SbbM),
                        (0b011, r) => Some(SbbR(r.into())),
                        (0b100, 0b110) => Some(AnaM),
                        (0b100, r) => Some(AnaR(r.into())),
                        (0b101, 0b110) => Some(XraM),
                        (0b101, r) => Some(XraR(r.into())),
                        (0b110, 0b110) => Some(OraM),
                        (0b110, r) => Some(OraR(r.into())),
                        (0b111, 0b110) => Some(CmpM),
                        (0b111, r) => Some(CmpR(r.into())),
                        _ => unreachable!(),
                    },
                    0b11 => match (r1, r2) {
                        (0b111, 0b011) => Some(EI),
                        (0b110, 0b011) => Some(DI),
                        (0b100, 0b011) => Some(XtHL),
                        (0b001, 0b001) => Some(Ret),
                        (0b101, 0b001) => Some(PcHL),
                        (0b111, 0b001) => Some(SpHL),
                        _ => None,
                    },
                    _ => unreachable!(),
                }
            }
        }
        fn encode(self) -> Vec<u8> {
            todo!()
        }
    }
}

pub mod alu {
    use crate::register::{R8Bits, Register};

    /// Flag Carry
    const F_CARRY: u8 = 1;
    /// Flag Parity
    const F_PARITY: u8 = 4;
    /// Flag Half Carry (or AUX Carry) for BDC
    const F_HALF_CARRY: u8 = 16;
    /// Flag Zero
    const F_ZERO: u8 = 64;
    /// Flag Sign
    const F_SIGN: u8 = 128;

    /// ALU(Arithmetic Logic Unit) with Flags
    #[derive(Debug, Default)]
    pub struct ArithmeticLogicUnit88 {
        /// Flag
        /// b0: (C) Carry
        /// b2: (P) Parity
        /// b4: (H) AUX Carry
        /// b6: (Z) Zero
        /// b7: (S) Sign
        flag: R8Bits,
    }

    impl ArithmeticLogicUnit88 {
        #[inline]
        pub fn write_flag(&mut self, flag: u8) {
            self.flag.write(flag);
        }
        #[inline]
        pub fn set_flag_of(&mut self, flag: u8) {
            self.write_flag(self.get_flag() | flag);
        }
        #[inline]
        pub fn reset_flag_of(&mut self, flag: u8) {
            self.write_flag(self.get_flag() & !flag);
        }
        pub fn change_flag_of(&mut self, flag: u8, set: bool) {
            if set {
                self.set_flag_of(flag);
            } else {
                self.reset_flag_of(flag);
            }
        }
        #[inline]
        pub fn get_flag_of(&self, flag: u8) -> bool {
            (self.get_flag() & flag) == flag
        }
        #[inline]
        pub fn get_flag(&self) -> u8 {
            self.flag.load()
        }
        #[inline]
        fn change_flags_with_result(&mut self, result: u8) {
            self.change_flag_of(F_PARITY, result.count_ones() % 2 == 0);
            self.change_flag_of(F_SIGN, result >= 0x80);
            self.change_flag_of(F_ZERO, result == 0x00);
        }
        pub fn add(&mut self, a: u8, n: u8) -> u8 {
            let (x, ovf) = a.overflowing_add(n);
            self.change_flag_of(F_CARRY, ovf);
            self.change_flag_of(F_HALF_CARRY, a % 0x10 + n % 0x10 >= 0x10);
            self.change_flags_with_result(x);
            x
        }
        /// add with carry
        pub fn add_carried(&mut self, a: u8, n: u8) -> u8 {
            if !self.get_flag_of(F_CARRY) {
                self.add(a, n)
            } else {
                let (x, ovf1) = a.overflowing_add(1);
                let (x, ovf2) = x.overflowing_add(n);
                self.change_flag_of(F_CARRY, ovf1 | ovf2);
                self.change_flag_of(F_HALF_CARRY, a % 0x10 + n % 0x10 + 1 >= 0x10);
                self.change_flags_with_result(x);
                x
            }
        }
        #[inline]
        pub fn sub(&mut self, a: u8, n: u8) -> u8 {
            self.set_flag_of(F_CARRY);
            let x = self.add_carried(a, !n);
            self.change_flag_of(F_CARRY, !self.get_flag_of(F_CARRY));
            self.change_flag_of(F_HALF_CARRY, a % 0x10 < n % 0x10);
            x
        }
        #[inline]
        pub fn sub_borrowed(&mut self, a: u8, n: u8) -> u8 {
            if self.get_flag_of(F_CARRY) {
                let x = self.add(a, !n);
                self.change_flag_of(F_CARRY, !self.get_flag_of(F_CARRY));
                self.change_flag_of(F_HALF_CARRY, a % 0x10 < n % 0x10 + 1);
                x
            } else {
                self.sub(a, n)
            }
        }
        pub fn and(&mut self, a: u8, n: u8) -> u8 {
            let x = a & n;
            self.set_flag_of(F_HALF_CARRY);
            self.reset_flag_of(F_CARRY);
            self.change_flags_with_result(x);
            x
        }
        pub fn or(&mut self, a: u8, n: u8) -> u8 {
            let x = a | n;
            self.set_flag_of(F_HALF_CARRY);
            self.reset_flag_of(F_CARRY);
            self.change_flags_with_result(x);
            x
        }
        pub fn xor(&mut self, a: u8, n: u8) -> u8 {
            let x = a ^ n;
            self.set_flag_of(F_HALF_CARRY);
            self.reset_flag_of(F_CARRY);
            self.change_flags_with_result(x);
            x
        }
        pub fn inc(&mut self, a: u8) -> u8 {
            let x = a.wrapping_add(1);
            self.change_flag_of(F_HALF_CARRY, a % 0x10 + 1 >= 0x10);
            self.change_flags_with_result(x);
            x
        }
        pub fn dec(&mut self, n: u8) -> u8 {
            let x = n.wrapping_sub(1);
            self.change_flag_of(F_HALF_CARRY, n % 8 == 0);
            self.change_flags_with_result(x);
            x
        }
        pub fn daa(&mut self, a: u8, neg: bool) -> u8 {
            let mut x = a;
            if x % 0x10 > 9 || self.get_flag_of(F_HALF_CARRY) {
                x = if neg {
                    x.wrapping_sub(0x06)
                } else {
                    x.wrapping_add(0x06)
                }
            }
            if x / 0x10 > 9 || self.get_flag_of(F_CARRY) {
                x = if neg {
                    x.wrapping_sub(0x60)
                } else {
                    x.wrapping_add(0x60)
                }
            };
            x
        }
        pub fn cpl(&mut self, a: u8) -> u8 {
            let x = !a;
            self.set_flag_of(F_HALF_CARRY);
            x
        }
        pub fn neg(&mut self, a: u8) -> u8 {
            let x = 0u8.wrapping_sub(a);
            self.change_flag_of(F_CARRY, a == 0x00);
            self.change_flag_of(F_HALF_CARRY, a % 8 == 0);
            self.change_flags_with_result(x);
            x
        }
        pub fn ccf(&mut self) {
            self.change_flag_of(F_HALF_CARRY, self.get_flag_of(F_CARRY));
            self.change_flag_of(F_CARRY, self.get_flag_of(F_CARRY));
        }
        pub fn scf(&mut self) {
            self.reset_flag_of(F_HALF_CARRY);
            self.set_flag_of(F_CARRY);
        }
        pub fn rlc(&mut self, a: u8) -> u8 {
            let x = a.rotate_left(1);
            self.change_flag_of(F_CARRY, a >= 0x80);
            self.reset_flag_of(F_HALF_CARRY);
            x
        }
        pub fn ral(&mut self, a: u8) -> u8 {
            let x = a << 1 | (self.get_flag_of(F_CARRY) as u8);
            self.change_flag_of(F_CARRY, a >= 0x80);
            self.reset_flag_of(F_HALF_CARRY);
            x
        }
        pub fn rrc(&mut self, a: u8) -> u8 {
            let x = a.rotate_right(1);
            self.change_flag_of(F_CARRY, a % 2 == 1);
            self.reset_flag_of(F_HALF_CARRY);
            x
        }
        pub fn rar(&mut self, a: u8) -> u8 {
            let x = a >> 1 | (self.get_flag_of(F_CARRY) as u8) << 7;
            self.change_flag_of(F_CARRY, a % 2 == 1);
            self.reset_flag_of(F_HALF_CARRY);
            x
        }
        pub fn dad(&mut self, hl: u16, n: u16) -> u16 {
            let (x, ovf) = hl.overflowing_add(hl);
            self.change_flag_of(F_CARRY, ovf);
            self.change_flag_of(F_HALF_CARRY, hl % 0x1000 + n % 0x1000 > 0x1000);
            x
        }
        pub fn inx(&mut self, n: u16) -> u16 {
            n.wrapping_add(1)
        }
        pub fn dcx(&mut self, n: u16) -> u16 {
            n.wrapping_sub(1)
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn add() {
            let mut arith = ArithmeticLogicUnit88::default();
            // no carrying
            assert_eq!(arith.add(0x23, 0x34), 0x57);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(!arith.get_flag_of(F_PARITY));
            assert!(!arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x57, false), 0x57);
            // half carrying
            assert_eq!(arith.add(0x28, 0x39), 0x61);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(!arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x61, false), 0x67);
            // top carrying
            assert_eq!(arith.add(0x90, 0x81), 0x11);
            assert!(arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(!arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x11, false), 0x71);
            // both carrying
            assert_eq!(arith.add(0x98, 0x88), 0x20);
            assert!(arith.get_flag_of(F_CARRY));
            assert!(!arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x20, false), 0x86);
            // signed overflow
            assert_eq!(arith.add(0x4a, 0x39), 0x83);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(!arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(arith.get_flag_of(F_SIGN));
            // ZERO
            assert_eq!(arith.add(0x81, 0x7f), 0x00);
            assert!(arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
        }

        #[test]
        fn sub() {
            let mut arith = ArithmeticLogicUnit88::default();
            // no borrowing
            assert_eq!(arith.sub(0x23, 0x11), 0x12);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(!arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x12, true), 0x12);
            // half borrowing
            assert_eq!(arith.sub(0x68, 0x39), 0x2f);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(!arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x2f, true), 0x29);
            // top borrowing
            assert_eq!(arith.sub(0x27, 0x81), 0xa6);
            assert!(arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(!arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0xa6, true), 0x46);
            // both borrowing
            assert_eq!(arith.sub(0x23, 0x45), 0xde);
            assert!(arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(arith.get_flag_of(F_HALF_CARRY));
            assert!(!arith.get_flag_of(F_ZERO));
            assert!(arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0xde, true), 0x78);
            // exact zero
            assert_eq!(arith.sub(0x67, 0x67), 0x00);
            assert!(!arith.get_flag_of(F_CARRY));
            assert!(arith.get_flag_of(F_PARITY));
            assert!(!arith.get_flag_of(F_HALF_CARRY));
            assert!(arith.get_flag_of(F_ZERO));
            assert!(!arith.get_flag_of(F_SIGN));
            assert_eq!(arith.daa(0x00, true), 0x00);
        }

        #[test]
        fn r16_arith() {
            let mut arith = ArithmeticLogicUnit88::default();
            // 0x3191 + 0x47a1 = 0x7932 (not carried)
            let (a1, a2, b1, b2) = (0x31, 0x91, 0x47, 0xa1);
            let x2 = arith.add(a2, b2);
            let x1 = arith.add_carried(a1, b1);
            assert_eq!(x1, 0x79);
            assert_eq!(x2, 0x32);
            assert!(!arith.get_flag_of(F_CARRY));
            // 0x3191 - 0x47a1 = 0xe9f0 (borrowed)
            let x2 = arith.sub(a2, b2);
            let x1 = arith.sub_borrowed(a1, b1);
            assert_eq!(x1, 0xe9);
            assert_eq!(x2, 0xf0);
            assert!(arith.get_flag_of(F_CARRY));
        }
    }
}
