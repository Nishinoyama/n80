pub mod cpu {
    use crate::instruction::InstructionDecoder;
    use crate::memory::{Memory, Memory8Bit64KB};
    use crate::n88cpu::alu::ArithmeticLogicUnit88;
    use crate::n88cpu::instruction::N88InstructionSet;
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
            let mut parser = InstructionDecoder::<u8, N88InstructionSet>::new();
            while parser.inst().is_none() {
                parser.decode(self.fetch());
            }
        }
    }
}

pub mod instruction {
    use crate::instruction::InstructionSet;

    pub enum N88InstructionSet {
        MovRR(u8, u8),
        MovMR(u16, u8),
        // TODO more instructions!
    }

    impl InstructionSet<u8> for N88InstructionSet {
        fn decode(codes: &[u8]) -> Option<Self> {
            if codes.is_empty() {
                None
            } else {
                todo!()
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
