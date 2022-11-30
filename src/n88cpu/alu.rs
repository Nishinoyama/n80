use crate::alu::{ALUStatus, ALU};
use std::mem::transmute;

#[repr(u8)]
pub enum ALU88Flags {
    /// Flag Carry
    Carry = 1,
    /// Flag Parity
    Parity = 4,
    /// Flag Half Carry (or AUX Carry) for BDC
    HalfCarry = 16,
    /// Flag Zero
    Zero = 64,
    /// Flag Sign
    Sign = 128,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ALU88Status(u8);

impl ALUStatus for ALU88Status {
    type Flags = ALU88Flags;
    fn change(&mut self, flag: Self::Flags, set: bool) {
        let f: u8 = unsafe { transmute(flag) };
        if set {
            self.0 |= f;
        } else {
            self.0 &= !f;
        }
    }
    fn get_flag(&mut self, flag: Self::Flags) -> bool {
        let f: u8 = unsafe { transmute(flag) };
        return (self.0 & f) != 0;
    }
}

impl ALU88Status {
    pub fn change_by_result(&mut self, res: <ALU88 as ALU>::Data) {
        self.change(ALU88Flags::Parity, res.count_ones() % 2 == 0);
        self.change(ALU88Flags::Sign, res >= 0x80);
        self.change(ALU88Flags::Zero, res == 0);
    }
}

#[derive(Debug)]
pub enum ALU88Codes {
    /// a + b
    Add,
    /// a + b + 1
    Adc,
    /// a - b
    Sub,
    /// a - b - 1
    Sbb,
    /// a & b
    Ana,
    /// a | b
    Ora,
    /// a xor b
    Xra,
    /// !a
    Cma,
    /// -a
    Neg,
    /// Rot left
    Rlc,
    /// Rot left with CY
    Ral,
    /// Rot right
    Rrc,
    /// Rot right with CY
    Rar,
    /// a += 1
    Inr,
    /// a -= 1
    Dcr,
}

/// ALU(Arithmetic Logic Unit) with Flags
#[derive(Debug, Default)]
pub struct ALU88 {}

impl ALU88 {}

impl ALU for ALU88 {
    type Data = u8;
    type Control = ALU88Codes;
    type Status = ALU88Status;
    fn op(&self, code: Self::Control, a: Self::Data, b: Self::Data) -> (Self::Data, Self::Status) {
        use ALU88Codes::*;
        use ALU88Flags::*;
        let mut stat = ALU88Status(0);
        let res = match code {
            Add => {
                let (x, ovf) = a.overflowing_add(b);
                stat.change_by_result(x);
                stat.change(Carry, ovf);
                stat.change(HalfCarry, a % 0x10 + b % 0x10 >= 0x10);
                x
            }
            Adc => {
                let (x, ovf1) = a.overflowing_add(1);
                let (x, ovf2) = x.overflowing_add(b);
                stat.change_by_result(x);
                stat.change(Carry, ovf1 | ovf2);
                stat.change(HalfCarry, a % 0x10 + b % 0x10 + 1 >= 0x10);
                x
            }
            Sub => {
                let (x, ovf) = a.overflowing_sub(b);
                stat.change_by_result(x);
                stat.change(Carry, ovf);
                stat.change(HalfCarry, a % 0x10 < b % 0x10);
                x
            }
            Sbb => {
                let (x, ovf1) = a.overflowing_sub(b);
                let (x, ovf2) = x.overflowing_sub(1);
                stat.change_by_result(x);
                stat.change(Carry, ovf1 | ovf2);
                stat.change(HalfCarry, a % 0x10 < b % 0x10 + 1);
                x
            }
            Ana => {
                let x = a & b;
                stat.change_by_result(x);
                stat.reset(Carry);
                stat.set(HalfCarry);
                x
            }
            Ora => {
                let x = a | b;
                stat.change_by_result(x);
                stat.reset(Carry);
                stat.set(HalfCarry);
                x
            }
            Xra => {
                let x = a ^ b;
                stat.change_by_result(x);
                stat.reset(Carry);
                stat.set(HalfCarry);
                x
            }
            Cma => {
                let x = !a;
                stat.set(HalfCarry);
                x
            }
            Neg => {
                let x = 0u8.wrapping_sub(a);
                stat.change_by_result(x);
                stat.change(Carry, a == 0x00);
                stat.change(HalfCarry, a % 8 == 0);
                x
            }
            Rlc => {
                let x = a.rotate_left(1);
                stat.change(Carry, a >= 0x80);
                stat.reset(HalfCarry);
                x
            }
            Ral => {
                let x = a << 1 | ((b != 0) as u8);
                stat.change(Carry, a >= 0x80);
                stat.reset(HalfCarry);
                x
            }
            Rrc => {
                let x = a.rotate_right(1);
                stat.change(Carry, a % 2 == 1);
                stat.reset(HalfCarry);
                x
            }
            Rar => {
                let x = a >> 1 | ((b != 0) as u8) << 7;
                stat.change(Carry, a % 2 == 1);
                stat.reset(HalfCarry);
                x
            }
            Inr => {
                let x = a.wrapping_add(1);
                stat.change_by_result(x);
                stat.change(HalfCarry, a % 0x10 + 1 >= 0x10);
                x
            }
            Dcr => {
                let x = a.wrapping_sub(1);
                stat.change_by_result(x);
                stat.change(HalfCarry, a % 0x10 == 0);
                x
            }
        };
        (res, stat)
    }
}

#[cfg(test)]
mod test {
    use super::ALU88Codes::*;
    use super::ALU88Flags::*;
    use super::ALU88Status as S;
    use super::*;

    #[test]
    fn add() {
        let mut arith = ALU88::default();
        // no carrying
        assert_eq!(arith.op(Add, 0x23, 0x34), (0x57, S(0b0000_0000)));
        // half carrying
        assert_eq!(arith.op(Add, 0x28, 0x39), (0x61, S(0b0001_0000)));
        // top carrying
        assert_eq!(arith.op(Add, 0x90, 0x81), (0x11, S(0b0000_0101)));
        // both carrying
        assert_eq!(arith.op(Add, 0x98, 0x88), (0x20, S(0b0001_0001)));
        // signed overflow
        assert_eq!(arith.op(Add, 0x4b, 0x38), (0x83, S(0b1001_0000)));
        // ZERO
        assert_eq!(arith.op(Add, 0x81, 0x7f), (0x00, S(0b0101_0101)));
    }

    #[test]
    fn sub() {
        let mut arith = ALU88::default();
        // no borrowing
        assert_eq!(arith.op(Sub, 0x23, 0x11), (0x12, S(0b0000_0100)));
        // half borrowing
        assert_eq!(arith.op(Sub, 0x68, 0x39), (0x2f, S(0b0001_0000)));
        // top borrowing
        assert_eq!(arith.op(Sub, 0x27, 0x81), (0xa6, S(0b1000_0101)));
        // both borrowing
        assert_eq!(arith.op(Sub, 0x23, 0x45), (0xde, S(0b1001_0101)));
        // exact zero
        assert_eq!(arith.op(Sub, 0x67, 0x67), (0x00, S(0b0100_0100)));
    }
}
