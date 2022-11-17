use crate::register::{R16Bits, R16Bits8Bits, Register16, Register16Dividable};

trait CPU<I, A> {
    fn clock(&mut self);
    fn set_instruct(&mut self) -> I;
    fn get_addr(&self) -> A;
}

#[derive(Default, Debug, Clone)]
/// Little Endian
/// CISC
struct N8080 {
    /// Accumulator and Flag
    /// b0: (C) Carry
    /// b2: (P) Parity
    /// b4: (H) AUX Carry
    /// b6: (Z) Zero
    /// b7: (S) Sign
    /// PSW:16 A:8h Flag:8l
    psw: R16Bits8Bits,
    /// General register
    /// BC:16 B:8h C:8l
    bc: R16Bits8Bits,
    /// DE:16 D:8h E:8l
    de: R16Bits8Bits,
    /// General and Indirect Address
    /// HL:16 H:8h L:8l
    hl: R16Bits8Bits,
    /// Program Counter
    pc: R16Bits,
    /// Stack Pointer
    /// Decrease pointer address to grow stack
    sp: R16Bits,
    /// interrupt ff
    iff: bool,
}

impl CPU<u8, u16> for N8080 {
    fn clock(&mut self) {
        self.pc.write(self.pc.load().wrapping_add(1))
    }
    fn set_instruct(&mut self) -> u8 {
        todo!()
    }
    fn get_addr(&self) -> u16 {
        todo!()
    }
}

const F_C: u8 = 1;
const F_N: u8 = 2;
const F_P: u8 = 4;
const F_H: u8 = 16;
const F_Z: u8 = 64;
const F_S: u8 = 128;

impl N8080 {
    #[inline]
    fn write_flag(&mut self, flag: u8) {
        self.psw.write_l(flag);
    }
    #[inline]
    fn set_flag_of(&mut self, flag: u8) {
        self.write_flag(self.get_flag() | flag);
    }
    #[inline]
    fn reset_flag_of(&mut self, flag: u8) {
        self.write_flag(self.get_flag() & !flag);
    }
    #[inline]
    fn get_flag_at(&mut self, flag: u8) -> bool {
        (self.get_flag() & flag) == flag
    }
    #[inline]
    fn get_flag(&mut self) -> u8 {
        self.psw.load_l()
    }
    #[inline]
    fn alu_add(&mut self, a: u8, n: u8) -> u8 {
        let (x, ovf) = a.overflowing_add(n);
        let mut flag = 0u8;
        if ovf {
            flag |= F_P;
            flag |= F_C;
        };
        if x >= 0x80 {
            flag |= F_N;
        };
        if x == 0x00 {
            flag |= F_Z;
        };
        if a % 8 + n % 8 > 8 {
            flag |= F_H;
        };
        self.write_flag(flag);
        x
    }
    #[inline]
    /// add with carry
    fn alu_adc(&mut self, a: u8, n: u8) -> u8 {
        if !self.get_flag_at(F_C) {
            return self.alu_add(a, n);
        }
        let (n, ovf_n) = n.overflowing_add(1);
        let (x, ovf) = a.overflowing_add(n);
        let mut flag = 0u8;
        if ovf | ovf_n {
            flag |= F_P;
            flag |= F_C;
        };
        if x >= 0x80 {
            flag |= F_N;
        };
        if x == 0x00 {
            flag |= F_Z;
        };
        if a % 8 + n % 8 + 1 > 8 {
            flag |= F_H;
        };
        self.write_flag(flag);
        x
    }
    fn alu_sub(&mut self, a: u8, n: u8) -> u8 {
        self.alu_adc(a, !n)
    }
    fn alu_sbc(&mut self, a: u8, n: u8) -> u8 {
        self.alu_add(a, !n)
    }
    fn alu_and(&mut self, a: u8, n: u8) -> u8 {
        let x = a & n;
        let mut flg = F_H;
        if x >= 0x80 {
            flg |= F_S;
        };
        if x == 0x00 {
            flg |= F_Z;
        };
        self.write_flag(flg);
        x
    }
    fn alu_or(&mut self, a: u8, n: u8) -> u8 {
        let x = a | n;
        let mut flg = F_H;
        if x >= 0x80 {
            flg |= F_S;
        };
        if x == 0x00 {
            flg |= F_Z;
        };
        self.write_flag(flg);
        x
    }
    fn alu_xor(&mut self, a: u8, n: u8) -> u8 {
        let x = a | n;
        let mut flg = F_H;
        if x >= 0x80 {
            flg |= F_S;
        };
        if x == 0x00 {
            flg |= F_Z;
        };
        if x.count_ones() % 2 == 0 {
            flg |= F_P;
        }
        self.write_flag(flg);
        x
    }
    fn alu_inc(&mut self, a: u8) -> u8 {
        let x = a.wrapping_add(1);
        let mut flg = self.get_flag() & F_C;
        if x >= 0x80 {
            flg |= F_S;
        };
        if x == 0x00 {
            flg |= F_Z;
        };
        if a % 8 == 0x07 {
            flg |= F_H;
        };
        if a == 0x7f {
            flg |= F_P;
        };
        self.write_flag(flg);
        x
    }
    fn alu_dec(&mut self, a: u8) -> u8 {
        let x = a.wrapping_sub(1);
        let mut flg = self.get_flag() & F_C;
        if x >= 0x80 {
            flg |= F_S;
        };
        if x == 0x00 {
            flg |= F_Z;
        };
        if a % 8 == 0x00 {
            flg |= F_H;
        };
        if a == 0x80 {
            flg |= F_P;
        };
        self.write_flag(flg);
        x
    }
    fn alu_daa(&mut self, a: u8) -> u8 {
        todo!()
    }
    /// get complement
    fn alu_cpl(&mut self, a: u8) -> u8 {
        self.set_flag_of(F_H);
        self.set_flag_of(F_N);
        !a
    }
    fn alu_neg(&mut self, a: u8) -> u8 {
        self.alu_sub(0, a)
    }
}
