pub trait RegisterSet<C> {
    type RegisterSize;
    fn load(&self, code: C) -> Self::RegisterSize;
    fn write(&mut self, code: C, bits: Self::RegisterSize);
}

#[cfg(test)]
mod test {
    use crate::register::{R16Bits, R16Bits8Bits, R8Bits, Register, RegisterDividable};
    use crate::register_set::RegisterSet;

    #[derive(Debug, Default)]
    struct TestRegisters {
        af: R16Bits8Bits,
        hl: R16Bits8Bits,
        pc: R16Bits,
    }

    enum TestRegister8Code {
        A,
        F,
        H,
        L,
    }

    enum TestRegister16Code {
        AF,
        HL,
        PC,
    }

    impl RegisterSet<TestRegister8Code> for TestRegisters {
        type RegisterSize = <R8Bits as Register>::Size;
        fn load(&self, code: TestRegister8Code) -> Self::RegisterSize {
            match code {
                TestRegister8Code::A => self.af.load_h(),
                TestRegister8Code::F => self.af.load_l(),
                TestRegister8Code::H => self.hl.load_h(),
                TestRegister8Code::L => self.hl.load_l(),
            }
        }
        fn write(&mut self, code: TestRegister8Code, bits: Self::RegisterSize) {
            match code {
                TestRegister8Code::A => self.af.write_h(bits),
                TestRegister8Code::F => self.af.write_l(bits),
                TestRegister8Code::H => self.hl.write_h(bits),
                TestRegister8Code::L => self.hl.write_l(bits),
            }
        }
    }

    impl RegisterSet<TestRegister16Code> for TestRegisters {
        type RegisterSize = <R16Bits as Register>::Size;
        fn load(&self, code: TestRegister16Code) -> <R16Bits as Register>::Size {
            match code {
                TestRegister16Code::AF => self.af.load(),
                TestRegister16Code::HL => self.hl.load(),
                TestRegister16Code::PC => self.pc.load(),
            }
        }
        fn write(&mut self, code: TestRegister16Code, bits: <R16Bits as Register>::Size) {
            match code {
                TestRegister16Code::AF => self.af.write(bits),
                TestRegister16Code::HL => self.hl.write(bits),
                TestRegister16Code::PC => self.pc.write(bits),
            }
        }
    }

    #[test]
    fn test() {
        use TestRegister16Code::*;
        use TestRegister8Code::*;
        let mut rs = TestRegisters::default();
        rs.write(A, 20);
        rs.load(A);
        assert_eq!(rs.load(A), 20);
        rs.write(AF, 0x1729);
        assert_eq!(rs.load(A), 0x29);
        assert_eq!(rs.load(F), 0x17);
    }
}
