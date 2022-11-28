use std::mem::transmute;

pub trait Register {
    type Size;
    fn write(&mut self, bits: Self::Size);
    fn load(&self) -> Self::Size;
}

pub trait RegisterDividable<C>: Register {
    fn write_l(&mut self, bits: C);
    fn load_l(&self) -> C;
    fn write_h(&mut self, bits: C);
    fn load_h(&self) -> C;
}

#[derive(Debug, Default, Clone)]
pub struct R16Bits {
    bits: u16,
}

impl Register for R16Bits {
    type Size = u16;
    #[inline]
    fn write(&mut self, bits: u16) {
        self.bits = bits
    }
    #[inline]
    fn load(&self) -> u16 {
        self.bits
    }
}

#[derive(Debug, Default, Clone)]
pub struct R16Bits8Bits {
    bits: u16,
}

impl RegisterDividable<u8> for R16Bits8Bits {
    fn write_l(&mut self, bits: u8) {
        unsafe {
            *transmute::<&u16, *mut u8>(&self.bits).add(1) = bits;
        }
    }
    #[inline]
    fn load_l(&self) -> u8 {
        ((self.bits >> 8) & 0xff) as u8
    }
    fn write_h(&mut self, bits: u8) {
        unsafe {
            *transmute::<&u16, *mut u8>(&self.bits) = bits;
        }
    }
    #[inline]
    fn load_h(&self) -> u8 {
        (self.bits & 0xff) as u8
    }
}

impl Register for R16Bits8Bits {
    type Size = u16;
    #[inline]
    fn write(&mut self, bits: u16) {
        self.bits = bits
    }
    #[inline]
    fn load(&self) -> u16 {
        self.bits
    }
}

#[derive(Debug, Default, Clone)]
pub struct R8Bits {
    bits: u8,
}

impl Register for R8Bits {
    type Size = u8;
    #[inline]
    fn write(&mut self, bits: u8) {
        self.bits = bits
    }
    #[inline]
    fn load(&self) -> u8 {
        self.bits
    }
}

#[cfg(test)]
mod test {
    use crate::register::{R16Bits8Bits, Register, RegisterDividable};

    #[test]
    fn set_get_register() {
        let mut reg = R16Bits8Bits::default();
        reg.write(0x0123_u16.to_be());
        assert_eq!(reg.load().to_be(), 0x0123);
        assert_eq!(reg.load_h(), 0x01);
        assert_eq!(reg.load_l(), 0x23);
        reg.write_h(0x45);
        assert_eq!(reg.load().to_be(), 0x4523);
        assert_eq!(reg.load_h(), 0x45);
        assert_eq!(reg.load_l(), 0x23);
        reg.write_l(0x67);
        assert_eq!(reg.load().to_be(), 0x4567);
        assert_eq!(reg.load_h(), 0x45);
        assert_eq!(reg.load_l(), 0x67);
    }
}
