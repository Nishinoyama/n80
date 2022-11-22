use std::mem::transmute;

pub trait Register8 {
    fn write(&mut self, bits: u8);
    fn load(&self) -> u8;
}

pub trait Register16 {
    fn write(&mut self, bits: u16);
    fn load(&self) -> u16;
}

pub trait Register16Dividable: Register16 {
    fn write_l(&mut self, bits: u8);
    fn load_l(&self) -> u8;
    fn write_h(&mut self, bits: u8);
    fn load_h(&self) -> u8;
}

#[derive(Debug, Default, Clone)]
pub struct R16Bits {
    bits: u16,
}

impl Register16 for R16Bits {
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

impl Register16Dividable for R16Bits8Bits {
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

impl Register16 for R16Bits8Bits {
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

impl Register8 for R8Bits {
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
    use crate::register::{R16Bits8Bits, Register16, Register16Dividable};

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
