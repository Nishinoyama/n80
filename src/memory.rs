pub trait Memory {
    type Address;
    type Data;
    fn load(&self, index: Self::Address) -> Self::Data;
    fn write(&mut self, index: Self::Address, data: Self::Data);
}

#[derive(Debug)]
pub struct Memory8Bit64KB {
    bytes: [u8; 65536],
}

impl Default for Memory8Bit64KB {
    fn default() -> Self {
        Memory8Bit64KB {
            bytes: [0u8; 65536],
        }
    }
}

impl Memory for Memory8Bit64KB {
    type Address = u16;
    type Data = u8;
    fn load(&self, index: u16) -> u8 {
        self.bytes[index as usize]
    }
    fn write(&mut self, index: u16, data: u8) {
        self.bytes[index as usize] = data
    }
}
