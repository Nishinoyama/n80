pub trait Memory<S, T> {
    fn load(&self, index: S) -> T;
    fn write(&mut self, index: S, data: T);
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

impl Memory<u16, u8> for Memory8Bit64KB {
    fn load(&self, index: u16) -> u8 {
        self.bytes[index as usize]
    }
    fn write(&mut self, index: u16, data: u8) {
        self.bytes[index as usize] = data
    }
}
