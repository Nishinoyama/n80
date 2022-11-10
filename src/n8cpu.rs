use std::fmt::Debug;

#[derive(Default, Debug)]
pub struct N8 {
    cpu: CPU,
    mem: Ram256BU8,
}

impl N8 {
    pub fn cycle(&mut self) -> Result<(), code::DecodeError> {
        let op = self.fetch();
        let inst = code::decoder(op)?;
        self.cpu.op(inst);
        Ok(())
    }
    pub fn fetch(&mut self) -> u16 {
        self.cpu.pc += 2;
        u16::from_le_bytes([
            self.mem.read(self.cpu.pc - 2),
            self.mem.read(self.cpu.pc - 1),
        ])
    }
    pub fn flush(&mut self, dis: u8, data: &[u8]) {
        data.into_iter()
            .enumerate()
            .for_each(|(i, &d)| self.mem.write(dis + i as u8, d));
    }
    pub fn get_output(&self) -> u8 {
        self.cpu.or
    }
}

#[derive(Default, Debug)]
pub struct CPU {
    /// general register a
    a: u8,
    /// program counter register
    pc: u8,
    /// output register
    or: u8,
    /// flag register
    f: u8,
}

impl CPU {
    fn get_flg_at(&self, dis: usize) -> bool {
        (self.f & (1 << dis)) != 0
    }
    fn set_flg_at(&mut self, dis: usize, flg: bool) {
        if flg {
            self.f |= 1 << dis
        } else {
            self.f &= !(1 << dis)
        }
    }
    fn get_carry_flg(&self) -> bool {
        self.get_flg_at(0)
    }
    fn set_carry_flg(&mut self, flg: bool) {
        self.set_flg_at(0, flg);
    }
    fn alu_add_im(&mut self, im: u8) {
        let (a, carry) = self.a.overflowing_add(im);
        self.set_carry_flg(carry);
        self.a = a;
    }
    pub fn op(&mut self, inst: InstructionSet) {
        inst.op(self);
    }
}

#[derive(Debug)]
pub struct Ram256BU8 {
    mem: [u8; 65536],
}

impl Default for Ram256BU8 {
    fn default() -> Self {
        Ram256BU8 { mem: [0; 65536] }
    }
}

impl Ram256BU8 {
    pub fn read(&self, addr: u8) -> u8 {
        self.mem[addr as usize]
    }
    pub fn write(&mut self, addr: u8, data: u8) {
        self.mem[addr as usize] = data;
    }
}

pub enum InstructionSet {
    OutIm(u8),
    OutA,
    LdAIm(u8),
    AddAIm(u8),
    Jp(u8),
    JpF(u8),
    Nop,
    Others(Box<dyn OtherInstruction>),
}

impl InstructionSet {
    pub fn op(self, cpu: &mut CPU) {
        use InstructionSet::*;
        match self {
            OutIm(im) => cpu.or = im,
            OutA => cpu.or = cpu.a,
            LdAIm(im) => cpu.a = im,
            AddAIm(im) => cpu.alu_add_im(im),
            Jp(addr) => cpu.pc = addr,
            JpF(addr) => {
                if cpu.get_carry_flg() {
                    cpu.pc = addr
                }
            }
            Nop => {}
            Others(t) => t.op(cpu),
        }
    }
}

pub trait OtherInstruction {
    fn op(&self, cpu: &mut CPU);
}

pub mod code {
    use super::InstructionSet;

    pub fn decoder(inst: u16) -> Result<InstructionSet, DecodeError> {
        use DecodeError::*;
        use InstructionSet::*;
        match inst.to_le_bytes() {
            [x, y] if x < 7 => Ok(match (x, y) {
                (0, im) => OutIm(im),
                (1, addr) => Jp(addr),
                (2, im) => LdAIm(im),
                (3, _) => OutA,
                (5, im) => AddAIm(im),
                (6, addr) => JpF(addr),
                (_, _) => Nop,
            }),
            _ => Err(UnknownCode(inst)),
        }
    }

    #[derive(Debug)]
    pub enum DecodeError {
        UnknownCode(u16),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cpu_works() {
        let mut cpu = CPU::default();
        use InstructionSet::*;
        cpu.op(LdAIm(20));
        cpu.op(AddAIm(30));
        cpu.op(JpF(200));
        assert_eq!(cpu.a, 50);
        assert_eq!(cpu.pc, 0);
        assert_eq!(cpu.f, 0);
        cpu.op(AddAIm(220));
        cpu.op(JpF(220));
        assert_eq!(cpu.a, 14);
        assert_eq!(cpu.pc, 220);
        assert_eq!(cpu.f, 1);
    }

    #[test]
    fn n8_works() -> Result<(), code::DecodeError> {
        let mut n8 = N8::default();
        n8.flush(
            0,
            &[
                0x02, 0x32, 0x05, 0x24, 0x03, 0x00, 0x05, 0xff, 0x06, 0x0c, 0x05, 0x22, 0x03, 0x00,
            ],
        );
        // ld 0x32, add 0x24, out, add 0xff, jpf 0x0c, add 0x22, out
        for _ in 0..6 {
            n8.cycle()?;
        }
        assert_eq!(n8.get_output(), 0x55);
        assert_eq!(n8.cpu.f, 1);
        Ok(())
    }
}
