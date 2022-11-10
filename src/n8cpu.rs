pub struct N8 {
    cpu: CPU,
    mem: Ram256BU8,
}

#[derive(Default)]
pub struct CPU {
    /// general register a
    a: u8,
    /// program counter register
    pc: u8,
    /// output register
    or: u8,
    /// fetch register
    fr: u8,
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
}
