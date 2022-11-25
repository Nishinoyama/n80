use crate::instruction::InstructionDecoder;
use crate::memory::{Memory, Memory8Bit64KB};
use crate::n88cpu::alu::ArithmeticLogicUnit88;
use crate::n88cpu::instruction::{Bit16RegisterCode, Bit8RegisterCode, N88InstructionSet};
use crate::register::{R16Bits, R16Bits8Bits, R8Bits, Register, RegisterDividable};

pub struct CPU88<
    M: Memory<u16, u8>,
    R: Register<u16>,
    RD: RegisterDividable<u16, u8>,
    RA: Register<u8>,
> {
    alu: ArithmeticLogicUnit88,
    mem: M,
    a: RA,
    bc: RD,
    de: RD,
    hl: RD,
    sp: R,
    pc: R,
    interruptable: bool,
    halted: bool,
}

impl CPU88<Memory8Bit64KB, R16Bits, R16Bits8Bits, R8Bits> {
    pub fn new() -> CPU88<Memory8Bit64KB, R16Bits, R16Bits8Bits, R8Bits> {
        CPU88 {
            alu: ArithmeticLogicUnit88::default(),
            mem: Memory8Bit64KB::default(),
            a: R8Bits::default(),
            bc: R16Bits8Bits::default(),
            de: R16Bits8Bits::default(),
            hl: R16Bits8Bits::default(),
            sp: R16Bits::default(),
            pc: R16Bits::default(),
            interruptable: false,
            halted: false,
        }
    }
    pub fn flush(&mut self, bytes: &[u8]) {
        bytes
            .iter()
            .enumerate()
            .for_each(|(i, &b)| self.write_mem_at(i as u16, b))
    }

    pub fn fetch(&mut self) -> u8 {
        let t = self.mem.load(self.pc.load());
        self.pc.write(self.pc.load().wrapping_add(1));
        t
    }

    pub fn parse_instruct(&mut self) {
        let mut parser = InstructionDecoder::<N88InstructionSet>::new();
        let inst = loop {
            parser.push(self.fetch());
            if let Some(inst) = parser.decode() {
                break inst;
            }
        };
        self.run_instruct(inst);
    }

    pub fn load8_by_code(&self, r: Bit8RegisterCode) -> u8 {
        match r {
            Bit8RegisterCode::A => self.a.load(),
            Bit8RegisterCode::B => self.bc.load_h(),
            Bit8RegisterCode::C => self.bc.load_l(),
            Bit8RegisterCode::D => self.de.load_h(),
            Bit8RegisterCode::E => self.de.load_l(),
            Bit8RegisterCode::H => self.hl.load_h(),
            Bit8RegisterCode::L => self.hl.load_l(),
        }
    }

    pub fn write8_by_code(&mut self, r: Bit8RegisterCode, b: u8) {
        match r {
            Bit8RegisterCode::A => self.a.write(b),
            Bit8RegisterCode::B => self.bc.write_h(b),
            Bit8RegisterCode::C => self.bc.write_l(b),
            Bit8RegisterCode::D => self.de.write_h(b),
            Bit8RegisterCode::E => self.de.write_l(b),
            Bit8RegisterCode::H => self.hl.write_h(b),
            Bit8RegisterCode::L => self.hl.write_l(b),
        }
    }

    pub fn load16_by_code(&self, rp: Bit16RegisterCode) -> u16 {
        match rp {
            Bit16RegisterCode::BC => self.bc.load(),
            Bit16RegisterCode::DE => self.de.load(),
            Bit16RegisterCode::HL => self.hl.load(),
            Bit16RegisterCode::SP => self.sp.load(),
        }
    }

    pub fn write16_by_code(&mut self, rp: Bit16RegisterCode, bb: u16) {
        match rp {
            Bit16RegisterCode::BC => self.bc.write(bb),
            Bit16RegisterCode::DE => self.de.write(bb),
            Bit16RegisterCode::HL => self.hl.write(bb),
            Bit16RegisterCode::SP => self.sp.write(bb),
        }
    }

    pub fn load_mem_hl(&self) -> u8 {
        self.mem.load(self.hl.load())
    }

    pub fn write_mem_hl(&mut self, bits: u8) {
        self.mem.write(self.hl.load(), bits)
    }

    pub fn load_mem_at(&self, addr: u16) -> u8 {
        self.mem.load(addr)
    }

    pub fn write_mem_at(&mut self, addr: u16, b: u8) {
        self.mem.write(addr, b)
    }

    pub fn load16_mem_at(&self, addr: u16) -> u16 {
        u16::from_ne_bytes([self.mem.load(addr), self.mem.load(addr + 1)])
    }

    pub fn write16_mem_at(&mut self, addr: u16, bb: u16) {
        bb.to_ne_bytes()
            .into_iter()
            .enumerate()
            .for_each(|(i, b)| self.mem.write(addr + i as u16, b))
    }

    pub fn load_acc(&self) -> u8 {
        self.a.load()
    }

    pub fn write_acc(&mut self, b: u8) {
        self.a.write(b)
    }

    pub fn jump(&mut self, addr: u16) {
        self.pc.write(addr)
    }

    pub fn jump_on(&mut self, flag: u8, is: bool, addr: u16) {
        if self.alu.get_flag_of(flag) == is {
            self.jump(addr)
        }
    }

    pub fn call(&mut self, addr: u16) {
        let sp = self.sp.load().wrapping_sub(2);
        self.write16_mem_at(sp, self.pc.load());
        self.sp.write(sp);
        self.jump(addr);
    }

    pub fn call_on(&mut self, flag: u8, is: bool, addr: u16) {
        if self.alu.get_flag_of(flag) == is {
            self.call(addr)
        }
    }

    pub fn ret(&mut self) {
        let sp = self.sp.load();
        self.jump(self.load16_mem_at(sp));
        self.sp.write(sp.wrapping_add(2));
    }

    pub fn ret_on(&mut self, flag: u8, is: bool) {
        if self.alu.get_flag_of(flag) == is {
            self.ret()
        }
    }

    pub fn run_instruct(&mut self, inst: N88InstructionSet) {
        use super::alu::*;
        use Bit16RegisterCode::*;
        use Bit8RegisterCode::*;
        use N88InstructionSet::*;
        match inst {
            MovRR(d, s) => self.write8_by_code(d, self.load8_by_code(s)),
            MovRM(r) => self.write8_by_code(r, self.load_mem_hl()),
            MovMR(r) => self.write_mem_hl(self.load8_by_code(r)),
            MviR(r, b) => self.write8_by_code(r, b),
            MviM(b) => self.write_mem_hl(b),
            LxiR(rp, bb) => self.write16_by_code(rp, bb),
            Lda(addr) => self.write_acc(self.load_mem_at(addr)),
            Sta(addr) => self.write_mem_at(addr, self.load_acc()),
            Lhld(addr) => self.write16_by_code(HL, self.load16_mem_at(addr)),
            Shld(addr) => self.write16_mem_at(addr, self.load16_by_code(HL)),
            Ldax(rp) => self.write_acc(self.load_mem_at(self.load16_by_code(rp))),
            Stax(rp) => self.write_mem_at(self.load16_by_code(rp), self.load_acc()),
            Xchg => {
                let (hl, de) = (self.hl.load(), self.de.load());
                self.de.write(hl);
                self.hl.write(de);
            }
            AddR(r) => {
                let a = self.alu.add(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            AddM => {
                let a = self.alu.add(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Adi(n) => {
                let a = self.alu.add(self.load_acc(), n);
                self.write_acc(a);
            }
            AdcR(r) => {
                let a = self.alu.add_carried(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            AdcM => {
                let a = self.alu.add_carried(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Aci(n) => {
                let a = self.alu.add_carried(self.load_acc(), n);
                self.write_acc(a);
            }
            SubR(r) => {
                let a = self.alu.sub(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            SubM => {
                let a = self.alu.sub(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Sui(n) => {
                let a = self.alu.sub(self.load_acc(), n);
                self.write_acc(a);
            }
            SbbR(r) => {
                let a = self.alu.sub(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            SbbM => {
                let a = self.alu.sub(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Sbi(n) => {
                let a = self.alu.sub(self.load_acc(), n);
                self.write_acc(a);
            }
            InrR(r) => {
                let a = self.alu.inr(self.load8_by_code(r));
                self.write8_by_code(r, a);
            }
            InrM => {
                let a = self.alu.inr(self.load_mem_hl());
                self.write_mem_hl(a);
            }
            DcrR(r) => {
                let a = self.alu.dcr(self.load8_by_code(r));
                self.write8_by_code(r, a);
            }
            DcrM => {
                let a = self.alu.inr(self.load_mem_hl());
                self.write_mem_hl(a);
            }
            Inx(rp) => {
                let a = self.alu.inx(self.load16_by_code(rp));
                self.write16_by_code(rp, a);
            }
            Dcx(rp) => {
                let a = self.alu.inx(self.load16_by_code(rp));
                self.write16_by_code(rp, a);
            }
            Dad(rp) => {
                let hl = self
                    .alu
                    .dad(self.load16_by_code(HL), self.load16_by_code(rp));
                self.write16_by_code(HL, hl);
            }
            Daa => {
                // TODO: neg true?
                let daa = self.alu.daa(self.load_acc(), false);
                self.write_acc(daa);
            }
            AnaR(r) => {
                let a = self.alu.and(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            AnaM => {
                let a = self.alu.and(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Ani(n) => {
                let a = self.alu.and(self.load_acc(), n);
                self.write_acc(a);
            }
            XraR(r) => {
                let a = self.alu.xor(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            XraM => {
                let a = self.alu.xor(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Xri(n) => {
                let a = self.alu.xor(self.load_acc(), n);
                self.write_acc(a);
            }
            OraR(r) => {
                let a = self.alu.or(self.load_acc(), self.load8_by_code(r));
                self.write_acc(a);
            }
            OraM => {
                let a = self.alu.or(self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Ori(n) => {
                let a = self.alu.or(self.load_acc(), n);
                self.write_acc(a);
            }
            CmpR(r) => {
                self.alu.sub(self.load_acc(), self.load8_by_code(r));
            }
            CmpM => {
                self.alu.sub(self.load_acc(), self.load_mem_hl());
            }
            Cpi(n) => {
                self.alu.sub(self.load_acc(), n);
            }
            Rlc => {
                let a = self.alu.rlc(self.load_acc());
                self.write_acc(a)
            }
            Rrc => {
                let a = self.alu.rrc(self.load_acc());
                self.write_acc(a)
            }
            Ral => {
                let a = self.alu.rrc(self.load_acc());
                self.write_acc(a)
            }
            Rar => {
                let a = self.alu.rar(self.load_acc());
                self.write_acc(a)
            }
            Cma => self.write_acc(!self.load_acc()),
            Cmc => self.alu.cmc(),
            Stc => self.alu.stc(),
            Jmp(addr) => self.jump(addr),
            Jc(addr) => self.jump_on(F_CARRY, true, addr),
            Jnc(addr) => self.jump_on(F_CARRY, false, addr),
            Jz(addr) => self.jump_on(F_ZERO, true, addr),
            Jnz(addr) => self.jump_on(F_ZERO, false, addr),
            Jp(addr) => self.jump_on(F_SIGN, false, addr),
            Jm(addr) => self.jump_on(F_SIGN, true, addr),
            Jpe(addr) => self.jump_on(F_PARITY, true, addr),
            Jpo(addr) => self.jump_on(F_PARITY, false, addr),
            Call(addr) => self.call(addr),
            Cc(addr) => self.call_on(F_CARRY, true, addr),
            Cnc(addr) => self.call_on(F_CARRY, false, addr),
            Cz(addr) => self.call_on(F_ZERO, true, addr),
            Cnz(addr) => self.call_on(F_ZERO, false, addr),
            Cp(addr) => self.call_on(F_SIGN, false, addr),
            Cm(addr) => self.call_on(F_SIGN, true, addr),
            Cpe(addr) => self.call_on(F_PARITY, true, addr),
            Cpo(addr) => self.call_on(F_PARITY, false, addr),
            Ret => self.ret(),
            Rc => self.ret_on(F_CARRY, true),
            Rnc => self.ret_on(F_CARRY, false),
            Rz => self.ret_on(F_ZERO, true),
            Rnz => self.ret_on(F_ZERO, false),
            Rp => self.ret_on(F_SIGN, false),
            Rm => self.ret_on(F_SIGN, true),
            Rpe => self.ret_on(F_PARITY, true),
            Rpo => self.ret_on(F_PARITY, false),
            PcHL => self.pc.write(self.hl.load()),
            PushRP(rp) => {
                let sp = self.sp.load().wrapping_sub(2);
                self.write16_mem_at(sp, self.load16_by_code(rp));
                self.sp.write(sp);
            }
            PushPSW => {
                let sp = self.sp.load().wrapping_sub(2);
                self.write_mem_at(sp.wrapping_add(1), self.load_acc());
                self.write_mem_at(sp, self.alu.load_flag() | 0b10);
                self.sp.write(sp);
            }
            PopRP(rp) => {
                let sp = self.sp.load();
                self.write16_by_code(rp, self.load16_mem_at(sp));
                self.sp.write(sp.wrapping_add(2));
            }
            PopPSW => {
                let sp = self.sp.load();
                self.alu.write_flag(self.load_mem_at(sp) | !0b10);
                self.a.write(self.load_mem_at(sp.wrapping_add(1)));
                self.sp.write(sp.wrapping_add(2));
            }
            XtHL => {
                let sp = self.sp.load();
                self.hl.write(self.load16_mem_at(sp));
            }
            SpHL => self.sp.write(self.hl.load()),
            In(port) => {
                let mut buf = String::new();
                std::io::stdin().read_line(&mut buf).expect("IO error");
                let a = buf.parse().unwrap_or_default();
                self.write_acc(a)
            }
            Out(port) => {
                println!("{}", self.a.load());
            }
            EI => self.interruptable = true,
            DI => self.interruptable = false,
            Rst(addr) => {
                let sp = self.sp.load().wrapping_sub(2);
                self.write16_mem_at(sp, self.pc.load());
                self.sp.write(sp);
                self.pc.write(addr * 8);
            }
            Hlt => self.halted = true,
            Nop => {}
        }
        #[test]
        fn cpu_test() {
            let mut cpu = CPU88::new();
        }
    }
}
