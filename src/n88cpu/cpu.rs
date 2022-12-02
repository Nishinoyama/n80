use crate::alu::ALU;
use crate::instruction::InstructionDecoder;
use crate::memory::{Memory, Memory8Bit64KB};
use crate::n88cpu::alu::{ALU88Codes, ALU88Status, ALU88};
use crate::n88cpu::instruction::{Bit16RegisterCode, Bit8RegisterCode, N88InstructionSet};
use crate::register::{R16Bits, R16Bits8Bits, R8Bits, Register, RegisterDividable};
use crate::register_set::RegisterSet;
use std::fmt::{Debug, Formatter};

#[derive(Default)]
pub struct N88RegisterSet {
    a: R8Bits,
    /// Flag
    /// b0: (C) Carry
    /// b2: (P) Parity
    /// b4: (H) AUX Carry
    /// b6: (Z) Zero
    /// b7: (S) Sign
    flag: R8Bits,
    bc: R16Bits8Bits,
    de: R16Bits8Bits,
    hl: R16Bits8Bits,
    sp: R16Bits,
}

impl Debug for N88RegisterSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = String::new();
        builder += &format!("sp: {:04x}\n", self.sp.load());
        builder += &format!("a : {:08b}\n", self.a.load());
        builder += &format!("bc: {:08b} {:08b}\n", self.bc.load_h(), self.bc.load_l());
        builder += &format!("de: {:08b} {:08b}\n", self.de.load_h(), self.de.load_l());
        builder += &format!("hl: {:08b} {:08b}\n", self.hl.load_h(), self.hl.load_l());
        builder += "mem:\n";
        writeln!(f, "{}", builder)
    }
}

impl RegisterSet<Bit8RegisterCode> for N88RegisterSet {
    type RegisterSize = u8;

    fn load(&self, code: Bit8RegisterCode) -> Self::RegisterSize {
        use Bit8RegisterCode::*;
        match code {
            A => self.a.load(),
            B => self.bc.load_h(),
            C => self.bc.load_l(),
            D => self.de.load_h(),
            E => self.de.load_l(),
            H => self.hl.load_h(),
            L => self.hl.load_l(),
        }
    }

    fn write(&mut self, code: Bit8RegisterCode, bits: Self::RegisterSize) {
        use Bit8RegisterCode::*;
        match code {
            A => self.a.write(bits),
            B => self.bc.write_h(bits),
            C => self.bc.write_l(bits),
            D => self.de.write_h(bits),
            E => self.de.write_l(bits),
            H => self.hl.write_h(bits),
            L => self.hl.write_l(bits),
        }
    }
}

impl RegisterSet<Bit16RegisterCode> for N88RegisterSet {
    type RegisterSize = u16;

    fn load(&self, code: Bit16RegisterCode) -> Self::RegisterSize {
        use Bit16RegisterCode::*;
        match code {
            BC => self.bc.load(),
            DE => self.de.load(),
            HL => self.hl.load(),
            SP => self.sp.load(),
        }
    }

    fn write(&mut self, code: Bit16RegisterCode, bits: Self::RegisterSize) {
        use Bit16RegisterCode::*;
        match code {
            BC => self.bc.write(bits),
            DE => self.de.write(bits),
            HL => self.hl.write(bits),
            SP => self.sp.write(bits),
        }
    }
}

#[derive(Default)]
pub struct CPU88<
    A: ALU<Data = u8, Control = ALU88Codes, Status = ALU88Status>,
    M: Memory<Address = u16, Data = u8>,
> {
    alu: A,
    mem: M,
    registers: N88RegisterSet,
    pc: R16Bits,
    interruptable: bool,
    halted: bool,
}

impl Debug for CPU88<ALU88, Memory8Bit64KB> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = String::new();
        builder += &format!("{:?}\n", self.registers);
        builder += "mem:\n";
        builder += &format!("({:04x}):", self.pc.load());
        (self.pc.load()..self.pc.load().saturating_add(32)).for_each(|i| {
            builder += &format!(" {:02x}", self.mem.load(i));
        });
        writeln!(f, "{}", builder)
    }
}

impl CPU88<ALU88, Memory8Bit64KB> {
    pub fn new() -> Self {
        CPU88 {
            alu: ALU88::default(),
            mem: Memory8Bit64KB::default(),
            registers: N88RegisterSet::default(),
            pc: Default::default(),
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

    fn fetch(&mut self) -> u8 {
        let t = self.mem.load(self.pc.load());
        self.pc.write(self.pc.load().wrapping_add(1));
        t
    }

    fn fetch_instruct(&mut self) -> N88InstructionSet {
        let mut parser = InstructionDecoder::<N88InstructionSet>::default();
        loop {
            parser.push(self.fetch());
            if let Some(inst) = parser.decode() {
                return inst;
            }
        }
    }

    fn load_by_code(&self, code: Bit8RegisterCode) -> u8 {
        self.registers.load(code)
    }

    fn write_by_code(&mut self, code: Bit8RegisterCode, bits: u8) {
        self.registers.write(code, bits);
    }

    fn load16_by_code(&self, code: Bit16RegisterCode) -> u16 {
        self.registers.load(code)
    }

    fn write16_by_code(&mut self, code: Bit16RegisterCode, bits: u16) {
        self.registers.write(code, bits);
    }

    fn load_mem_hl(&self) -> u8 {
        self.mem.load(self.registers.hl.load())
    }

    fn write_mem_hl(&mut self, bits: u8) {
        self.mem.write(self.registers.hl.load(), bits)
    }

    fn load_mem_at(&self, addr: u16) -> u8 {
        self.mem.load(addr)
    }

    fn write_mem_at(&mut self, addr: u16, b: u8) {
        self.mem.write(addr, b)
    }

    fn load16_mem_at(&self, addr: u16) -> u16 {
        u16::from_ne_bytes([self.mem.load(addr), self.mem.load(addr + 1)])
    }

    fn write16_mem_at(&mut self, addr: u16, bb: u16) {
        bb.to_ne_bytes()
            .into_iter()
            .enumerate()
            .for_each(|(i, b)| self.mem.write(addr + i as u16, b))
    }

    fn load_acc(&self) -> u8 {
        self.registers.a.load()
    }

    fn write_acc(&mut self, b: u8) {
        self.registers.a.write(b)
    }

    fn jump(&mut self, addr: u16) {
        self.pc.write(addr)
    }

    fn jump_on(&mut self, flag: u8, is: bool, addr: u16) {
        if self.alu.get_flag_of(flag) == is {
            self.jump(addr)
        }
    }

    fn call(&mut self, addr: u16) {
        let sp = self.registers.sp.load().wrapping_sub(2);
        self.write16_mem_at(sp, self.pc.load());
        self.registers.sp.write(sp);
        self.jump(addr);
    }

    fn call_on(&mut self, flag: u8, is: bool, addr: u16) {
        if self.alu.get_flag_of(flag) == is {
            self.call(addr)
        }
    }

    fn ret(&mut self) {
        let sp = self.registers.sp.load();
        self.jump(self.load16_mem_at(sp));
        self.registers.sp.write(sp.wrapping_add(2));
    }

    fn ret_on(&mut self, flag: u8, is: bool) {
        if self.alu.get_flag_of(flag) == is {
            self.ret()
        }
    }

    pub fn run(&mut self) {
        self.halted = false;
        while !self.halted {
            let inst = self.fetch_instruct();
            self.run_instruct(inst);
        }
    }

    pub fn run_instruct(&mut self, inst: N88InstructionSet) {
        use super::alu::ALU88Codes as AC;
        use super::alu::ALU88Flags::*;
        use Bit16RegisterCode::*;
        use Bit8RegisterCode::*;
        use N88InstructionSet::*;
        match inst {
            MovRR(d, s) => self.write_by_code(d, self.load_by_code(s)),
            MovRM(r) => self.write_by_code(r, self.load_mem_hl()),
            MovMR(r) => self.write_mem_hl(self.load_by_code(r)),
            MviR(r, b) => self.write_by_code(r, b),
            MviM(b) => self.write_mem_hl(b),
            LxiR(rp, bb) => self.write16_by_code(rp, bb),
            Lda(addr) => self.write_acc(self.load_mem_at(addr)),
            Sta(addr) => self.write_mem_at(addr, self.load_acc()),
            Lhld(addr) => self.write16_by_code(HL, self.load16_mem_at(addr)),
            Shld(addr) => self.write16_mem_at(addr, self.load16_by_code(HL)),
            Ldax(rp) => self.write_acc(self.load_mem_at(self.load16_by_code(rp))),
            Stax(rp) => self.write_mem_at(self.load16_by_code(rp), self.load_acc()),
            Xchg => {
                let (hl, de) = (self.registers.hl.load(), self.registers.de.load());
                self.registers.de.write(hl);
                self.registers.hl.write(de);
            }
            AddR(r) => {
                let a = self.alu.op(AC::Add, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            AddM => {
                let a = self.alu.op(AC::Add, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Adi(n) => {
                let a = self.alu.op(AC::Add, self.load_acc(), n);
                self.write_acc(a);
            }
            AdcR(r) => {
                let a = self.alu.op(AC::Adc, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            AdcM => {
                let a = self.alu.op(AC::Adc, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Aci(n) => {
                let a = self.alu.op(AC::Adc, self.load_acc(), n);
                self.write_acc(a);
            }
            SubR(r) => {
                let a = self.alu.op(AC::Sub, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            SubM => {
                let a = self.alu.op(AC::Sub, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Sui(n) => {
                let a = self.alu.op(AC::Sub, self.load_acc(), n);
                self.write_acc(a);
            }
            SbbR(r) => {
                let a = self.alu.op(AC::Sub, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            SbbM => {
                let a = self.alu.op(AC::Sub, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Sbi(n) => {
                let a = self.alu.op(AC::Sub, self.load_acc(), n);
                self.write_acc(a);
            }
            InrR(r) => {
                let a = self.alu.op(AC::Inr, self.load_by_code(r));
                self.write_by_code(r, a);
            }
            InrM => {
                let a = self.alu.op(AC::Inr, self.load_mem_hl());
                self.write_mem_hl(a);
            }
            DcrR(r) => {
                let a = self.alu.op(AC::Dcr, self.load_by_code(r));
                self.write_by_code(r, a);
            }
            DcrM => {
                let a = self.alu.op(AC::Dcr, self.load_mem_hl());
                self.write_mem_hl(a);
            }
            Inx(rp) => {
                todo!("flags!");
                let a = self.load16_by_code(rp).wrapping_add(1);
                self.write16_by_code(rp, a);
            }
            Dcx(rp) => {
                let a = self.load16_by_code(rp).wrapping_sub(1);
                self.write16_by_code(rp, a);
            }
            Dad(rp) => {
                let hl = self
                    .alu
                    .dad(self.registers.hl.load(), self.load16_by_code(rp));
                self.registers.hl.write(hl);
            }
            Daa => {
                // TODO: neg true?
                let mut x = a;
                if x % 0x10 > 9 || self.get_flag_of(F_HALF_CARRY) {
                    x = if neg {
                        x.wrapping_sub(0x06)
                    } else {
                        x.wrapping_add(0x06)
                    }
                }
                if x / 0x10 > 9 || self.get_flag_of(F_CARRY) {
                    x = if neg {
                        x.wrapping_sub(0x60)
                    } else {
                        x.wrapping_add(0x60)
                    }
                };
                self.write_acc(x);
            }
            AnaR(r) => {
                let a = self.alu.op(AC::Ana, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            AnaM => {
                let a = self.alu.op(AC::Ana, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Ani(n) => {
                let a = self.alu.op(AC::Ana, self.load_acc(), n);
                self.write_acc(a);
            }
            XraR(r) => {
                let a = self.alu.op(AC::Xra, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            XraM => {
                let a = self.alu.op(AC::Xra, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Xri(n) => {
                let a = self.alu.op(AC::Xra, self.load_acc(), n);
                self.write_acc(a);
            }
            OraR(r) => {
                let a = self.alu.op(AC::Ora, self.load_acc(), self.load_by_code(r));
                self.write_acc(a);
            }
            OraM => {
                let a = self.alu.op(AC::Ora, self.load_acc(), self.load_mem_hl());
                self.write_acc(a);
            }
            Ori(n) => {
                let a = self.alu.op(AC::Ora, self.load_acc(), n);
                self.write_acc(a);
            }
            CmpR(r) => {
                self.alu.op(AC::Sub, self.load_acc(), self.load_by_code(r));
            }
            CmpM => {
                self.alu.op(AC::Sub, self.load_acc(), self.load_mem_hl());
            }
            Cpi(n) => {
                self.alu.op(AC::Sub, self.load_acc(), n);
            }
            Rlc => {
                let a = self.alu.op(AC::Rlc, self.load_acc());
                self.write_acc(a)
            }
            Rrc => {
                let a = self.alu.op(AC::Rrc, self.load_acc());
                self.write_acc(a)
            }
            Ral => {
                let a = self.alu.op(AC::Rrc, self.load_acc());
                self.write_acc(a)
            }
            Rar => {
                let a = self.alu.op(AC::Rar, self.load_acc());
                self.write_acc(a)
            }
            Cma => self.write_acc(!self.load_acc()),
            Cmc => self.alu.cmc(),
            Stc => self.alu.stc(),
            Jmp(addr) => self.jump(addr),
            Jc(addr) => self.jump_on(Carry, true, addr),
            Jnc(addr) => self.jump_on(Carry, false, addr),
            Jz(addr) => self.jump_on(Zero, true, addr),
            Jnz(addr) => self.jump_on(Zero, false, addr),
            Jp(addr) => self.jump_on(Sign, false, addr),
            Jm(addr) => self.jump_on(Sign, true, addr),
            Jpe(addr) => self.jump_on(Parity, true, addr),
            Jpo(addr) => self.jump_on(Parity, false, addr),
            Call(addr) => self.call(addr),
            Cc(addr) => self.call_on(Carry, true, addr),
            Cnc(addr) => self.call_on(Carry, false, addr),
            Cz(addr) => self.call_on(Zero, true, addr),
            Cnz(addr) => self.call_on(Zero, false, addr),
            Cp(addr) => self.call_on(Sign, false, addr),
            Cm(addr) => self.call_on(Sign, true, addr),
            Cpe(addr) => self.call_on(Parity, true, addr),
            Cpo(addr) => self.call_on(Parity, false, addr),
            Ret => self.ret(),
            Rc => self.ret_on(Carry, true),
            Rnc => self.ret_on(Carry, false),
            Rz => self.ret_on(Zero, true),
            Rnz => self.ret_on(Zero, false),
            Rp => self.ret_on(Sign, false),
            Rm => self.ret_on(Sign, true),
            Rpe => self.ret_on(Parity, true),
            Rpo => self.ret_on(Parity, false),
            PcHL => self.pc.write(self.registers.hl.load()),
            PushRP(rp) => {
                let sp = self.registers.sp.load().wrapping_sub(2);
                self.write16_mem_at(sp, self.load16_by_code(rp));
                self.registers.sp.write(sp);
            }
            PushPSW => {
                let sp = self.registers.sp.load().wrapping_sub(2);
                self.write_mem_at(sp.wrapping_add(1), self.load_acc());
                self.write_mem_at(sp, self.alu.load_flag() | 0b10);
                self.registers.sp.write(sp);
            }
            PopRP(rp) => {
                let sp = self.registers.sp.load();
                self.write16_by_code(rp, self.load16_mem_at(sp));
                self.registers.sp.write(sp.wrapping_add(2));
            }
            PopPSW => {
                let sp = self.registers.sp.load();
                self.alu.write_flag(self.load_mem_at(sp) | !0b10);
                self.write_acc(self.load_mem_at(sp.wrapping_add(1)));
                self.registers.sp.write(sp.wrapping_add(2));
            }
            XtHL => {
                let sp = self.registers.sp.load();
                self.registers.hl.write(self.load16_mem_at(sp));
            }
            SpHL => self.registers.sp.write(self.registers.hl.load()),
            In(port) => {
                let mut buf = String::new();
                std::io::stdin().read_line(&mut buf).expect("IO error");
                let a = buf.parse().unwrap_or_default();
                self.write_acc(a)
            }
            Out(port) => {
                println!("{}", self.load_acc());
            }
            EI => self.interruptable = true,
            DI => self.interruptable = false,
            Rst(addr) => {
                let sp = self.registers.sp.load().wrapping_sub(2);
                self.write16_mem_at(sp, self.pc.load());
                self.registers.sp.write(sp);
                self.pc.write(addr * 8);
            }
            Hlt => self.halted = true,
            Nop => {}
        }
    }
}

#[cfg(test)]
mod test {
    use crate::instruction::InstructionDecoder;
    use crate::n88cpu::cpu::CPU88;
    use crate::n88cpu::instruction::N88InstructionSet;
    use crate::register::Register;

    #[test]
    fn cpu_test() {
        let mut cpu = CPU88::default();
        // calculate sum of 1..=80 (that equals 3240 = 81 * 80 / 2)
        let codes = &[
            0x3e, 0x50, //       mov a,=80
            0x47, // loop: mov b,a
            0x09, //       dad b
            0x3d, //       dcr a
            0xc2, 0x02, 0x00, //       jnz loop
            0x76, //       halt
        ];
        cpu.flush(codes);
        println!("{:?}", cpu);
        cpu.run();
        println!("{:?}", cpu);
        assert_eq!(cpu.registers.hl.load(), (1..=0x50).sum());
    }
}
