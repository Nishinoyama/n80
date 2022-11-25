use crate::instruction::InstructionSet;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum Bit8RegisterCode {
    A = 7,
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
}

impl From<u8> for Bit8RegisterCode {
    fn from(r: u8) -> Self {
        assert!(r != 6 && r < 8);
        unsafe { std::mem::transmute(r) }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum Bit16RegisterCode {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

impl From<u8> for Bit16RegisterCode {
    fn from(r: u8) -> Self {
        assert!(r < 4);
        unsafe { std::mem::transmute(r) }
    }
}

#[derive(Eq, PartialEq, Debug)]
/// All Data and Addr are Little Endian
pub enum N88InstructionSet {
    /// Move Register 01DDDSSS
    MovRR(Bit8RegisterCode, Bit8RegisterCode),
    /// Move from memory. 01DDD110
    MovRM(Bit8RegisterCode),
    /// Move to memory. 01110SSS
    MovMR(Bit8RegisterCode),
    /// Move immediate. 00DDD110 Data
    MviR(Bit8RegisterCode, u8),
    /// Move to memory immediate. 00110110 Data
    MviM(u8),
    ///  Move to register pair. 00RP0001 Data
    LxiR(Bit16RegisterCode, u16),
    /// Load acc direct. 00111010 Addr
    Lda(u16),
    /// Store acc direct. 00110010 Addr
    Sta(u16),
    /// Load HL direct. 00101010 Addr
    Lhld(u16),
    /// Store HL direct. 00100010 Addr
    Shld(u16),
    /// Load acc indirect. 00RP1010
    Ldax(Bit16RegisterCode),
    /// Store acc indirect. 00RP0010
    Stax(Bit16RegisterCode),
    /// Exchange H and L with D and E
    Xchg,
    /// Add register, A <- A + r. 10000SSS
    AddR(Bit8RegisterCode),
    /// Add memory, A <- A + (HL). 10000110
    AddM,
    /// Add immediate A <- A + d. 11000110 Data
    Adi(u8),
    /// Add register with carry. 10001SSS
    AdcR(Bit8RegisterCode),
    /// Add memory with carry. 10001110
    AdcM,
    /// Add immediate with carry. 11001110 Data
    Aci(u8),
    /// Subtract register, A <- A - r. 10010SSS
    SubR(Bit8RegisterCode),
    /// Subtract memory, A <- A - (HL). 10010110
    SubM,
    /// Subtract immediate, A <- A - d. 11010110
    Sui(u8),
    SbbR(Bit8RegisterCode),
    SbbM,
    Sbi(u8),
    /// Increment register. 00DDD100
    InrR(Bit8RegisterCode),
    InrM,
    DcrR(Bit8RegisterCode),
    DcrM,
    Inx(Bit16RegisterCode),
    Dcx(Bit16RegisterCode),
    /// Add register pair to HL. 00RP1001
    Dad(Bit16RegisterCode),
    /// Decimal adjust acc. 00100111
    Daa,
    /// AND register. 10100SSS
    AnaR(Bit8RegisterCode),
    /// AND memory. 10100110
    AnaM,
    /// AND immediate. 11100110 Data
    Ani(u8),
    /// XOR register. 10101SSS
    XraR(Bit8RegisterCode),
    XraM,
    Xri(u8),
    /// OR register. 10110SSS
    OraR(Bit8RegisterCode),
    OraM,
    Ori(u8),
    /// Compare register, set flags of A - r. 10111SSS
    CmpR(Bit8RegisterCode),
    CmpM,
    Cpi(u8),
    /// Rotate left. 00000111
    Rlc,
    /// Rotate right. 00001111
    Rrc,
    /// Rotate left through carry. 00010111
    Ral,
    /// Rotate right through carry. 00011111
    Rar,
    /// Complement acc. 00101111
    Cma,
    /// Complement carry. 00111111
    Cmc,
    /// Set carry. 00110111
    Stc,
    /// Jump unconditional. 11000011 Addr
    /// Conditional jump that is 11XXX010 Addr is not this code.
    Jmp(u16),
    /// Jump on carry. 11011010
    Jc(u16),
    /// Jump on no carry. 11010010
    Jnc(u16),
    Jz(u16),
    Jnz(u16),
    Jp(u16),
    Jm(u16),
    Jpe(u16),
    Jpo(u16),
    /// Call unconditional. 11000101 Addr
    /// (SP - 1) <- PCH
    /// (SP - 2) <- PCL
    /// SP <- SP - 2
    /// PC <- addr
    /// Conditional call that is 11XXX100 Addr is not this code
    Call(u16),
    /// Call on carry. 11011010 Addr
    Cc(u16),
    Cnc(u16),
    Cz(u16),
    Cnz(u16),
    Cp(u16),
    Cm(u16),
    Cpe(u16),
    Cpo(u16),
    /// Return. 11001001
    /// PCL <- (SP)
    /// PCH <- (SP + 1)
    /// SP <- SP + 2
    /// Conditional return that is 11XXX000 is not this code
    Ret,
    /// Return on carry. 11011000
    Rc,
    Rnc,
    Rz,
    Rnz,
    Rp,
    Rm,
    Rpe,
    Rpo,
    /// Restart. 11NNN111
    /// Equals to Call 00nnn000 00000000.
    /// (SP - 1) <- PCH
    /// (SP - 2) <- PCL
    /// SP <- SP - 2
    /// PC <- 8 * nnn
    Rst(u16),
    /// Jump indirect. 11101001
    /// PC <- HL
    PcHL,
    /// Push. 11RP0101
    /// RP = 11 which is SP is not allowed.
    PushRP(Bit16RegisterCode),
    /// Push processor status word. 11110101
    ///
    /// (SP-1) <- A
    /// (SP-2) <- F as bit 1 is set
    /// SP <- SP - 2
    PushPSW,
    /// Pop. 11RP0001
    /// RP = 11 which is SP is not allowed.
    PopRP(Bit16RegisterCode),
    /// Push processor status word. 11110001
    PopPSW,
    /// Exchange stack top with HL. 11100011
    XtHL,
    /// Move HL to SP. 11111001
    SpHL,
    /// Input. 11011011 port
    In(u8),
    /// Input. 11010011 port
    Out(u8),
    /// Enable interrupts. 11111011
    EI,
    /// Disable interrupts. 11110011
    DI,
    /// Halt. 01110110
    Hlt,
    /// No op. 00000000
    Nop,
}

impl Default for N88InstructionSet {
    fn default() -> Self {
        Self::Nop
    }
}

impl InstructionSet for N88InstructionSet {
    type Code = u8;

    fn decode(codes: &[u8]) -> Option<Self> {
        use N88InstructionSet::*;
        if codes.is_empty() {
            None
        } else {
            let c = codes[0] >> 6;
            let r1 = codes[0] >> 3 & 0b111;
            let r2 = codes[0] & 0b111;
            let d1 = codes.get(1).copied();
            let d2 = codes.get(2).copied();
            let dd = d1.and_then(|d1| d2.map(|d2| u16::from_ne_bytes([d1, d2])));
            match c {
                0b00 => match (r1, r2) {
                    (0b000, 0b000) => Some(Nop),
                    (rp, 0b001) => dd.map(|dd| LxiR((rp >> 1).into(), dd)),
                    (0b000, 0b010) => Some(Stax(Bit16RegisterCode::BC)),
                    (0b001, 0b010) => Some(Ldax(Bit16RegisterCode::BC)),
                    (0b010, 0b010) => Some(Stax(Bit16RegisterCode::DE)),
                    (0b011, 0b010) => Some(Ldax(Bit16RegisterCode::DE)),
                    (0b100, 0b010) => dd.map(Shld),
                    (0b101, 0b010) => dd.map(Lhld),
                    (0b110, 0b010) => dd.map(Sta),
                    (0b111, 0b010) => dd.map(Lda),
                    (d, 0b011) => Some(match d {
                        d if d % 2 == 0 => Inx((d >> 1).into()),
                        d => Dcx((d >> 1).into()),
                    }),
                    (0b110, 0b100) => Some(InrM),
                    (d, 0b100) => Some(InrR(d.into())),
                    (0b110, 0b101) => Some(DcrM),
                    (d, 0b101) => Some(DcrR(d.into())),
                    (0b110, 0b110) => d1.map(MviM),
                    (d, 0b110) => d1.map(|da| MviR(d.into(), da)),
                    (0b000, 0b111) => Some(Rlc),
                    (0b001, 0b111) => Some(Rrc),
                    (0b010, 0b111) => Some(Ral),
                    (0b011, 0b111) => Some(Rar),
                    (0b101, 0b111) => Some(Cma),
                    (0b110, 0b111) => Some(Stc),
                    (0b111, 0b111) => Some(Cmc),
                    (0b100, 0b111) => Some(Daa),
                    _ => None,
                },
                0b01 => match (r1, r2) {
                    (0b110, 0b110) => Some(Hlt),
                    (0b110, r) => Some(MovMR(r.into())),
                    (r, 0b110) => Some(MovRM(r.into())),
                    (d, s) => Some(MovRR(d.into(), s.into())),
                },
                0b10 => match (r1, r2) {
                    (0b000, 0b110) => Some(AddM),
                    (0b000, r) => Some(AddR(r.into())),
                    (0b001, 0b110) => Some(AdcM),
                    (0b001, r) => Some(AdcR(r.into())),
                    (0b010, 0b110) => Some(SubM),
                    (0b010, r) => Some(SubR(r.into())),
                    (0b011, 0b110) => Some(SbbM),
                    (0b011, r) => Some(SbbR(r.into())),
                    (0b100, 0b110) => Some(AnaM),
                    (0b100, r) => Some(AnaR(r.into())),
                    (0b101, 0b110) => Some(XraM),
                    (0b101, r) => Some(XraR(r.into())),
                    (0b110, 0b110) => Some(OraM),
                    (0b110, r) => Some(OraR(r.into())),
                    (0b111, 0b110) => Some(CmpM),
                    (0b111, r) => Some(CmpR(r.into())),
                    _ => unreachable!(),
                },
                0b11 => match (r1, r2) {
                    (cond, 0b000) => Some(match cond {
                        0b000 => Rnz,
                        0b001 => Rz,
                        0b010 => Rnc,
                        0b011 => Rc,
                        0b100 => Rpo,
                        0b101 => Rpe,
                        0b110 => Rp,
                        0b111 => Rm,
                        _ => unreachable!(),
                    }),
                    (0b001, 0b001) => Some(Ret),
                    (0b101, 0b001) => Some(PcHL),
                    (0b111, 0b001) => Some(SpHL),
                    (0b110, 0b001) => Some(PopPSW),
                    (pr, 0b001) => Some(PopRP((pr >> 1).into())),
                    (cond, 0b010) => dd.map(match cond {
                        0b000 => Jnz,
                        0b001 => Jz,
                        0b010 => Jnc,
                        0b011 => Jc,
                        0b100 => Jpo,
                        0b101 => Jpe,
                        0b110 => Jp,
                        0b111 => Jm,
                        _ => unreachable!(),
                    }),
                    (0b000, 0b011) => dd.map(Jmp),
                    (0b010, 0b011) => d1.map(Out),
                    (0b011, 0b011) => d1.map(In),
                    (0b111, 0b011) => Some(EI),
                    (0b110, 0b011) => Some(DI),
                    (0b100, 0b011) => Some(XtHL),
                    (cond, 0b100) => dd.map(match cond {
                        0b000 => Cnz,
                        0b001 => Cz,
                        0b010 => Cnc,
                        0b011 => Cc,
                        0b100 => Cpo,
                        0b101 => Cpe,
                        0b110 => Cp,
                        0b111 => Cm,
                        _ => unreachable!(),
                    }),
                    (0b001, 0b101) => dd.map(Call),
                    (0b110, 0b101) => Some(PushPSW),
                    (pr, 0b101) => Some(PushRP((pr >> 1).into())),
                    (0b000, 0b110) => d1.map(Adi),
                    (0b001, 0b110) => d1.map(Aci),
                    (0b010, 0b110) => d1.map(Sui),
                    (0b011, 0b110) => d1.map(Sbi),
                    (0b100, 0b110) => d1.map(Ani),
                    (0b101, 0b110) => d1.map(Xri),
                    (0b110, 0b110) => d1.map(Ori),
                    (0b111, 0b110) => d1.map(Cpi),
                    (n, 0b111) => Some(Rst(n as u16)),
                    _ => None,
                },
                _ => unreachable!(),
            }
        }
    }
    fn encode(self) -> Vec<u8> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::instruction::InstructionDecoder;
    use crate::n88cpu::instruction::{Bit8RegisterCode, N88InstructionSet};

    #[test]
    fn from() {
        use Bit8RegisterCode::*;
        let t = (0..8)
            .filter(|t| !6.eq(t))
            .map(Bit8RegisterCode::from)
            .collect::<Vec<_>>();
        let s = vec![B, C, D, E, H, L, A];
        assert_eq!(t, s);
    }

    #[test]
    fn instruction_universe() {
        (0..=255).for_each(|inst| {
            let mut d = InstructionDecoder::<N88InstructionSet>::default();
            d.push(inst);
            let res = d.decode();
            if res.is_none() {
                d.push(63)
            };
            let res = d.decode();
            if res.is_none() {
                d.push(71)
            };
            let res = d.decode();
        });
    }
}
