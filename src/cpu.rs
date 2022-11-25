use crate::instruction::{InstructionDecoder, InstructionSet};
use crate::register::Register;

pub trait CPU<D, A, I: InstructionSet, R: Register<I::Code>> {
    fn fetch(&mut self);
}
