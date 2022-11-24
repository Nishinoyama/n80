use std::marker::PhantomData;

#[derive(Debug, Default)]
pub struct InstructionDecoder<S: InstructionSet> {
    instruction_set: PhantomData<S>,
    buf: Vec<S::Code>,
}

impl<S: InstructionSet> InstructionDecoder<S> {
    pub fn new() -> Self {
        Self {
            instruction_set: Default::default(),
            buf: vec![],
        }
    }
    pub fn decode(&mut self, code: S::Code) {
        self.buf.push(code)
    }
    pub fn inst(&self) -> Option<S> {
        S::decode(&self.buf)
    }
}

pub trait InstructionSet: Sized {
    type Code;
    fn decode(codes: &[Self::Code]) -> Option<Self>;
    fn encode(self) -> Vec<Self::Code>;
}
