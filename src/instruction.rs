use std::marker::PhantomData;

#[derive(Debug, Default)]
pub struct InstructionDecoder<B, S> {
    instruction_set: PhantomData<S>,
    buf: Vec<B>,
}

impl<B, S: InstructionSet<B>> InstructionDecoder<B, S> {
    pub fn new() -> Self {
        Self {
            instruction_set: Default::default(),
            buf: vec![],
        }
    }
    pub fn decode(&mut self, code: B) {
        self.buf.push(code)
    }
    pub fn inst(&self) -> Option<S> {
        S::decode(&self.buf)
    }
}

pub trait InstructionSet<B>: Sized {
    fn decode(codes: &[B]) -> Option<Self>;
    fn encode(self) -> Vec<B>;
}
