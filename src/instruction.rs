#[derive(Debug)]
pub struct InstructionDecoder<S: InstructionSet> {
    buf: Vec<S::Code>,
}

impl<S: InstructionSet> InstructionDecoder<S> {
    pub fn new() -> Self {
        Self { buf: vec![] }
    }
    pub fn push(&mut self, code: S::Code) {
        self.buf.push(code)
    }
    pub fn decode(&self) -> Option<S> {
        S::decode(&self.buf)
    }
}

pub trait InstructionSet: Sized {
    type Code;
    fn decode(codes: &[Self::Code]) -> Option<Self>;
    fn encode(self) -> Vec<Self::Code>;
}
