pub trait ALU {
    type Data;
    type Control;
    type Status;
    fn op(&self, code: Self::Control, a: Self::Data, b: Self::Data) -> (Self::Data, Self::Status);
}

#[cfg(test)]
mod test {
    use super::ALU;

    #[derive(Default, Debug, Copy, Clone)]
    struct Adder {}

    impl ALU for Adder {
        type Data = u8;
        /// if true, then sub.
        type Control = bool;
        /// Overflow
        type Status = bool;

        fn op(
            &self,
            sub: Self::Control,
            a: Self::Data,
            b: Self::Data,
        ) -> (Self::Data, Self::Status) {
            if sub {
                a.overflowing_sub(b)
            } else {
                a.overflowing_add(b)
            }
        }
    }

    #[test]
    fn test() {
        let adder = Adder::default();
        assert_eq!(adder.op(false, 20, 50), (70, false));
        assert_eq!(adder.op(false, 120, 50), (170, false));
        assert_eq!(adder.op(false, 220, 50), (14, true));
        assert_eq!(adder.op(true, 20, 50), (226, true));
        assert_eq!(adder.op(true, 120, 50), (70, false));
        assert_eq!(adder.op(true, 220, 50), (170, false));
    }
}
