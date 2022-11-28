// pub mod cpu;
pub mod instruction;
pub mod memory;
pub mod n88cpu;
/// sample for 8-bit cpu which is hardcoded.
/// RISC
pub mod n8cpu;
pub mod register;
pub mod register_set;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
