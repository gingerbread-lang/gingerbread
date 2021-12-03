mod instruction_set;
use instruction_set::{Instruction, Loc, Reg, Val};

use std::convert::TryInto;
use std::ops::{Index, IndexMut};

pub fn run(bytecode: &[u8]) -> bincode::Result<()> {
    let instructions = bincode::deserialize(bytecode)?;
    Vm::from_instructions(instructions).run();

    Ok(())
}

const STACK_PTR: Reg = Reg(0);

type VmWord = u32;
const VM_WORD_SIZE: VmWord = std::mem::size_of::<VmWord>() as VmWord;
const VM_WORD_SIZE_USIZE: usize = std::mem::size_of::<VmWord>();

#[derive(Debug)]
struct Vm {
    instructions: Vec<Instruction>,
    instruction_ptr: usize,
    stack: Stack,
    is_stack_empty: bool,
    registers: Registers,
}

impl Vm {
    fn from_instructions(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            instruction_ptr: 0,
            stack: Stack::default(),
            is_stack_empty: true,
            registers: Registers::default(),
        }
    }

    fn run(&mut self) {
        while self.instruction_ptr < self.instructions.len() {
            let instruction = self.instructions[self.instruction_ptr];

            match instruction {
                Instruction::Exit => return,

                Instruction::Push(value) => {
                    let val = self.eval(value);

                    let stack_ptr = &mut self.registers[STACK_PTR];
                    if self.is_stack_empty {
                        self.is_stack_empty = false;
                    } else {
                        *stack_ptr += VM_WORD_SIZE;
                    }
                    let stack_ptr = *stack_ptr;

                    if stack_ptr == self.stack.len() {
                        self.stack.grow_one();
                    }

                    self.stack.store(stack_ptr, val);
                }

                Instruction::Pop(dst) => {
                    let popped_val = self.stack.read(self.registers[STACK_PTR]);

                    self.registers[dst] = popped_val;

                    assert!(!self.is_stack_empty, "tried to pop with empty stack");

                    if self.registers[STACK_PTR] == 0 {
                        self.is_stack_empty = true;
                    } else {
                        self.registers[STACK_PTR] -= VM_WORD_SIZE;
                    }
                }

                Instruction::Store(dst, val) => {
                    let val = self.eval(val);
                    match dst {
                        Loc::Ptr(reg) => self.stack.store(self.registers[reg], val),
                        Loc::Reg(reg) => self.registers[reg] = val,
                    }
                }

                Instruction::Add(dst, val) => self.registers[dst] += self.eval(val),
                Instruction::Sub(dst, val) => self.registers[dst] -= self.eval(val),
                Instruction::Mul(dst, val) => self.registers[dst] *= self.eval(val),
                Instruction::Div(dst, val) => self.registers[dst] /= self.eval(val),
            }

            self.instruction_ptr += 1;
        }
    }

    fn eval(&mut self, val: Val) -> VmWord {
        match val {
            Val::Imm(val) => val,
            Val::Reg(reg) => self.registers[reg],
            Val::Ptr(reg) => self.stack.read(self.registers[reg]),
        }
    }
}

#[derive(Debug, Default)]
struct Registers([VmWord; 32]);

impl Index<Reg> for Registers {
    type Output = VmWord;

    fn index(&self, idx: Reg) -> &Self::Output {
        &self.0[usize::from(idx.0)]
    }
}

impl IndexMut<Reg> for Registers {
    fn index_mut(&mut self, idx: Reg) -> &mut Self::Output {
        &mut self.0[usize::from(idx.0)]
    }
}

#[derive(Debug, PartialEq, Default)]
struct Stack {
    bytes: Vec<u8>,
}

impl Stack {
    #[cfg(test)]
    fn new<const LEN: usize>(stack: [VmWord; LEN]) -> Self {
        let mut bytes = Vec::new();

        for x in stack {
            bytes.extend_from_slice(&x.to_ne_bytes());
        }

        assert_eq!(bytes.len(), stack.len() * VM_WORD_SIZE_USIZE);

        Self { bytes }
    }

    fn len(&self) -> VmWord {
        self.bytes.len() as VmWord
    }

    #[cfg(test)]
    fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    fn grow_one(&mut self) {
        self.bytes.extend_from_slice(&[0; VM_WORD_SIZE_USIZE]);
    }

    fn read(&self, ptr: VmWord) -> VmWord {
        assert!(
            ptr + VM_WORD_SIZE <= self.len(),
            "tried to read out of stack bounds: {} + {} > {}",
            ptr,
            VM_WORD_SIZE,
            self.len()
        );

        let bytes = self.bytes[ptr as usize..ptr as usize + VM_WORD_SIZE_USIZE].try_into().unwrap();
        VmWord::from_ne_bytes(bytes)
    }

    fn store(&mut self, ptr: VmWord, val: VmWord) {
        assert!(
            ptr + VM_WORD_SIZE <= self.len(),
            "tried to store out of stack bounds: {} + {} > {}",
            ptr,
            VM_WORD_SIZE,
            self.len()
        );

        for (idx, val) in val.to_ne_bytes().into_iter().enumerate() {
            self.bytes[ptr as usize + idx] = val;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn noop() {
        let mut vm = Vm::from_instructions(vec![Instruction::Exit]);
        vm.run();

        assert!(vm.stack.is_empty());
    }

    #[test]
    fn push_immediate() {
        let mut vm = Vm::from_instructions(vec![Instruction::Push(Val::Imm(92))]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([92]));
        assert_eq!(vm.registers[STACK_PTR], 0);
    }

    #[test]
    fn push_2_immediate() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(10)),
            Instruction::Push(Val::Imm(20)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([10, 20]));
        assert_eq!(vm.registers[STACK_PTR], VM_WORD_SIZE);
    }

    #[test]
    fn push_register() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(256)),
            Instruction::Push(Val::Reg(Reg(1))),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([256]));
        assert_eq!(vm.registers[STACK_PTR], 0);
        assert_eq!(vm.registers[Reg(1)], 256);
    }

    #[test]
    fn pop() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(100)),
            Instruction::Pop(Reg(10)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([100]));
        assert_eq!(vm.registers[STACK_PTR], 0);
        assert_eq!(vm.registers[Reg(10)], 100);
    }

    #[test]
    fn pop_2() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(3)),
            Instruction::Push(Val::Imm(4)),
            Instruction::Pop(Reg(1)),
            Instruction::Pop(Reg(2)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([3, 4]));
        assert_eq!(vm.registers[STACK_PTR], 0);
        assert_eq!(vm.registers[Reg(1)], 4);
        assert_eq!(vm.registers[Reg(2)], 3);
    }

    #[test]
    fn push_pop() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(10)),
            Instruction::Push(Val::Imm(9)),
            Instruction::Push(Val::Imm(7)),
            Instruction::Pop(Reg(1)),
            Instruction::Push(Val::Imm(8)),
            Instruction::Pop(Reg(2)),
            Instruction::Pop(Reg(3)),
            Instruction::Pop(Reg(4)),
            Instruction::Push(Val::Imm(92)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([92, 9, 8]));
        assert_eq!(vm.registers[STACK_PTR], 0);
        assert_eq!(vm.registers[Reg(1)], 7);
        assert_eq!(vm.registers[Reg(2)], 8);
        assert_eq!(vm.registers[Reg(3)], 9);
        assert_eq!(vm.registers[Reg(4)], 10);
    }

    #[test]
    fn store_immediate_in_register() {
        let mut vm =
            Vm::from_instructions(vec![Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(10))]);
        vm.run();

        assert_eq!(vm.registers[Reg(1)], 10);
    }

    #[test]
    fn store_register_in_register() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(3)), Val::Imm(92)),
            Instruction::Store(Loc::Reg(Reg(4)), Val::Reg(Reg(3))),
        ]);
        vm.run();

        assert_eq!(vm.registers[Reg(3)], 92);
        assert_eq!(vm.registers[Reg(4)], 92);
    }

    #[test]
    fn store_from_stack_in_register() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(100)),
            Instruction::Store(Loc::Reg(Reg(5)), Val::Ptr(STACK_PTR)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([100]));
        assert_eq!(vm.registers[Reg(5)], 100);
    }

    #[test]
    fn stack_ptr_is_incremented_after_each_push() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Push(Val::Imm(50)),
            Instruction::Store(Loc::Reg(Reg(1)), Val::Ptr(STACK_PTR)),
            Instruction::Push(Val::Imm(100)),
            Instruction::Store(Loc::Reg(Reg(2)), Val::Ptr(STACK_PTR)),
            Instruction::Push(Val::Imm(150)),
            Instruction::Store(Loc::Reg(Reg(3)), Val::Ptr(STACK_PTR)),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([50, 100, 150]));
        assert_eq!(vm.registers[STACK_PTR], 2 * VM_WORD_SIZE);
        assert_eq!(vm.registers[Reg(1)], 50);
        assert_eq!(vm.registers[Reg(2)], 100);
        assert_eq!(vm.registers[Reg(3)], 150);
    }

    #[test]
    #[should_panic(expected = "tried to read out of stack bounds: 0 + 4 > 0")]
    fn die_reading_out_of_stack_bounds() {
        Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(0)),
            Instruction::Store(Loc::Reg(Reg(2)), Val::Ptr(Reg(1))),
        ])
        .run();
    }

    #[test]
    #[should_panic(expected = "tried to store out of stack bounds: 0 + 4 > 0")]
    fn die_storing_out_of_stack_bounds() {
        Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(0)),
            Instruction::Store(Loc::Ptr(Reg(1)), Val::Imm(10)),
        ])
        .run();
    }

    #[test]
    #[should_panic(expected = "tried to read out of stack bounds: 8 + 4 > 8")]
    fn die_reading_out_of_stack_bounds_after_push() {
        Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(10)), Val::Imm(0)),
            Instruction::Store(Loc::Reg(Reg(11)), Val::Imm(VM_WORD_SIZE)),
            Instruction::Store(Loc::Reg(Reg(12)), Val::Imm(2 * VM_WORD_SIZE)),
            Instruction::Push(Val::Imm(100)),
            Instruction::Store(Loc::Reg(Reg(1)), Val::Ptr(Reg(10))),
            Instruction::Push(Val::Imm(200)),
            Instruction::Store(Loc::Reg(Reg(2)), Val::Ptr(Reg(11))),
            Instruction::Store(Loc::Reg(Reg(3)), Val::Ptr(Reg(12))),
        ])
        .run();
    }

    #[test]
    fn add_immediate() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(10)),
            Instruction::Add(Reg(1), Val::Imm(1)),
        ]);
        vm.run();

        assert_eq!(vm.registers[Reg(1)], 11);
    }

    #[test]
    fn add_register() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(90)),
            Instruction::Store(Loc::Reg(Reg(2)), Val::Imm(2)),
            Instruction::Add(Reg(1), Val::Reg(Reg(2))),
        ]);
        vm.run();

        assert_eq!(vm.registers[Reg(1)], 92);
        assert_eq!(vm.registers[Reg(2)], 2);
    }

    #[test]
    fn add_from_stack() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(1)), Val::Imm(50)),
            Instruction::Push(Val::Imm(50)),
            Instruction::Push(Val::Imm(100)),
            Instruction::Store(Loc::Reg(Reg(10)), Val::Reg(STACK_PTR)),
            Instruction::Push(Val::Imm(200)),
            Instruction::Add(Reg(1), Val::Ptr(Reg(10))),
        ]);
        vm.run();

        assert_eq!(vm.stack, Stack::new([50, 100, 200]));
        assert_eq!(vm.registers[STACK_PTR], 2 * VM_WORD_SIZE);
        assert_eq!(vm.registers[Reg(1)], 150);
        assert_eq!(vm.registers[Reg(10)], VM_WORD_SIZE);
    }

    #[test]
    fn other_arithmetic() {
        let mut vm = Vm::from_instructions(vec![
            Instruction::Store(Loc::Reg(Reg(7)), Val::Imm(1)), // 1
            Instruction::Add(Reg(7), Val::Imm(1)),             // 2
            Instruction::Mul(Reg(7), Val::Imm(2)),             // 4
            Instruction::Sub(Reg(7), Val::Imm(1)),             // 3
            Instruction::Mul(Reg(7), Val::Imm(3)),             // 9
            Instruction::Add(Reg(7), Val::Imm(1)),             // 10
            Instruction::Div(Reg(7), Val::Imm(5)),             // 2
        ]);
        vm.run();

        assert_eq!(vm.registers[Reg(7)], 2);
    }
}
