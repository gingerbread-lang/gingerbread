use arena::{ArenaMap, Id};
use rustc_hash::FxHashMap;
use wasm_encoder::{
    CodeSection, DataSection, Export, ExportSection, Function, FunctionSection, Instruction,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

pub(crate) struct Ctx {
    type_section: TypeSection,
    function_section: FunctionSection,
    export_section: ExportSection,
    code_section: CodeSection,
    data_section: DataSection,
    instructions: Vec<Instruction<'static>>,
    function_idxs: FxHashMap<hir::Fqn, u32>,
    function_idx: u32,
    functions_to_compile: Vec<hir::Fqn>,
    local_idxs: ArenaMap<Id<hir::LocalDef>, u32>,
    local_idx: u32,
    local_tys: Vec<(u32, ValType)>,
    constant_idx: i32,
    bodies_map: FxHashMap<hir::Name, hir::Bodies>,
    tys_map: FxHashMap<hir::Name, hir_ty::InferenceResult>,
    world_index: hir::WorldIndex,
}

impl Ctx {
    pub(crate) fn new(
        bodies_map: FxHashMap<hir::Name, hir::Bodies>,
        tys_map: FxHashMap<hir::Name, hir_ty::InferenceResult>,
        world_index: hir::WorldIndex,
        entry_point: hir::Fqn,
    ) -> Self {
        let mut ctx = Self {
            type_section: TypeSection::new(),
            function_section: FunctionSection::new(),
            export_section: ExportSection::new(),
            code_section: CodeSection::new(),
            data_section: DataSection::new(),
            instructions: Vec::new(),
            function_idxs: FxHashMap::default(),
            function_idx: 0,
            functions_to_compile: vec![entry_point],
            local_idxs: ArenaMap::default(),
            local_idx: 0,
            local_tys: Vec::new(),
            constant_idx: 0,
            bodies_map,
            tys_map,
            world_index,
        };
        ctx.export_section.export("main", Export::Function(ctx.function_idx));
        ctx.export_section.export("memory", Export::Memory(0));

        ctx
    }

    pub(crate) fn finish(mut self) -> Vec<u8> {
        self.compile_queued_functions();

        let mut module = Module::new();
        module.section(&self.type_section);
        module.section(&self.function_section);
        module.section(MemorySection::new().memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
        }));
        module.section(&self.export_section);
        module.section(&self.code_section);
        module.section(&self.data_section);

        module.finish()
    }

    fn compile_function(&mut self, fqn: hir::Fqn) {
        let function = self.world_index.get_function(fqn).unwrap();

        let params: Vec<_> = function
            .params
            .iter()
            .filter_map(|param| match param.ty {
                hir::Ty::Unknown => unreachable!(),
                hir::Ty::S32 => Some(ValType::I32),
                hir::Ty::String => todo!(),
                hir::Ty::Unit => None,
            })
            .collect();

        let results = match function.return_ty {
            hir::Ty::Unknown => unreachable!(),
            hir::Ty::S32 => vec![ValType::I32],
            hir::Ty::String => vec![ValType::I32],
            hir::Ty::Unit => Vec::new(),
        };

        self.type_section.function(params, results);

        self.function_section.function(self.function_idx);

        self.compile_expr(fqn.module, self.bodies_map[&fqn.module].function_body(fqn.function));
        self.push(Instruction::End);

        let mut f = Function::new(self.local_tys.drain(..));
        for instruction in &self.instructions {
            f.instruction(instruction);
        }
        self.instructions.clear();
        self.code_section.function(&f);

        self.compile_queued_functions();
    }

    fn compile_queued_functions(&mut self) {
        while let Some(name) = self.functions_to_compile.pop() {
            self.compile_function(name);
        }
    }

    fn compile_statement(&mut self, module: hir::Name, statement: Id<hir::Statement>) {
        match self.bodies_map[&module][statement] {
            hir::Statement::Expr(expr) => self.compile_expr(module, expr),
            hir::Statement::LocalDef(local_def) => {
                let idx = self.local_idx;
                self.local_idx += 1;

                self.local_idxs.insert(local_def, idx);

                let value = self.bodies_map[&module][local_def].value;
                self.compile_expr(module, value);
                self.push(Instruction::LocalSet(idx));

                let ty = match self.tys_map[&module][local_def] {
                    hir::Ty::Unknown => unreachable!(),
                    hir::Ty::S32 => ValType::I32,
                    hir::Ty::String => ValType::I32,
                    hir::Ty::Unit => return,
                };
                self.local_tys.push((1, ty));
            }
        }
    }

    fn compile_expr(&mut self, module: hir::Name, expr: Id<hir::Expr>) {
        match self.bodies_map[&module][expr].clone() {
            hir::Expr::Missing => unreachable!(),

            hir::Expr::IntLiteral(n) => {
                self.push(Instruction::I32Const(n as i32));
            }

            hir::Expr::StringLiteral(s) => {
                let instruction = Instruction::I32Const(self.constant_idx);

                let len = s.len() as i32;
                let mut bytes = len.to_le_bytes().to_vec();
                bytes.append(&mut s.into_bytes());

                self.constant_idx += bytes.len() as i32;

                self.data_section.active(0, &instruction, bytes);
                self.push(instruction);
            }

            hir::Expr::Binary { lhs, rhs, operator } => {
                self.compile_expr(module, lhs);
                self.compile_expr(module, rhs);

                match operator {
                    hir::BinaryOperator::Add => self.push(Instruction::I32Add),
                    hir::BinaryOperator::Sub => self.push(Instruction::I32Sub),
                    hir::BinaryOperator::Mul => self.push(Instruction::I32Mul),
                    hir::BinaryOperator::Div => self.push(Instruction::I32DivU),
                };
            }

            hir::Expr::Block { statements, tail_expr } => {
                for statement in statements {
                    self.compile_statement(module, statement);
                }

                if let Some(tail_expr) = tail_expr {
                    self.compile_expr(module, tail_expr);
                }
            }

            hir::Expr::Local(local_def) => {
                self.push(Instruction::LocalGet(self.local_idxs[local_def]));
            }

            hir::Expr::Param { idx } => {
                self.push(Instruction::LocalGet(idx));
            }

            hir::Expr::Call { path, args } => {
                let fqn = match path {
                    hir::Path::ThisModule(function) => hir::Fqn { module, function },
                    hir::Path::OtherModule(fqn) => fqn,
                };

                self.functions_to_compile.push(fqn);
                self.function_idxs.insert(fqn, self.function_idx);
                self.function_idx += 1;

                for arg in args {
                    self.compile_expr(module, arg);
                }

                self.push(Instruction::Call(self.function_idx));
            }
        }
    }

    fn push(&mut self, instruction: Instruction<'static>) {
        self.instructions.push(instruction);
    }
}
