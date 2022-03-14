use nanorand::{Rng, WyRand};
use std::collections::{HashMap, HashSet};
use std::ops::RangeInclusive;

const INDENT: &str = "    ";
const MAX_LEVEL: u8 = 8;

pub fn gen(min_len: usize) -> String {
    Gen {
        rng: WyRand::new(),
        buf: String::with_capacity(min_len),
        min_len,
        indentation_level: 0,
        scopes: Vec::new(),
        functions: HashMap::new(),
        used_idents: HashSet::new(),
    }
    .finish()
}

struct Gen {
    rng: WyRand,
    buf: String,
    min_len: usize,
    indentation_level: u8,
    scopes: Vec<HashMap<Ty, Vec<String>>>,
    functions: HashMap<Ty, Vec<(String, Vec<Ty>)>>,
    used_idents: HashSet<String>,
}

impl Gen {
    fn finish(mut self) -> String {
        while !self.generated_enough() {
            self.gen_function();
        }

        self.buf
    }

    fn gen_function(&mut self) {
        let mut new_scope: HashMap<Ty, Vec<String>> = HashMap::new();

        self.buf.push_str("fnc ");
        let name = self.gen_ident();

        let num_params = self.rng.generate_range(0..=3);
        let params = if num_params == 0 {
            Vec::new()
        } else {
            self.buf.push('(');

            let mut params = Vec::with_capacity(num_params);
            for i in 0..num_params {
                if i != 0 {
                    self.buf.push_str(", ");
                }

                let param_name = self.gen_ident();
                self.buf.push_str(": ");
                let param_ty = self.gen_ty();
                params.push(param_ty);

                new_scope.entry(param_ty).or_default().push(param_name);
            }

            self.buf.push(')');

            params
        };

        let return_ty = {
            let has_return_ty = self.rng.generate();
            if has_return_ty {
                self.buf.push_str(": ");
                self.gen_ty()
            } else {
                Ty::Unit
            }
        };

        self.buf.push_str(" -> ");

        self.scopes.push(new_scope);
        self.gen_expr(0, return_ty);
        self.scopes.pop();

        self.buf.push(';');
        self.newline();

        self.functions.entry(return_ty).or_default().push((name, params));
    }

    fn gen_statement(&mut self, level: u8) {
        match self.rng.generate_range(0..=1) {
            0 => self.gen_local_def(level + 1),
            1 => {
                let ty = self.choose_ty();
                self.gen_expr(level + 1, ty);
                self.buf.push(';');
                self.newline()
            }
            _ => unreachable!(),
        }
    }

    fn gen_local_def(&mut self, level: u8) {
        self.buf.push_str("let ");
        let name = self.gen_ident();
        let ty = self.choose_ty();
        self.buf.push_str(" = ");
        self.gen_expr(level + 1, ty);
        self.buf.push(';');
        self.newline();

        self.scopes.last_mut().unwrap().entry(ty).or_default().push(name);
    }

    fn gen_expr(&mut self, level: u8, ty: Ty) {
        if level > MAX_LEVEL {
            match ty {
                Ty::S32 => self.gen_int_literal(),
                Ty::String => self.gen_string_literal(),
                Ty::Unit => self.buf.push_str("{}"),
            }
            return;
        }

        let mut choose = || -> Result<(), ()> {
            match ty {
                Ty::S32 => match self.rng.generate_range(0_u8..=4) {
                    0 => self.gen_int_literal(),
                    1 => self.gen_binary_expr(level + 1),
                    2 => self.gen_block_expr(level + 1, ty),
                    3 => self.gen_call(level + 1, ty)?,
                    4 => self.gen_local_ref(ty)?,
                    _ => unreachable!(),
                },
                Ty::String => match self.rng.generate_range(0_u8..=3) {
                    0 => self.gen_string_literal(),
                    1 => self.gen_block_expr(level + 1, ty),
                    2 => self.gen_call(level + 1, ty)?,
                    3 => self.gen_local_ref(ty)?,
                    _ => unreachable!(),
                },
                Ty::Unit => match self.rng.generate_range(0_u8..=2) {
                    0 => self.gen_block_expr(level + 1, ty),
                    1 => self.gen_call(level + 1, ty)?,
                    2 => self.gen_local_ref(ty)?,
                    _ => unreachable!(),
                },
            }
            Ok(())
        };

        while choose().is_err() {}
    }

    fn gen_int_literal(&mut self) {
        let n = self.rng.generate_range(0..u16::MAX);
        self.buf.push_str(&n.to_string());
    }

    fn gen_string_literal(&mut self) {
        self.buf.push('"');

        let len = self.rng.generate_range(5..=10);
        for _ in 0..len {
            let range = b'#'..=b'~';
            let c = self.choose_ascii(range);
            self.buf.push(c);
        }

        self.buf.push('"');
    }

    fn gen_binary_expr(&mut self, level: u8) {
        self.gen_expr(level + 1, Ty::S32);

        let operator = match self.rng.generate_range(0_u8..=3) {
            0 => '+',
            1 => '-',
            2 => '*',
            3 => '/',
            _ => unreachable!(),
        };

        self.buf.push(' ');
        self.buf.push(operator);
        self.buf.push(' ');

        self.gen_expr(level + 1, Ty::S32);
    }

    fn gen_block_expr(&mut self, level: u8, ty: Ty) {
        let num_statements = self.rng.generate_range(0_u8..=5);

        if num_statements == 0 && ty == Ty::Unit {
            self.buf.push_str("{}");
            return;
        }

        self.scopes.push(HashMap::new());
        self.indentation_level += 1;

        self.buf.push('{');
        self.newline();

        for _ in 0..num_statements {
            self.gen_statement(level + 1);
        }

        let has_tail = ty != Ty::Unit;
        if has_tail {
            self.gen_expr(level + 1, ty);
            self.newline();
        }

        self.eat_indent();
        self.buf.push('}');

        self.indentation_level -= 1;
        self.scopes.pop();
    }

    fn gen_call(&mut self, level: u8, ty: Ty) -> Result<(), ()> {
        let functions_with_desired_ty = self.functions.get(&ty).ok_or(())?;
        let (name, param_tys) =
            &functions_with_desired_ty[self.rng.generate_range(0..functions_with_desired_ty.len())];

        self.buf.push_str(name);

        for (i, ty) in param_tys.clone().into_iter().enumerate() {
            if i != 0 {
                self.buf.push(',');
            }

            self.buf.push(' ');
            self.gen_expr(level + 1, ty);
        }

        Ok(())
    }

    fn gen_local_ref(&mut self, ty: Ty) -> Result<(), ()> {
        let mut locals_with_desired_ty = Vec::new();

        for scope in self.scopes.iter().rev() {
            if let Some(locals) = scope.get(&ty) {
                locals_with_desired_ty.extend_from_slice(locals);
            }
        }

        if locals_with_desired_ty.is_empty() {
            return Err(());
        }

        let idx = self.rng.generate_range(0..locals_with_desired_ty.len());
        let name = &locals_with_desired_ty[idx];
        self.buf.push_str(name);

        Ok(())
    }

    fn gen_ty(&mut self) -> Ty {
        match self.rng.generate_range(0..=1) {
            0 => {
                self.buf.push_str("s32");
                Ty::S32
            }
            1 => {
                self.buf.push_str("string");
                Ty::String
            }
            _ => unreachable!(),
        }
    }

    fn choose_ty(&mut self) -> Ty {
        match self.rng.generate_range(0..=4) {
            0 | 1 => Ty::S32,
            2 | 3 => Ty::String,
            4 => Ty::Unit,
            _ => unreachable!(),
        }
    }

    #[must_use]
    fn gen_ident(&mut self) -> String {
        let mut ident = String::new();

        loop {
            let num_words = self.rng.generate_range(1..=2);
            for i in 0..num_words {
                if i != 0 {
                    ident.push('_');
                }

                let word_len = self.rng.generate_range(2..=5);
                for _ in 0..word_len {
                    ident.push(self.choose_ascii(b'a'..=b'z'));
                }
            }

            if ident == "let" || ident == "fnc" {
                ident.clear();
                continue;
            }

            if self.used_idents.contains(&ident) {
                ident.clear();
                continue;
            }

            break;
        }

        self.buf.push_str(&ident);
        self.used_idents.insert(ident.clone());

        ident
    }

    fn choose_ascii(&mut self, range: RangeInclusive<u8>) -> char {
        let ascii = self.rng.generate_range(range);
        char::from_u32(ascii as u32).unwrap()
    }

    fn newline(&mut self) {
        self.buf.push('\n');
        for _ in 0..self.indentation_level {
            self.buf.push_str(INDENT);
        }
    }

    fn eat_indent(&mut self) {
        assert!(self.buf.ends_with(INDENT));
        self.buf.truncate(self.buf.len() - INDENT.len());
    }

    fn generated_enough(&self) -> bool {
        self.buf.len() >= self.min_len
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Ty {
    S32,
    String,
    Unit,
}
