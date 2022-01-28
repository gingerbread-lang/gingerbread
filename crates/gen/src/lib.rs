use nanorand::{Rng, WyRand};

const INDENT: &str = "    ";
const MAX_LEVEL: u8 = 8;

pub fn gen(min_len: usize) -> String {
    Gen { rng: WyRand::new(), buf: String::with_capacity(min_len), min_len, indentation_level: 0 }
        .finish()
}

struct Gen {
    rng: WyRand,
    buf: String,
    min_len: usize,
    indentation_level: u8,
}

impl Gen {
    fn finish(mut self) -> String {
        while !self.generated_enough() {
            self.gen_function();
        }

        self.buf
    }

    fn gen_function(&mut self) {
        self.buf.push_str("fnc ");
        self.gen_ident();
        self.buf.push('(');

        let num_params = self.rng.generate_range(0..=3);
        for i in 0..num_params {
            if i != 0 {
                self.buf.push_str(", ");
            }

            self.gen_ident();
            self.buf.push_str(": ");
            self.gen_ident();
        }

        self.buf.push(')');

        let has_return_ty = self.rng.generate();
        if has_return_ty {
            self.buf.push_str(": ");
            self.gen_ident()
        }

        self.buf.push_str(" -> ");
        self.gen_expr(0);

        self.buf.push(';');
        self.newline();
    }

    fn gen_statement(&mut self, level: u8) {
        match self.rng.generate_range(0..2) {
            0 => self.gen_local_def(level + 1),
            1 => {
                self.gen_expr(level + 1);
                self.buf.push(';');
                self.newline()
            }
            _ => unreachable!(),
        }
    }

    fn gen_local_def(&mut self, level: u8) {
        self.buf.push_str("let ");
        self.gen_ident();
        self.buf.push_str(" = ");
        self.gen_expr(level + 1);
        self.buf.push(';');
        self.newline();
    }

    fn gen_expr(&mut self, level: u8) {
        if level > MAX_LEVEL {
            match self.rng.generate_range(0..2) {
                0 => self.gen_int_literal(),
                1 => self.gen_string_literal(),
                _ => unreachable!(),
            }
            return;
        }

        match self.rng.generate_range(0..5) {
            0 => self.gen_int_literal(),
            1 => self.gen_string_literal(),
            2 => self.gen_bin_expr(level + 1),
            3 => self.gen_block_expr(level + 1),
            4 => self.gen_call(level + 1),
            _ => unreachable!(),
        }
    }

    fn gen_int_literal(&mut self) {
        let n = self.rng.generate_range(0..u16::MAX);
        self.buf.push_str(&n.to_string());
    }

    fn gen_string_literal(&mut self) {
        self.buf.push('"');

        let len = self.rng.generate_range(5..=10);
        for _ in 0..len {
            self.gen_ascii(b'#'..=b'~');
        }

        self.buf.push('"');
    }

    fn gen_bin_expr(&mut self, level: u8) {
        self.gen_expr(level + 1);

        let op = match self.rng.generate_range(0..4) {
            0 => '+',
            1 => '-',
            2 => '*',
            3 => '/',
            _ => unreachable!(),
        };

        self.buf.push(' ');
        self.buf.push(op);
        self.buf.push(' ');

        self.gen_expr(level + 1);
    }

    fn gen_block_expr(&mut self, level: u8) {
        let num_statements = self.rng.generate_range(0..=5);

        if num_statements == 0 {
            self.buf.push_str("{}");
            return;
        }

        self.indentation_level += 1;

        self.buf.push('{');
        self.newline();

        for _ in 0..num_statements {
            self.gen_statement(level + 1);
        }

        let has_tail = self.rng.generate();
        if has_tail {
            self.gen_expr(level + 1);
            self.newline();
        }

        self.eat_indent();
        self.buf.push('}');

        self.indentation_level -= 1;
    }

    fn gen_call(&mut self, level: u8) {
        self.gen_ident();

        let num_params = self.rng.generate_range(0..=3);
        for i in 0..num_params {
            if i != 0 {
                self.buf.push(',');
            }

            self.buf.push(' ');
            self.gen_expr(level + 1);
        }
    }

    fn gen_ident(&mut self) {
        let num_words = self.rng.generate_range(1..=2);
        for i in 0..num_words {
            if i != 0 {
                self.buf.push('_');
            }

            let word_len = self.rng.generate_range(2..=5);
            for _ in 0..word_len {
                self.gen_ascii(b'a'..=b'z');
            }
        }
    }

    fn gen_ascii(&mut self, range: std::ops::RangeInclusive<u8>) {
        let ascii = self.rng.generate_range(range);
        let ch = char::from_u32(ascii as u32).unwrap();
        self.buf.push(ch);
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
