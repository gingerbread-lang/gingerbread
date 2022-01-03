use indexmap::IndexMap;
use std::fmt;

#[derive(Default)]
pub struct Summary {
    pub fncs: IndexMap<Name, Fnc>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub String);

pub struct Fnc {
    pub params: Vec<Param>,
    pub ret_ty: Ty,
}

pub struct Param {
    pub name: Option<Name>,
    pub ty: Ty,
}

#[derive(PartialEq)]
pub enum Ty {
    Name(Name),
    Unit,
    Missing,
}

impl fmt::Debug for Summary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, fnc) in &self.fncs {
            write!(f, "fnc {}", name.0)?;

            write!(f, "(")?;

            for (idx, param) in fnc.params.iter().enumerate() {
                if idx != 0 {
                    write!(f, ", ")?;
                }

                match &param.name {
                    Some(Name(name)) => write!(f, "{}", name)?,
                    None => write!(f, "?")?,
                }

                write!(f, ": ")?;
                format_ty(&param.ty, f)?;
            }

            write!(f, ")")?;

            if fnc.ret_ty != Ty::Unit {
                write!(f, ": ")?;
                format_ty(&fnc.ret_ty, f)?;
            }

            writeln!(f, ";")?;
        }

        Ok(())
    }
}

fn format_ty(ty: &Ty, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match ty {
        Ty::Name(Name(name)) => write!(f, "{}", name),
        Ty::Unit => write!(f, "unit"),
        Ty::Missing => write!(f, "?"),
    }
}
