use crate::{Function, Index, Name, Ty};
use std::collections::HashMap;

#[derive(Default)]
pub struct WorldIndex(HashMap<Name, Index>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fqn {
    pub module: Name,
    pub function: Name,
}

impl WorldIndex {
    pub fn get_function(&self, fqn: &Fqn) -> Result<&Function, GetFunctionError> {
        match self.0.get(&fqn.module) {
            Some(index) => match index.get_function(&fqn.function) {
                Some(function) => Ok(function),
                None => Err(GetFunctionError::UnknownFunction),
            },
            None => Err(GetFunctionError::UnknownModule),
        }
    }

    pub fn get_ty(&self, name: &Name) -> Option<Ty> {
        match name.0.as_str() {
            "s32" => Some(Ty::S32),
            "string" => Some(Ty::String),
            _ => None,
        }
    }

    pub fn add_module(&mut self, module: Name, index: Index) {
        assert!(self.0.insert(module, index).is_none());
    }

    pub fn update_module(&mut self, module: &Name, index: Index) {
        *self.0.get_mut(module).unwrap() = index;
    }
}

#[derive(Debug)]
pub enum GetFunctionError {
    UnknownModule,
    UnknownFunction,
}
