use crate::{Function, Index, Name, Ty};
use std::collections::HashMap;

#[derive(Default)]
pub struct WorldIndex(HashMap<Name, Index>);

impl WorldIndex {
    pub fn get_function(
        &self,
        module: &Name,
        function: &Name,
    ) -> Result<&Function, GetFunctionError> {
        match self.0.get(module) {
            Some(index) => match index.get_function(function) {
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
