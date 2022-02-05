use crate::{Function, Index, Name};
use std::collections::HashMap;

#[derive(Default)]
pub struct WorldIndex(HashMap<Name, Index>);

impl WorldIndex {
    pub fn get_function(&self, module: &Name, function: &Name) -> GetFunctionResult<'_> {
        match self.0.get(module) {
            Some(index) => match index.get_function(function) {
                Some(function) => GetFunctionResult::Found(function),
                None => GetFunctionResult::UnknownFunction,
            },
            None => GetFunctionResult::UnknownModule,
        }
    }

    pub fn add_module(&mut self, module: Name, index: Index) {
        assert!(self.0.insert(module, index).is_none());
    }

    pub fn update_module(&mut self, module: &Name, index: Index) {
        *self.0.get_mut(module).unwrap() = index;
    }
}

pub enum GetFunctionResult<'a> {
    Found(&'a Function),
    UnknownModule,
    UnknownFunction,
}
