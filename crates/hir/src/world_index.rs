use crate::{Function, Index, Name, RangeInfo, Ty};
use interner::Key;
use rustc_hash::FxHashMap;

#[derive(Default, Clone)]
pub struct WorldIndex(FxHashMap<Name, Index>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fqn {
    pub module: Name,
    pub function: Name,
}

impl WorldIndex {
    pub fn get_function(&self, fqn: Fqn) -> Result<&Function, GetFunctionError> {
        match self.0.get(&fqn.module) {
            Some(index) => match index.get_function(fqn.function) {
                Some(function) => Ok(function),
                None => Err(GetFunctionError::UnknownFunction),
            },
            None => Err(GetFunctionError::UnknownModule),
        }
    }

    pub fn get_ty(&self, name: Name) -> Option<Ty> {
        if name.0 == Key::s32() {
            Some(Ty::S32)
        } else if name.0 == Key::string() {
            Some(Ty::String)
        } else {
            None
        }
    }

    pub fn range_info(&self, fqn: Fqn) -> RangeInfo {
        self.0[&fqn.module].range_info[&fqn.function]
    }

    pub fn add_module(&mut self, module: Name, index: Index) {
        assert!(self.0.insert(module, index).is_none());
    }

    pub fn update_module(&mut self, module: Name, index: Index) {
        *self.0.get_mut(&module).unwrap() = index;
    }

    pub fn iter(&self) -> impl Iterator<Item = (Fqn, RangeInfo)> + '_ {
        self.0.iter().flat_map(|(module, index)| {
            index.iter().map(|(function, range)| (Fqn { module: *module, function }, range))
        })
    }
}

#[derive(Debug)]
pub enum GetFunctionError {
    UnknownModule,
    UnknownFunction,
}
