use crate::{Definition, Fqn, Index, Name, RangeInfo, Ty};
use interner::Key;
use rustc_hash::FxHashMap;

#[derive(Default)]
pub struct WorldIndex(FxHashMap<Name, Index>);

impl WorldIndex {
    pub fn get_definition(&self, fqn: Fqn) -> Result<&Definition, GetDefinitionError> {
        match self.0.get(&fqn.module) {
            Some(index) => match index.get_definition(fqn.name) {
                Some(def) => Ok(def),
                None => Err(GetDefinitionError::UnknownDefinition),
            },
            None => Err(GetDefinitionError::UnknownModule),
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
        self.0[&fqn.module].range_info[&fqn.name]
    }

    pub fn add_module(&mut self, module: Name, index: Index) {
        assert!(self.0.insert(module, index).is_none());
    }

    pub fn update_module(&mut self, module: Name, index: Index) {
        *self.0.get_mut(&module).unwrap() = index;
    }

    pub fn iter(&self) -> impl Iterator<Item = (Fqn, RangeInfo)> + '_ {
        self.0.iter().flat_map(|(module, index)| {
            index.iter().map(|(name, range)| (Fqn { module: *module, name }, range))
        })
    }
}

#[derive(Debug)]
pub enum GetDefinitionError {
    UnknownModule,
    UnknownDefinition,
}
