use crate::Name;
use interner::{Interner, Key};
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

pub struct Project {
    modules: HashMap<Key, PathBuf>,
}

impl Project {
    pub fn new(root: &Path, interner: &mut Interner) -> io::Result<Self> {
        let mut modules = HashMap::new();

        for entry in root.read_dir()? {
            let entry = entry?;
            let metadata = entry.metadata()?;

            if !metadata.is_file() {
                continue;
            }

            let file_name = entry.file_name();
            let file_name = match file_name.to_str() {
                Some(f) => f,
                None => continue,
            };

            let module_name = match file_name.rsplit_once('.') {
                Some((m, "gb")) => m,
                Some(_) | None => continue,
            };

            modules.insert(interner.intern(module_name), entry.path());
        }

        Ok(Self { modules })
    }

    pub fn is_module(&self, path: &Path) -> bool {
        self.modules.values().any(|p| p == path)
    }

    pub fn modules(&self) -> impl Iterator<Item = &Path> {
        self.modules.values().map(|p| p.as_path())
    }

    pub fn module_path(&self, name: Name) -> Option<&Path> {
        self.modules.get(&name.0).map(|p| p.as_path())
    }
}
