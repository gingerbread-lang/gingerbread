use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};

pub struct Project {
    modules: HashSet<PathBuf>,
}

impl Project {
    pub fn new(root: &Path) -> io::Result<Self> {
        let mut modules = HashSet::new();

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

            match file_name.rsplit_once('.') {
                Some((_, "gb")) => {}
                Some(_) | None => continue,
            };

            modules.insert(entry.path());
        }

        Ok(Self { modules })
    }

    pub fn is_module(&self, path: &Path) -> bool {
        self.modules.contains(path)
    }

    pub fn modules(&self) -> impl Iterator<Item = &Path> {
        self.modules.iter().map(|p| p.as_path())
    }
}
