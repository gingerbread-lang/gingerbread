use crate::{Definition, Fqn, GetDefinitionError, Index, Name, WorldIndex};
use text_size::TextRange;

#[derive(Debug, Clone, Copy)]
pub enum Path {
    ThisModule(Name),
    OtherModule(Fqn),
}

#[derive(Debug, Clone, Copy)]
pub enum PathWithRange {
    ThisModule { name: Name, range: TextRange },
    OtherModule { fqn: Fqn, module_range: TextRange, name_range: TextRange },
}

pub struct NameResDiagnostic {
    pub kind: NameResDiagnosticKind,
    pub range: TextRange,
}

pub enum NameResDiagnosticKind {
    Undefined(Name),
}

pub fn resolve<'a>(
    path: PathWithRange,
    index: &'a Index,
    world_index: &'a WorldIndex,
    diagnostics: &mut Vec<NameResDiagnostic>,
) -> Option<&'a Definition> {
    match path {
        PathWithRange::ThisModule { name, range } => match index.get_definition(name) {
            Some(definition) => Some(definition),
            None => {
                diagnostics.push(NameResDiagnostic {
                    kind: NameResDiagnosticKind::Undefined(name),
                    range,
                });
                None
            }
        },

        PathWithRange::OtherModule { fqn, module_range, name_range } => {
            match world_index.get_definition(fqn) {
                Ok(definition) => Some(definition),
                Err(GetDefinitionError::UnknownModule) => {
                    diagnostics.push(NameResDiagnostic {
                        kind: NameResDiagnosticKind::Undefined(fqn.module),
                        range: module_range,
                    });
                    None
                }
                Err(GetDefinitionError::UnknownDefinition) => {
                    diagnostics.push(NameResDiagnostic {
                        kind: NameResDiagnosticKind::Undefined(fqn.name),
                        range: name_range,
                    });
                    None
                }
            }
        }
    }
}

impl PathWithRange {
    pub fn path(self) -> Path {
        match self {
            PathWithRange::ThisModule { name, .. } => Path::ThisModule(name),
            PathWithRange::OtherModule { fqn, .. } => Path::OtherModule(fqn),
        }
    }
}
