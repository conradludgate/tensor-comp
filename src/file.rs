use std::{collections::HashMap, num::NonZeroU16};

use ariadne::{Cache, Source};
use lasso::Rodeo;

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct File(NonZeroU16);
unsafe impl lasso::Key for File {
    fn into_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        int.checked_add(1)
            .and_then(|x| u16::try_from(x).ok())
            .and_then(NonZeroU16::new)
            .map(Self)
    }
}

/// A [`Cache`] that fetches [`Source`]s using the provided function.
#[derive(Debug, Clone)]
pub struct FileCache<I: AsRef<str>> {
    files: Rodeo<File>,
    sources: HashMap<File, Source<I>>,
}

impl<I: AsRef<str>> Default for FileCache<I> {
    fn default() -> Self {
        Self {
            files: Rodeo::new(),
            sources: Default::default(),
        }
    }
}

impl<I: AsRef<str>> FileCache<I> {
    pub fn insert(&mut self, path: &str, source: impl Into<Source<I>>) -> (File, &str) {
        let file = self.files.get_or_intern(path);
        let source = self.sources.entry(file).or_insert(source.into());
        (file, source.text())
    }
}

impl<I: AsRef<str>> Cache<File> for FileCache<I> {
    type Storage = I;

    fn fetch(
        &mut self,
        id: &File,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new("missing file") as Box<_>)
    }

    fn display<'a>(&self, id: &'a File) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.files
            .try_resolve(id)
            .map(|x| Box::new(x.to_string()) as Box<_>)
    }
}
