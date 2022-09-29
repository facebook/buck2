use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;

use crate::artifact_value::ArtifactValue;
use crate::directory::ActionDirectoryMember;

pub trait OutputSize {
    fn calc_output_bytes(self) -> u64;
}

impl OutputSize for &ArtifactValue {
    fn calc_output_bytes(self) -> u64 {
        let mut output_bytes = 0;
        let mut walk = unordered_entry_walk(self.entry().as_ref());
        while let Some((_path, entry)) = walk.next() {
            match entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    output_bytes += f.digest.size();
                }
                _ => {}
            }
        }
        output_bytes
    }
}

impl<'a, T> OutputSize for T
where
    T: IntoIterator<Item = &'a ArtifactValue>,
{
    fn calc_output_bytes(self) -> u64 {
        self.into_iter().map(|v| v.calc_output_bytes()).sum()
    }
}
