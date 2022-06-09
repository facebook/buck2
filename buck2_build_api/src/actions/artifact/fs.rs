use buck2_core::{
    cells::paths::CellPath,
    fs::project::{ProjectFilesystem, ProjectRelativePathBuf},
};
use either::Either;

use crate::{
    actions::artifact::{Artifact, ArtifactKind, BuildArtifact, SourceArtifact},
    interpreter::rule_defs::artifact::StarlarkArtifactLike,
    path::{BuckOutPathResolver, BuckPathResolver},
};

#[derive(Clone)]
pub struct ArtifactFs {
    buck_path_resolver: BuckPathResolver,
    buck_out_path_resolver: BuckOutPathResolver,
    project_filesystem: ProjectFilesystem,
}

impl ArtifactFs {
    pub fn new(
        buck_path_resolver: BuckPathResolver,
        buck_out_path_resolver: BuckOutPathResolver,
        project_filesystem: ProjectFilesystem,
    ) -> Self {
        Self {
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        }
    }

    /// Resolves the 'Artifact's to a 'ProjectRelativePathBuf'
    pub fn resolve(&self, artifact: &Artifact) -> anyhow::Result<ProjectRelativePathBuf> {
        match artifact.0.as_ref() {
            ArtifactKind::Build(built) => Ok(self.resolve_build(built)),
            ArtifactKind::Source(source) => self.resolve_source(source),
        }
    }

    /// Resolves the 'Artifact's to a 'ProjectRelativePathBuf'
    pub(crate) fn resolve_artifactlike(
        &self,
        artifactlike: &dyn StarlarkArtifactLike,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        match artifactlike.fingerprint() {
            Either::Left(build) => Ok(self.buck_out_path_resolver.resolve_gen(&build)),
            Either::Right(source) => self.buck_path_resolver.resolve(source),
        }
    }

    pub fn retrieve_unhashed_location(&self, artifact: &BuildArtifact) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver
            .unhashed_gen(artifact.get_path())
    }

    pub fn resolve_build(&self, artifact: &BuildArtifact) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_gen(artifact.get_path())
    }

    pub fn resolve_cell_path(&self, path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve_cell_path(path)
    }

    pub fn resolve_source(
        &self,
        artifact: &SourceArtifact,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve(artifact.get_path())
    }

    /// Writes a file's contents to disk, creating any intermediate directories needed
    pub fn write_file(
        &self,
        dest: &BuildArtifact,
        contents: impl AsRef<[u8]>,
        executable: bool,
    ) -> anyhow::Result<()> {
        let dest_path = self.resolve_build(dest);
        self.project_filesystem
            .write_file(&dest_path, contents, executable)
    }

    pub fn copy(&self, src: &Artifact, dest: &BuildArtifact) -> anyhow::Result<()> {
        let src_path = self.resolve(src)?;
        let dest_path = self.resolve_build(dest);
        self.project_filesystem.copy(&src_path, &dest_path)
    }

    pub fn fs(&self) -> &ProjectFilesystem {
        &self.project_filesystem
    }

    pub fn buck_out_path_resolver(&self) -> &BuckOutPathResolver {
        &self.buck_out_path_resolver
    }
}
