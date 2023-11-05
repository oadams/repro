# git-annex-repro
Reproducible data processing pipelines

To-Do
- [ ] Parse YAML. If I don't use a strict schema I should just raise warnings about stuff in the YAML that we're not going to use.
- [ ] Convert YAML into DAG. Raise errors if it's not a DAG. Raise warnings if there are programs specified in the command that aren't local scripts.
- [ ] Topologically sort the DAG.
- [x] Running ordered processes list
- [ ] Support for dependencies that are tracked in git. If the hash of the dependency is different than what was last on git, then tell or warn the user to commit them.
- [ ] Similarly, support for dependencies that are tracked in git-annex.
- [ ] Support incremental processing. YAML should support stages that operate on files in a directory indepedently.
- [ ] Robust testing, perhaps using Quickcheck.
