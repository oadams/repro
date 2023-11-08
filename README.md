# git-annex-repro
Reproducible data processing pipelines

To-Do:
- [x] Parse YAML. If I don't use a strict schema I should just raise warnings about stuff in the YAML that we're not going to use.
- [x] Convert YAML into DAG. Raise errors if it's not a DAG.
- [x] Topologically sort the DAG.
- [x] Running ordered processes list
- [ ] Normalizing indentation among other things. Implement the [Kowainik style guide](https://kowainik.github.io/posts/2019-02-06-style-guide)
- [ ] Refactoring, checking in on TODOs in code.
- [ ] Start checking `git hash-object` for all dependencies and then running stages that depend on them and stages that depend on those stages.
- [ ] Support stipulation of incremental processing in the YAML. YAML should support stages that operate on files in a directory indepedently. This involves first deciding on appropriate YAML syntax for indicating that a directory should have the script applied to each file incrementally.
- [ ] Robust testing, perhaps using Quickcheck.
- [ ] Address all the warnings at compile time
- [ ] Improve error reporting at all stages of potential failure.
- [ ] Raise warnings if there are programs specified in the command that aren't local scripts.
