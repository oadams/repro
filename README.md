# Repro
Reproducible data processing pipelines

To-Do:
- [x] Parse YAML. If I don't use a strict schema I should just raise warnings about stuff in the YAML that we're not going to use.
- [x] Convert YAML into DAG. Raise errors if it's not a DAG.
- [x] Topologically sort the DAG.
- [x] Running ordered processes list
- [ ] Start checking `git hash-object` for all dependencies and then running stages that depend on them and stages that depend on those stages.
- [ ] Support stipulation of incremental processing in the YAML. YAML should support stages that operate on files in a directory indepedently. This involves first deciding on appropriate YAML syntax for indicating that a directory should have the script applied to each file incrementally.
- [ ] Implement the [Kowainik style guide](https://kowainik.github.io/posts/2019-02-06-style-guide)
- [ ] Refactoring, checking in on TODOs in code.
- [ ] Robust testing, perhaps using Quickcheck.
- [ ] Address all the warnings at compile time
- [ ] Improve error reporting at all stages of potential failure.
- [ ] Raise warnings if there are programs specified in the command that aren't local scripts.
- [ ] Rename from git-annex-repro to repro.
- [ ] Learn about dealing with IO exceptions in Haskell and thoroughly check our bases there.

This tool is essentially an independent implementation of [DVC repro](https://dvc.org/doc/command-reference/repro) that does not depend on DVC and offers support for incremental processing. Currently we are not there yet. Mention that it does depend on `git` though for the hashing.

Explain motivation for incremental processing.

Emphasize testing.

# How to use

Talk about building with stack, and other requirements such as getting haskell. Eventually should release executables. Currently we do:
```
stack build
stack exec git-annex-repro-exe
```

Input data format. I'd like to move towards a place where the YAML follows the same format as the dvc.yaml for defining stages, but fo the sake of implementation simplicity, currently the YAML takes the form:
```
- name: firststage
  cmd: echo "hello" > out1.txt
  outs: [out1.txt]
- name: secondstage
  cmd: false; echo "something" > out2.txt
  deps: [out1.txt]
  outs: [out2.txt]
- name: thirdstage
  cmd: echo "world"
  deps: [out2.txt]
  outs: [out3.txt]
```
Each item in the list is a stage in an overall DAG. Each stage has a name, a command, and defines files that are output. Stages can also have dependencies. A stage is run if one of the following is true:
- We do not have hashes of at least one of the outputs
- We do not have hashes of at least one of the dependencies.
- Hashes of any of the dependent files have changed.

# Implementation monologue notes (as I'm working)

Agenda is to start running stages where dependencies have changed or the output doesn't already exist.

Running dependencies on the basis of the git hash. I suppose necessary steps in every case is:
- Read in the hashes from a repro.lock file.
- Compute the hashes.
Then we compare these two sets and if there's any difference for a given stage we compute the stage. Important questions here are:
- What data structure should we use to store this information? A map from filenames to hashes is obvious. Since we're interested in stage-specific sets we want to be able to subset the map based on what's in a stage. This can probably be achieved by just doing a lookup for all dependencies and outputs and confirming that all values are the same.
- We need to be a little bit forward thinking about how this will extend to our incremental processing. We would like incremental processing of a given step to be expressed on a per-file basis, and somehow parametrize our command to indicate this. So I guess we'll just bear that in mind.

So with that let's start by creating a function to write the `repro.lock` file by checking the hashes of all deps and outs mentioned in the repro.yaml