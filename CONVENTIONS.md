<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Elovalo project conventions

## Git usage conventions

This short guidance is heavily affected by [this writing][id1].
Read it for details.

### Branches
Branches can be divided into two categories: public and private
 * Public branches: master branch and all kind of feature branches
  * Public branches must be as linear and immutable as possible.
  * The commits may not cause build breaks.
 * Private branches: developer branches for daily work
  * There should be no need to have private branches in elovalo repository.
    Developers should keep their private branches in forked repositories in Github.
  * However, if one pushes his private branch into repository, its name
    must start with "developer nick" + '_'

### Merging
Before merging commits from private branch to public branch, a convoluted commit tree
should be cleaned e.g. with this command:

    git rebase --interactive master

The final merge to public branch should then be a fast-forward merge.

### Releasing
Separate release branches are not used. Code releases are just tagged in the master branch.

## Coding conventions

 * When writing C code, [Linux kernel coding style][id2] is used.
 * When writing Python code, [PEP 8 -- Style Guide for Python Code][id3] is used.

## References

[id1]: http://sandofsky.com/blog/git-workflow.html  "Understanding the Git workflow"
[id2]: https://www.kernel.org/doc/Documentation/CodingStyle "Linux kernel coding style"
[id3]: http://www.python.org/dev/peps/pep-0008/ "PEP 8 -- Style Guide for Python Code"
