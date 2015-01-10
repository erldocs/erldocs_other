# To Do

* store commit dates (first as strings, then as UTC and stdlib-ready dates). Allows for later usage in Which lib to use? algorithm.
* module that compiles meta files together and that can apply processing functions to it
* log what deps (rebar's or SCM’s) a repo’s rev depends on
* svn support: checkout trunk
* hg support: fetch/2 would have to clone repo for each rev
* git support: add a default fetch/2 that, like hg's, does not optimise cloning
