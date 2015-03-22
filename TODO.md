# To Do

* store commit dates (first as strings, then as UTC and stdlib-ready dates). Allows for later usage in Which lib to use? algorithm.
    * No remote fetching of such info in SCM APIs: `stat` local archive or files
* module that compiles meta files together and that can apply processing functions to it
* log what deps (rebar's or SCM’s) a repo’s rev depends on
* SVN support: checkout trunk
* HG support: fetch/2 would have to clone repo for each rev
* For distributed generation, reduce with: `tar jxvf gend_repo.tar.bz2
