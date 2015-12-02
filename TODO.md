# To Do

* store commit dates (first as strings, then as UTC and stdlib-ready dates). Allows for later usage in Which lib to use? algorithm.
    * No remote fetching of such info in SCM APIs: `stat` local archive or files
* module that compiles meta files together and that can apply processing functions to it
* log what deps (rebar's or SCM’s) a repo’s rev depends on
* HG support: fetch/2 would have to clone repo for each rev
* For distributed generation, reduce with: `tar jxvf gend_repo.tar.bz2`
* Tag a project using the 5 most outstanding words out of the project's doc / README
* Boolean that says whether Erlang code was found somewhere in the project
* Graph connecting all projects
    * See forks of a project
    * See most used projects (top 15)
    * See most used version of a project (by:user, by:project)
    * See most active projects
* Count of atoms introduced
* Calls that can be dangerous (list_to_atom/1, …)
* Fix `Too many db tables` [maybe this way](http://erlang.org/pipermail/erlang-questions/2005-October/017419.html)
