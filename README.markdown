Browser for Scion
=================

The code here aims to be the backend part for package browser support in EclipseFP and any other development environment which uses Scion as backend. It can generate databases from the local set of packages, and from the Hackage database if internet connectivity is available. Later, those databases can be queries for modules, or declarations inside them.

The executable makes use of parallelism to run several instances of Haddock at once for creating the documentation. The maximum number of threads that can be created is 7 (this amount is in constant `numberOfThreads` in `src/Scion/Browser/Util.hs`). To make use of them, you must run the program with command arguments `+RTS -N7` (or any other number different from 7 if you want less threads to be created).

Currently, the functionality is provided via a command-line program which receives JSON requests and prints out JSON values in response. In each moment, a database is made *current* and will be the one to be queried. The available commands are:

* `{ "command" : "load-local-db", "filepath" : "local.db" }`: generates a database from the local installed packages, and saves the result in `local.db`,
* `{ "command" : "get-packages" }`: shows the list of packages in the current database,
* `{ "command" : "set-current-db", "new-db" : id }`: sets the current database. `id` can take different values: `"_all"` for both Hackage and local databases, `"_hackage"` for Hackage database (not yet implemented), `"_local"` for the local packages database and `{ "name" : "package-name", "version" : "x.y.z" }` for query only a specific package,
* `{ "command" : "get-modules", "module" : "mod" }`: get all descendants of the module `mod` (that is, if we query for modules in `A`, we would get `A.B`, `A.B.C`, and so on). To ask for all modules, query for module `""`,
* `{ "command" : "get-declarations", "module" : "mod" }`: queries the module `mod` for all declarations (datatypes, newtypes, typeclasses, instances, functions and type synonyms) inside it.

