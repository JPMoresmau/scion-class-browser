Browser for Scion
=================

The code here aims to be the backend part for package browser support in EclipseFP and any other development environment which uses Scion as backend. It can generate databases from the local set of packages, and from the Hackage database if internet connectivity is available. Later, those databases can be queries for modules, or declarations inside them.

Building
--------

The executable makes use of parallelism to run several instances of Haddock at once for creating the documentation. To make use of them, you must run the program with command arguments `+RTS -N7` (or any other number different from 7). **Note**: it seems that running several threads in parallel is not working correctly. As of now, it's recommended to run the program with only one thread.

In some systems, while compiling the program an error like `can't load .so/.DLL for: ncursesw (/usr/lib/libncursesw.so: file too short)` may appear. This happens because GHC does not follow some kinds of links between libraries that GCC does. To solve it in my sstem, I run `cd /usr/lib && sudo mv libncurses.so libncurses.so.bak && sudo mv libncursesw.so libncursesw.so.bak && sudo ln -s /lib/libncurses.so.5 libncurses.so && sudo ln -s /lib/libncursesw.so.5 libncursesw.so`.

Available commands
------------------

Currently, the functionality is provided via a command-line program which receives JSON requests and prints out JSON values in response. In each moment, a database is made *current* and will be the one to be queried. The available commands are:

* `{ "command" : "load-local-db", "filepath" : "local.db", "rebuild" : true|false }`: loads the database specified in `filepath`. If `rebuild` is `true` it also regenerates a database from the local installed packages, and saves the result in `filepath`,
* `{ "command" : "load-hackage-db", "filepath" : "local.db", "rebuild" : true|false }`: loads the database specified in `filepath`. If `rebuild` is `true` it also regenerates a database from on-line Hackage information, and saves the result in `filepath`,
* `{ "command" : "get-packages" }`: shows the list of packages in the current database,
* `{ "command" : "set-current-db", "new-db" : id }`: sets the current database. `id` can take different values: `"_all"` for both Hackage and local databases, `"_hackage"` for Hackage database, `"_local"` for the local packages database and `{ "name" : "package-name", "version" : "x.y.z" }` for query only a specific package,
* `{ "command" : "get-modules", "module" : "mod" }`: get all descendants of the module `mod` (that is, if we query for modules in `A`, we would get `A.B`, `A.B.C`, and so on). To ask for all modules, query for module `""`,
* `{ "command" : "get-declarations", "module" : "mod" }`: queries the module `mod` for all declarations (datatypes, newtypes, typeclasses, instances, functions and type synonyms) inside it,
* `{ "command" : "hoogle-query", "query" : "mapM" }`: sends a query to Hoogle and relates the results with the current database (that is, only results that are in the current database will be returned). For this to work, the `hoogle` package must be installed and `hoogle data` must have been run at least once,
* `{ "command" : "hoogle-data" }`: ask Hoogle to refresh its internal database, using `hoogle data`,
* `{ "command" : "hoogle-check" }`: checks whether there is a working Hoogle program with a saved database.

