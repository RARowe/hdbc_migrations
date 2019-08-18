# Migration
A basic database migration library written in Haskell, dependent upon HDBC. The data access in this project is dependent upon HDBC.Sqlite3, but I may move that out.

## TODO
1. Integration tests (it would be nice to be able to start up and break down environment without bringing in additional System.Directory library)
2. Seperate data access into another project
3. Add ability to specify specific version for migration update/rollback (I do not have a personal use case for this)
