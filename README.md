
# Elm Sequelize UI

## In Progress

* Implement Resource for Schema/Table/Column

* Constraints
  * Refactor Constraint with Resource
  * Table constraints
    * Update column constraints without deleting and not erasing any fields
    * CRUD table constraints w/out names
    * Names
    * Composite unique keys
    * Composite foreign keys

## Bugs

* On delete column, or on update column to remove pk, only delete constraint if
  not composite primary key

# Todo
* Implement Resource
* Constraints
* Auto select foreign key column if only one
* Default id column on table create
* Make HTML more semantic
* Field Order
* Comments
* Template generator
* Elm CSS or CSS Library
* UI/UX
* Deployment
* Users/Sessions
* Github integration?
* Testing
* Build pipeline
* More advanced SQL types
