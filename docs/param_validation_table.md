# GIR parameter validation tables

## Struct types

| Direction | Caller-alloc | Ownership | Struct  | Struct*  | Struct** |
|-----------|--------------|-----------|---------|----------|----------|
| IN        | FALSE        | FALSE     | X       | X        |          |
| IN        | FALSE        | TRUE      |         | X        |          |
| IN        | TRUE         | FALSE     |         | X        |          |
| IN        | TRUE         | TRUE      |         | X        |          |
| OUT       | FALSE        | FALSE     |         |          | X        |
| OUT       | FALSE        | TRUE      |         |          | X        |
| OUT       | TRUE         | FALSE     |         | X        |          |
| OUT       | TRUE         | TRUE      |         | X        |          |
| INOUT     | FALSE        | FALSE     |         |          |          |
| INOUT     | FALSE        | TRUE      |         |          |          |
| INOUT     | TRUE         | FALSE     |         | X        |          |
| INOUT     | TRUE         | TRUE      |         |          |          |


## List container types

| Direction | Ownership | GList*  | GList**  |
|-----------|-----------|---------|----------|
| IN        | None      | X       |          |
| IN        | Container | X       |          |
| IN        | Full      | X       |          |
| OUT       | None      |         | X        |
| OUT       | Container |         | X        |
| OUT       | Full      |         | X        |
| INOUT     | None      |         | X        |
| INOUT     | Container |         |          |
| INOUT     | Full      |         |          |


## GArray and other container types

| Direction | Caller-alloc | Ownership | GArray* | GArray** |
|-----------|--------------|-----------|---------|----------|
| IN        | X            | None      | X       |          |
| IN        | X            | Container | X       |          |
| IN        | X            | Full      | X       |          |
| OUT       | FALSE        | None      |         | X        |
| OUT       | FALSE        | Container |         | X        |
| OUT       | FALSE        | Full      |         | X        |
| OUT       | TRUE         | None      | X       |          |
| OUT       | TRUE         | Container | X       |          |
| OUT       | TRUE         | Full      | X       |          |
| INOUT     | FALSE        | None      |         |          |
| INOUT     | FALSE        | Container |         |          |
| INOUT     | FALSE        | Full      |         |          |
| INOUT     | TRUE         | None      | X       |          |
| INOUT     | TRUE         | Container |         |          |
| INOUT     | TRUE         | Full      |         |          |
