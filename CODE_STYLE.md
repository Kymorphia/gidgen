# Code Style Guide

This document describes the coding conventions used in the gidgen project.

## Naming Conventions

### Variables

| Type | Convention | Example |
|------|------------|---------|
| Local variables | camelCase | `localVar`, `nodeIndex` |
| Function parameters | camelCase | `xmlNode`, `defPaths` |
| Module-level mutable variables | camelCase | `importManager` |
| **Module-level immutable variables** | **PascalCase** | `OwnershipValues`, `BasicTypeHash` |

### Immutable Variables

Immutable module-level variables use **PascalCase** to distinguish them from regular mutable variables. This convention emphasizes their constant nature and makes them visually distinct in the codebase.

```d
// Correct - PascalCase for immutable module-level variables
immutable dstring[] OwnershipValues = ["none", "container", "full"];
immutable dstring[] ContainerTypeValues = ["GLib.ByteArray", "GLib.Array", ...];
immutable string[] BasicTypeValues = ["bool", "byte", "char", ...];
immutable bool[dstring] BasicTypeHash;

// Incorrect - do not use camelCase for immutable module-level variables
immutable dstring[] ownershipValues = ...;  // Wrong
```

### Functions and Methods

| Type | Convention | Example |
|------|------------|---------|
| Regular functions | camelCase | `parseSelector`, `fromXml` |
| Properties | camelCase | `name`, `dName`, `fullDType` |

### Types

| Type | Convention | Example |
|------|------------|---------|
| Classes | PascalCase | `CodeWriter`, `XmlPatch` |
| Structs | PascalCase | `XmlSelector`, `SourceInfo` |
| Enums | PascalCase | `TypeKind`, `Ownership` |
| Enum members | PascalCase | `TypeKind.BasicAlias`, `Ownership.Full` |

### Constants

| Type | Convention | Example |
|------|------------|---------|
| `enum` manifest constants | PascalCase | `DefsCmdPrefix`, `MaxListStartIndent` |

```d
enum DefsCmdPrefix = "//!"d;
enum MaxListStartIndent = 4;
```

### Enum Members

Enum members use **PascalCase** in this project.

```d
// Correct - PascalCase for enum members
enum TypeKind
{
  Unknown,
  Basic,
  String,
  BasicAlias,
  Enum,
  Flags,
  Callback,
  Container,
  StructAlias,
  // ...
}

enum Ownership
{
  Unset = -1,
  None,
  Container,
  Full,
}

// Incorrect - do not use camelCase or SCREAMING_CASE for enum members
enum TypeKind
{
  unknown,      // Wrong - camelCase
  BASIC,        // Wrong - SCREAMING_CASE
  basic_alias,  // Wrong - snake_case
}
```

## Code Formatting

- Use 2 spaces for indentation (no tabs)
- Maximum line length: 120 characters (soft limit)
- Opening braces on the same line as the statement

## Documentation

- Use DDoc-style comments (`/** */` or `///`) for public API documentation
- Include `Params:` and `Returns:` sections for functions
- Document non-obvious behavior and edge cases

```d
/**
 * Check if a TypeKind is a structured type.
 * Params:
 *   kind = The TypeKind enum to check
 * Returns: true if kind is structured
 */
bool typeKindIsStructured(TypeKind kind)
{
  ...
}
```
