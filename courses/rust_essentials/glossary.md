# Rust Terminology Guide for Training Material

**Version:** 1.2.0
**Status:** Normative trainer reference
**Last updated:** 2026-07-13

This document defines the preferred Rust terminology for trainers, course authors,
reviewers, slides, labs, and exercises.

It is a **writing and review reference**, not a learner-facing glossary. Course
material should use these terms consistently unless different wording is required
when quoting compiler output, library documentation, or another source verbatim.

## Contents

- [AI-assisted maintenance](#ai-assisted-maintenance)
- [Authority](#authority)
- [Quick terminology guide](#quick-terminology-guide)
- [Core terms](#core-terms)
- [Terms to avoid or qualify](#terms-to-avoid-or-qualify)
- [Constructor terminology](#constructor-terminology)
- [Capitalization and formatting](#capitalization-and-formatting)
- [Review checklist](#review-checklist)
- [Reference sources](#reference-sources)
- [Version and change history](#version-and-change-history)

## AI-assisted maintenance

This glossary was developed and refined with the assistance of generative AI.

Using a generative-AI tool is recommended for future updates because the document
has several structural constraints that are easy to break during manual editing:

- the Quick terminology guide must remain concise;
- Core terms must remain detailed and strictly alphabetized;
- terminology must not be duplicated accidentally;
- the table of contents must remain synchronized;
- the semantic version and change history must be updated together;
- new definitions must remain consistent with existing entries;
- Markdown must remain readable directly in a text editor.

Any capable generative-AI system may be used, including ChatGPT, Claude, Gemini, or
another tool. No particular vendor or model is required.

AI output must still be reviewed by a human familiar with Rust. The tool should be
treated as an editing and consistency assistant, not as the final authority on Rust
semantics. Technical claims should be checked against the official sources listed in
this document.

### Recommended update workflow

1. Give the AI tool the complete current version of this file.
2. Add the specific requested changes or editorial directives.
3. Ask the tool to update the whole document rather than returning isolated snippets.
4. Review the generated diff.
5. Verify terminology against official Rust documentation when semantics changed.
6. Commit the updated glossary together with its version and changelog entry.

### Reusable update prompt

Copy the following prompt into the generative-AI tool, attach or paste the complete
current glossary, and append the requested changes under **Additional requests**.

```text
You are maintaining a normative, trainer-facing Rust terminology glossary.

Read the complete current glossary before making any change.

Update the glossary according to the requests below while preserving its scope,
tone, structure, and text-editor-friendly Markdown style.

Mandatory maintenance rules:

1. Return the complete updated Markdown document, not isolated fragments.
2. Keep the Quick terminology guide concise and optimized for rapid lookup.
3. Keep detailed definitions in the Core terms section.
4. Keep every Core terms heading in strict alphabetical order.
5. Do not create duplicate entries within the same section.
6. If a concise Quick entry and a detailed Core entry both exist, keep them
   intentionally distinct rather than repeating the same long definition twice.
7. Preserve existing correct terminology unless a requested change or verified
   correction requires modifying it.
8. Keep related terminology consistent across:
   - Quick terminology guide;
   - Core terms;
   - Terms to avoid or qualify;
   - Review checklist;
   - Reference sources.
9. Keep the table of contents synchronized with the document headings.
10. Preserve Markdown that is readable directly in a plain text editor.
11. Avoid Markdown tables unless they are clearly more readable than sections and
    lists in raw text.
12. Update the semantic version:
    - patch version for corrections, ordering fixes, or structural cleanup;
    - minor version for new terminology, sections, or maintenance features;
    - major version for incompatible policy or scope changes.
13. Add a dated entry at the top of Version and change history that clearly
    summarizes the modifications.
14. Do not remove existing definitions unless explicitly requested or made obsolete
    by a documented replacement.
15. Check capitalization and code formatting for Rust identifiers such as `Copy`,
    `Clone`, `Self`, `self`, `String`, and `Iterator::Item`.
16. Where a technical claim is uncertain, flag it for human verification instead of
    presenting it as authoritative.
17. Before returning the result, validate:
    - heading uniqueness;
    - strict alphabetical ordering of Core terms;
    - absence of accidental duplicate definitions;
    - valid table-of-contents links;
    - version and changelog consistency.

Additional requests:

[Add the requested terminology additions, corrections, or editorial directives here.]
```

### Human review checklist for AI-assisted updates

Before accepting an AI-generated revision, confirm that:

- the requested changes were actually applied;
- no unrelated definitions disappeared;
- no detailed entry was accidentally inserted into the Quick guide;
- every Core term remains in strict alphabetical order;
- similar concepts remain distinct rather than being merged incorrectly;
- the semantic version bump matches the scope of the update;
- the newest changelog entry accurately describes the diff;
- all technical wording remains defensible against official Rust documentation.

## Authority

Use the following sources in this order:

1. **The Rust Reference** for language terminology.
2. **The Rust Programming Language** for pedagogical wording.
3. **The Cargo Book** for Cargo concepts such as packages and targets.
4. Standard-library documentation for API-specific terminology.

When a familiar term from another language conflicts with Rust terminology, prefer
the Rust term.

## Quick terminology guide

### Item

Use for declarations that are part of a crate or module.

Examples:
- function item
- struct item
- module item
- trait item

Avoid:
- element
- member

### Associated item

Use for a function, type, or constant declared in a trait or `impl`.

Examples:
- associated function
- associated type
- associated constant

Avoid:
- class member

### Field

Use for data stored in a struct, union, tuple, tuple struct, or enum variant.

Examples:
- `user.name`
- `point.0`
- a field of a tuple variant
- a field of a struct variant

Avoid:
- member
- element

### Element

Use for a value stored at a position in an indexed sequence.

Examples:
- array element
- slice element
- vector element

Avoid:
- field
- item, unless discussing iteration

### Variant

Use for one possible form of an enum.

Examples:
- `Option::Some`
- `Result::Err`
- unit variant
- tuple variant
- struct variant

Avoid:
- case
- member

### Iterator item

Use for a value produced by an iterator.

Examples:
- the next iterator item
- `Iterator::Item`

Avoid:
- collection field

### Parameter and argument

Use **parameter** for a name in a declaration.

Example:
- `x` in `fn f(x: i32)`

Use **argument** for a value or expression supplied at a call site.

Example:
- `5` in `f(5)`

### Generic parameter and generic argument

Use **generic parameter** for a placeholder declared by a generic definition.

Example:
- `T` in `struct Box<T>`

Use **generic argument** for a concrete type, lifetime, or const supplied to it.

Example:
- `u8` in `Vec<u8>`

### Trait and trait bound

Use **trait** for the interface or behavioral abstraction itself.

Example:
- `Display`

Use **trait bound** for a restriction placed on a generic parameter or associated
type.

Example:
- `T: Display`

### Closure

Use **closure** for an anonymous function-like value that may capture its
surrounding environment.

Example:
- `|x| x + offset`

Use **capture** for the closure obtaining access to a local value from its
surrounding scope.

### Lifetime terminology

Use **lifetime** for the abstract extent during which a reference can be used
safely.

Use **lifetime annotation** for syntax such as `'a` when it relates lifetimes.

Use **lifetime parameter** for a named lifetime declared as a generic parameter,
such as `'a` in `struct View<'a>`.

Do not imply that writing an annotation extends a reference's lifetime.

### Macro terminology

Use **declarative macro** for a `macro_rules!` macro.

Use **procedural macro** for:
- custom derive macros;
- attribute-like macros;
- function-like procedural macros.

Do not imply that every macro invocation uses `!`: custom derives and
attribute-like procedural macros use attributes.

### Failure terminology

Use **recoverable error** for an expected failure represented through `Result`.

Use **panic** for Rust's panic mechanism, normally triggered by `panic!` or by an
operation that panics.

Avoid treating every panic as ordinary error handling.

### Slice

Use **slice** for a dynamically sized view into a contiguous sequence.

Examples:
- `[T]` is the slice type;
- `&[T]` is a shared borrowed slice;
- `&mut [T]` is a mutable borrowed slice.

A slice does not own its elements.

### String terminology

Use **`String`** for the owned, growable UTF-8 string type.

Use **string slice** for `str`, normally encountered through `&str`.

Avoid using the unqualified word *string* when ownership or borrowing matters.

### Ownership, move, `Copy`, and `Clone`

Use **ownership** for Rust's responsibility model for values and cleanup.

Use **move** when ownership is transferred and the original place can no longer be
used.

Use **`Copy`**—capitalized and formatted as code—when referring to the trait that
allows implicit duplication instead of a move.

Use **`Clone`**—capitalized and formatted as code—when referring to the trait for
explicit duplication, usually through `.clone()`.

Avoid saying that every assignment “copies” a value.

### Borrow and reference

Use **borrow** or **borrowing** for temporary access to a value without taking
ownership.

Use **reference** for the pointer-like value created by borrowing, such as `&T` or
`&mut T`.

A reference borrows data; it does not own that data.

### Smart pointer

Use **smart pointer** for an owning or managing data structure that behaves like a
pointer and provides additional capabilities.

Examples:
- `Box<T>`
- `Rc<T>`
- `Arc<T>`

Do not use **smart pointer** as a synonym for **reference**.

### Interior mutability

Use **interior mutability** for mutation performed through a shared reference,
according to the rules enforced by a type such as `Cell`, `RefCell`, `Mutex`, or
`RwLock`.

Do not call access through `RefCell<T>` or `Mutex<T>` a mutable reference unless an
actual `&mut T` is involved.

### Path

Use **path** for syntax that names an item, variant, associated item, or other named
entity.

Examples:
- `std::collections::HashMap`
- `crate::parser::parse`
- `self::helper`
- `super::Config`

Use **absolute path** for a path resolved from a crate root.

Use **relative path** for a path resolved from the current module or through `self`
or `super`.

### `Self` and `self`

Use **`Self`** for the type alias denoting the implementing or current type.

Use **`self`** for the receiver parameter or receiver value of a method.

Preserve the capitalization when speaking or writing about the distinction.

### Unsafe terminology

Use **unsafe block** for `unsafe { ... }`.

Use **unsafe function** for `unsafe fn ...`.

Use **unsafe operation** for an operation that requires an unsafe context.

Avoid the vague phrase *unchecked code*: `unsafe` permits specific operations but
does not disable Rust's type system or all compiler checks.

### Match arm

Use for one branch of a `match` expression.

Example:
- `Some(x) => x`

Avoid:
- case

### Binding

Use for a name introduced by a pattern.

Examples:
- `x` in `let x = 1`
- `value` in `let Some(value) = optional`

Avoid:
- field
- argument

### Shadowing and mutation

Use **shadowing** when a new binding reuses an existing name.

Use **mutation** when the value accessible through an existing mutable place is
changed.

Do not describe a shadowed binding as having been mutated.

### Receiver

Use for `self`, `&self`, or `&mut self` in a method.

Example:
- `&self` in `fn len(&self) -> usize`

### Expression

Use for Rust code that evaluates to a value.

Examples:
- `if` expression
- `match` expression
- block expression
- function call expression

### Expression statement

Use for an expression placed in statement position so that its value is discarded.

Example:
- `5;`

Do not say that the statement itself evaluates to `()`. More precisely:

- a block ending in `5` has the value `5`;
- a block ending in `5;` discards `5` and has no tail expression, so the block has
  the value `()`.

### Statement

Use for a construct appearing in statement position.

Examples:
- `let x = 1;`
- `do_work();`

Do not call `if` or `match` “statements” when explaining their language semantics:
they are expressions, even when their result is ignored.

## Core terms

The entries in this section are alphabetized for quick lookup.

### Absolute path

A path resolved from a crate root rather than from the current module.

Within the current crate, an absolute path commonly begins with `crate::`:

```rust
crate::parser::parse()
```

A path beginning with an external crate name also starts from that crate's root:

```rust
std::collections::HashMap::new()
```

Contrast with **Relative path**.

### Argument

An expression supplied to a function, method, closure, or macro invocation.

```rust
draw(point, 3);
```

`point` and `3` are arguments.

Do not use *argument* for the names declared in a function signature.

### Associated function

A function defined in an `impl` block or declared in a trait.

```rust
impl Buffer {
    fn new() -> Self {
        Self { data: Vec::new() }
    }
}
```

`Buffer::new` is an associated function.

An associated function is a **method** only when it has a `self` receiver.

### Associated item

An item declared in a trait or defined in an implementation.

Rust associated items are:

- associated functions, including methods;
- associated constants;
- associated types.

Avoid *class member*.

### Binding

The association between a name and a value, usually introduced by a pattern.

```rust
let mut count = 0;

let Some(value) = optional else {
    return;
};
```

`count` and `value` are bindings.

Use *binding* when discussing `let`, destructuring, shadowing, moves, and pattern
matching. *Variable* remains acceptable in introductory material when the distinction
is not important.

### Block expression

A sequence of statements followed by an optional tail expression, enclosed in
braces.

```rust
let total = {
    let subtotal = 20;
    subtotal + 5
};
```

The block evaluates to `25`. `subtotal + 5` is its tail expression.

A block with no tail expression has the value `()`.

### Borrow / borrowing

Temporary access to a value without taking ownership of it.

Borrowing normally produces a reference:

```rust
let name = String::from("Ada");
let length = name.len();
let view: &str = &name;
```

`view` borrows from `name`. Ownership of the `String` remains with `name`.

Preferred wording:

- **shared borrow** for access through `&T`;
- **mutable borrow** for access through `&mut T`;
- **borrowed value** for a value being accessed through a borrow.

Avoid saying that borrowing “copies ownership.”

### `Clone`

The standard-library trait for explicit duplication.

```rust
let original = String::from("hello");
let duplicate = original.clone();
```

The call to `.clone()` explicitly creates another value. The original remains
usable.

Use **clone** as a verb when an explicit `Clone::clone` operation is performed.

Do not assume that cloning is cheap or bitwise. A `Clone` implementation may allocate
memory or perform other work.

Contrast with **`Copy`**, which permits implicit duplication.

### Closure

An anonymous function-like value created by a closure expression.

```rust
let offset = 10;
let add_offset = |value| value + offset;
```

A closure may **capture** values from its surrounding **environment**.

For training purposes:

- **capture** means that the closure obtains access to a local value from the
  surrounding scope;
- **environment** means the captured values or references carried by the closure;
- captures may occur by shared borrow, mutable borrow, or value;
- `move` requests capture by value, although captured values may still be copied
  when their types implement `Copy`.

A function item does not capture a local environment.

### `Copy`

The marker trait that allows values to be implicitly duplicated rather than moved.

```rust
let first: u32 = 10;
let second = first;

// `first` remains usable because `u32: Copy`.
```

Use **`Copy` type** or “the type implements `Copy`.”

The semantic distinction is more important than the implementation description:
assignment, argument passing, and similar operations leave the source usable.

For `Copy` types, duplication is compatible with a simple bitwise copy. Do not use
the lowercase word *copy* ambiguously when the `Copy` trait is the point being
taught.

Contrast with **move** and **`Clone`**.

### Crate

A unit of compilation and linking with a module tree rooted at the crate root.

A crate can produce a library or executable. Do not define a crate merely as a
folder or as a Cargo package.

### Declarative macro

A macro defined using `macro_rules!`.

```rust
macro_rules! say_hello {
    () => {
        println!("Hello");
    };
}
```

Declarative macros perform syntax-based matching and expansion.

They are normally invoked with `!`, for example `say_hello!()`.

### Element

A value at a position in an indexed sequence or collection, especially:

- arrays;
- slices;
- `Vec<T>`;
- similar sequence-like collections.

```rust
let values = [10, 20, 30];
let second = values[1];
```

`20` is the second **element**.

Use **element**, not *item*, for array, slice, and vector positions unless quoting an
API that explicitly uses *item*.

### Enum variant

One of the alternatives declared by an enum.

```rust
enum Message {
    Quit,
    Move(i32, i32),
    Write { text: String },
}
```

`Quit`, `Move`, and `Write` are variants.

Preferred variant forms:

- **unit variant**: `Quit`;
- **tuple variant**: `Move(i32, i32)`;
- **struct variant**: `Write { text: String }`.

The data carried by tuple and struct variants consists of **fields**.

### Error / recoverable error

An expected failure represented as data, normally through `Result<T, E>`.

```rust
fn read_config() -> Result<Config, ConfigError> {
    // ...
}
```

Preferred wording:

- **recoverable error** when contrasting `Result` with panics;
- **error value** for the value in `Err(error)`;
- **error type** for `E` in `Result<T, E>`;
- **error propagation** for forwarding an error, for example with `?`;
- **error handling** for inspecting, transforming, recovering from, or propagating
  an error value.

Do not use **panic** as a synonym for a recoverable error.

### Expression

Rust syntax that evaluates to a value and may have side effects.

Examples include:

- literals and paths;
- function and method calls;
- blocks;
- `if`;
- `match`;
- `loop`, `while`, and `for`.

Use **if expression**, **match expression**, and **loop expression**.

When an expression's result is ignored, say that the expression is *used in
statement position* rather than renaming it an “if statement” or “match statement.”

### Expression statement

A statement formed from an expression whose value is discarded.

```rust
calculate();
5;
```

In `5;`, the expression `5` is evaluated and its value is discarded.

Be precise when comparing these blocks:

```rust
let a = {
    5
};

let b = {
    5;
};
```

- the first block has type `i32` and value `5`;
- the second block has type `()` because it has no tail expression.

Avoid saying that the statement `5;` itself “evaluates to `()`.” Statements do not
produce values; the containing block does.

### Field

A component of a struct, union, tuple, tuple struct, or data-carrying enum variant.

```rust
struct User {
    name: String,
    active: bool,
}

let pair = (10, false);
```

`name` and `active` are named fields. `pair.0` and `pair.1` access tuple fields.

Use:

- **named field** for `name: String`;
- **tuple field** or **unnamed field** for `.0`;
- **variant field** when the field belongs to an enum variant.

Avoid *member* and avoid *element* for field access.

### Free function / free item

An item that is not associated with a trait or type.

```rust
fn parse() {}
```

`parse` is a free function.

Use this only when the distinction from an associated function matters. Otherwise,
*function* is sufficient.

### Function

A callable item declared with `fn`.

```rust
fn add(left: i32, right: i32) -> i32 {
    left + right
}
```

Functions declare parameters and are called with arguments.

A function item does not capture local variables from its surrounding scope.
A closure may capture its environment.

### Generic argument

A concrete type, lifetime, or const used when instantiating a generic construct.

```rust
Vec::<u8>::new()
```

`u8` is a generic type argument.

### Generic parameter

A type, lifetime, or const placeholder declared by a generic construct.

```rust
struct Buffer<T, const N: usize> {
    data: [T; N],
}
```

`T` is a type parameter and `N` is a const parameter.

Use:

- **type parameter**;
- **lifetime parameter**;
- **const parameter**.

### Implementation / `impl` block

An `impl` block defines associated items for a type or implements a trait for a type.

```rust
impl Display for Message {
    // ...
}
```

Preferred terms:

- **inherent implementation**: `impl Type { ... }`;
- **trait implementation**: `impl Trait for Type { ... }`;
- **`impl` block**: acceptable concise wording.

Avoid *class implementation*.

### Interior mutability

A design pattern that permits mutation through a shared reference by moving the
relevant checks from ordinary compile-time exclusivity to rules enforced by a type.

Common examples include:

- `Cell<T>`;
- `RefCell<T>`;
- `Mutex<T>`;
- `RwLock<T>`;
- atomic types.

```rust
use std::cell::RefCell;

let values = RefCell::new(vec![1, 2]);
values.borrow_mut().push(3);
```

The `RefCell` is accessed through a shared reference, while its contents are mutated
under `RefCell`'s dynamic borrowing rules.

Do not describe this as obtaining an ordinary `&mut T` through `&T`. Use the term
**interior mutability** and name the mechanism involved.

### Item

A compile-time component of a crate, organized through the module tree.

Items include modules, functions, structs, enums, unions, traits, implementations,
type aliases, constants, statics, `use` declarations, external blocks, and relevant
macro items.

Use **item** when discussing:

- crate or module contents;
- item visibility;
- free and associated items;
- the `Iterator::Item` associated type;
- values yielded by an iterator, when *iterator item* is useful.

Do not use **item** as the default word for an array, slice, or vector element.

> `Item` in `Iterator::Item` is an identifier naming an associated type. It should
> be formatted as code and capitalized exactly as written.

### Lifetime

The abstract extent during which a reference can be used safely.

Lifetimes are part of Rust's borrow checking model. They describe relationships
between references and the values they borrow; they are not runtime objects or
manually managed timers.

Most lifetimes are inferred.

A lifetime is not necessarily identical to a lexical block scope. Non-lexical
lifetimes allow a borrow to end after its last use, before the enclosing scope ends.

### Lifetime annotation

Syntax that names or relates lifetimes, such as `'a`.

```rust
fn first<'a>(value: &'a str) -> &'a str {
    value
}
```

A lifetime annotation describes a relationship that the borrow checker must verify.

It does **not** extend the lifetime of a reference or cause a value to live longer.

### Lifetime parameter

A named generic lifetime declared by a function, type, trait, or implementation.

```rust
struct View<'a> {
    text: &'a str,
}
```

`'a` is a lifetime parameter.

At a use site, a supplied lifetime is a lifetime argument.

### Macro

A Rust metaprogramming construct that expands syntax into other Rust syntax.

The two main families are:

- **declarative macros**, commonly defined with `macro_rules!`;
- **procedural macros**, implemented as Rust functions operating on token streams.

Do not say that every macro is invoked with `!`.

### Match arm

A pattern, optional guard, and result expression within a `match`.

```rust
match value {
    Some(x) if x > 0 => x,
    _ => 0,
}
```

Each branch ending in `=> ...` is a match arm.

Use *arm*, not *case*.

### Method

An associated function whose first parameter is a `self` receiver.

```rust
impl Counter {
    fn increment(&mut self) {
        self.value += 1;
    }
}
```

`increment` is a method. `Counter::new()` is an associated function if it has no
`self` receiver.

### Module

A namespace and privacy container for zero or more items within a crate.

A module may be declared inline or loaded from another file. It is not inherently a
folder, even though the source-tree layout may use directories to organize module
files.

### Move

A transfer of ownership from one place to another.

```rust
let first = String::from("hello");
let second = first;
```

The `String` value is moved into `second`, so `first` can no longer be used.

Preferred wording:

- “the value is moved”;
- “ownership moves to `second`”;
- “the source binding is no longer usable.”

A move does not necessarily copy bytes at runtime, and the language-level term
should not be explained as a guaranteed physical memory transfer.

Types that implement `Copy` are implicitly duplicated instead of moved in these
contexts.
### Ownership

Rust's model for determining which value or place is responsible for a resource's
validity and cleanup.

For trainer-facing explanations:

- an owned value is responsible for the resource it manages;
- moving transfers that ownership;
- borrowing grants temporary access without transferring ownership;
- when an owning value is dropped, its owned resources are released according to
  their `Drop` behavior.

Avoid reducing ownership to “which variable contains the bytes.” Ownership is a
semantic responsibility and may involve resources beyond heap memory.

### Package

A Cargo unit described by a `Cargo.toml` manifest.

A package contains one or more targets. Those targets are compiled into crates.

Do not use *package* and *crate* interchangeably.

### Panic

Rust's panic mechanism, triggered explicitly by `panic!` or implicitly by an
operation that panics.

A panic:

- interrupts normal control flow;
- may unwind the current thread's stack or abort, depending on configuration;
- is normally used for programming bugs, violated assumptions, or situations from
  which the current code cannot reasonably continue;
- is not the normal representation of an expected recoverable failure.

In introductory material, *unrecoverable error* may be used to contrast a panic with
`Result`, but **panic** is the more precise Rust term.

Avoid implying that every panic necessarily terminates the whole process.

### Parameter

A named input declared in a function, method, closure, or similar definition.

```rust
fn resize(width: u32, height: u32) {}
```

`width` and `height` are parameters.

For a method, `self`, `&self`, or `&mut self` is the receiver parameter.

### Path

Syntax used to name an item, enum variant, associated item, type, module, or other
named entity.

```rust
std::collections::HashMap
crate::parser::parse
self::helper
super::Config
Type::associated_function
```

A path consists of one or more path segments separated by `::`.

Use:

- **absolute path** when resolution starts from a crate root;
- **relative path** when resolution starts from the current module, `self`, or
  `super`.

Avoid calling every path a filesystem path.

### Pattern

Syntax used to test and destructure a value and optionally introduce bindings.

```rust
let (x, y) = point;

match message {
    Message::Write { text } => println!("{text}"),
    _ => {}
}
```

`(x, y)` and `Message::Write { text }` are patterns. `x`, `y`, and `text` are
bindings.

### Procedural macro

A macro implemented as Rust code that consumes and produces token streams.

The three forms are:

- **custom derive macro**, used through `#[derive(...)]`;
- **attribute-like procedural macro**, used as an attribute;
- **function-like procedural macro**, invoked with `!`.

Do not describe custom derives or attribute-like macros as `!` macros.

### Receiver

The `self` parameter of a method:

- `self`;
- `&self`;
- `&mut self`;
- another permitted explicit receiver form.

At the call site in `value.method()`, `value` is the receiver expression.

### Reference

A pointer-like value that borrows another value without taking ownership.

The common reference types are:

- `&T`: shared reference;
- `&mut T`: mutable reference.

```rust
let value = String::from("hello");
let shared: &String = &value;
```

A reference is subject to Rust's borrowing and lifetime rules.

A reference is not an owning smart pointer. Types such as `Box<T>`, `Rc<T>`, and
`Arc<T>` manage ownership; `&T` and `&mut T` borrow.

### Relative path

A path resolved relative to the current module or through an explicitly relative
prefix.

Examples:

```rust
helper::run()
self::helper::run()
super::Config
```

A bare leading identifier is normally resolved in the current scope. `self::`
starts explicitly from the current module, and `super::` starts from its parent.

Contrast with **Absolute path**.

### `Self`

A type alias, written with a capital `S`, for the current or implementing type.

In an inherent implementation:

```rust
impl Buffer {
    fn new() -> Self {
        Self { data: Vec::new() }
    }
}
```

`Self` denotes `Buffer`.

In a trait, `Self` denotes the type that implements the trait.

Contrast with lowercase **`self`**, which is a method receiver parameter or receiver
value. `Self` is a type; `self` is a value-level name.

### Shadowing

The introduction of a new binding that uses the same name as an earlier binding.

```rust
let value = "42";
let value: u32 = value.parse().unwrap();
```

The second `value` shadows the first. It does not mutate the first binding.

Shadowing can change the bound value's type and mutability because it creates a new
binding.

### Slice

A dynamically sized view into a contiguous sequence of elements.

The slice type is `[T]`. Because it is dynamically sized, it is normally used behind
a pointer:

- `&[T]`: shared borrowed slice;
- `&mut [T]`: mutable borrowed slice;
- `Box<[T]>`: owned boxed slice.

A borrowed slice does not own its elements. It carries access to a sequence and a
length.

Do not describe a slice as an owned vector or as a raw pointer.

### Smart pointer

A data structure that behaves like a pointer while also providing ownership,
resource management, metadata, or other capabilities.

Common examples include:

- `Box<T>` for unique ownership of heap-allocated data;
- `Rc<T>` for single-threaded reference-counted ownership;
- `Arc<T>` for thread-safe reference-counted ownership.

Some smart-pointer types also support interior mutability when combined with types
such as `RefCell<T>`, `Mutex<T>`, or `RwLock<T>`.

Use the specific type name whenever possible. Do not call `&T` or `&mut T` smart
pointers: they are references and borrow data rather than own it.

### Statement

A construct occurring in statement position within a block.

The main categories are:

- `let` statements;
- item declaration statements;
- expression statements;
- macro invocation statements.

A statement does not produce a value.

A semicolon commonly forms an expression statement and causes the expression's
value to be discarded.

Do not classify `if`, `match`, `while`, `for`, or `loop` as fundamentally separate
“statement constructs”; they are expressions in Rust.

### String

`String` is Rust's owned, growable UTF-8 string type.

```rust
let mut text = String::from("hello");
text.push('!');
```

Use **`String`** when the concrete owned type matters.

Avoid saying only *string* when the distinction from `&str` is relevant.

### String slice

`str` is Rust's dynamically sized UTF-8 string slice type.

It is most commonly used through a borrowed reference:

```rust
let text: &str = "hello";
```

Preferred wording:

- **string slice** for `str` or, pedagogically, for a borrowed `&str`;
- **shared string slice** or **borrowed string slice** when emphasizing `&str`;
- **mutable string slice** for `&mut str`, which is uncommon.

A `&str` is borrowed and does not own the string data.

Do not call every textual value a `String`.

### Struct

A nominal data type with one of three forms:

- **struct with named fields**;
- **tuple struct**;
- **unit struct**.

*Named-field struct* is acceptable trainer shorthand, but *struct with named fields*
is clearer in formal definitions.

### Target

A Cargo build target corresponding to source that Cargo compiles into a crate.

Common target kinds include:

- library;
- binary;
- example;
- integration test;
- benchmark.

A package can contain multiple targets and therefore produce multiple crates.

### Trait

A language item that defines shared behavior through associated items.

A trait can declare:

- methods and associated functions;
- associated types;
- associated constants.

```rust
trait Render {
    fn render(&self);
}
```

`Render` is a trait.

Do not call `T: Render` a trait; it is a **trait bound** involving the trait
`Render`.

### Trait bound

A constraint requiring a type, lifetime, or associated type to satisfy a trait or
lifetime relationship.

```rust
fn print<T: Display>(value: T) {
    println!("{value}");
}
```

`T: Display` is a trait bound. `Display` is the trait.

Equivalent or related syntax includes:

```rust
fn print<T>(value: T)
where
    T: Display,
{
    println!("{value}");
}
```

Preferred wording:

- “`T` has a `Display` bound”;
- “the generic parameter is constrained by the `Display` trait”;
- “add a `Send + Sync` trait bound.”

Avoid saying that the bound itself is a trait.

### Trait item

An associated item declared by a trait: an associated function, associated constant,
or associated type.

A method signature inside a trait is a trait item.

### Tuple field

A numerically named field of a tuple or tuple struct.

```rust
let point = (4, 7);
println!("{}", point.0);
```

Use **field `0`** or **first tuple field**.

Avoid *member*. For consistency in the training material, prefer *field* over
*element* when discussing tuple access, field types, visibility, layout, or drop
order.

### Unsafe block / unsafe function

An **unsafe block** is a block introduced by `unsafe`:

```rust
unsafe {
    raw_pointer.read()
}
```

An **unsafe function** is declared with `unsafe fn` and requires callers to uphold
the function's documented safety contract.

Use **unsafe operation** for an operation that may only be performed in an unsafe
context, such as dereferencing a raw pointer or calling an unsafe function.

Important terminology rule:

- `unsafe` does not turn off the borrow checker, type checking, or all safety checks;
- it permits a limited set of operations whose safety obligations must be upheld by
  the programmer;
- prefer **unsafe block**, **unsafe function**, **unsafe trait**, or **unsafe
  operation** over the vague phrase *unchecked code*.

Use **Unsafe Rust** when referring collectively to the language features involving
these explicit safety obligations.

### Value

Runtime data of a particular type.

A value may be owned, moved, copied, borrowed, stored in a variable, passed as an
argument, or produced by an expression.

Do not use *variable* and *value* interchangeably.

### Variable

A local named binding used to refer to a value.

In introductory material, *variable* is acceptable for a `let` binding. Use the more
precise *binding* when explaining:

- mutability of the binding;
- shadowing;
- destructuring;
- ownership transfer;
- pattern matching.

Prefer “mutable binding” over implying that every value itself is intrinsically
mutable.

### Variant

See **Enum variant**.

## Terms to avoid or qualify

### Struct or tuple member

Prefer:
- field

### Array item or vector item

Prefer:
- element

### Enum case

Prefer:
- variant

### Match case

Prefer:
- match arm

### Class

Prefer the actual Rust construct:
- struct
- enum
- trait
- type
- implementation

### Object

Prefer:
- value
- instance

Keep **trait object** when referring to that exact Rust concept.

### Method without `self`

Prefer:
- associated function

### Constructor method

Prefer:
- associated function used as a constructor

### If statement

Prefer:
- if expression

When relevant, say that it is used in statement position.

### Match statement

Prefer:
- match expression

When relevant, say that it is used in statement position.

### For statement or while statement

Prefer:
- for loop expression
- while loop expression

### Parameter at a call site

Prefer:
- argument

### Argument in a declaration

Prefer:
- parameter

### Trait when referring to a restriction

Prefer:
- trait bound

Example:
- `T: Display` is a trait bound;
- `Display` is the trait.

### Anonymous function when discussing captures

Prefer:
- closure

Use *anonymous function* only as an introductory analogy, then introduce the Rust
term **closure**.

### Scope as an exact synonym for lifetime

Prefer:
- lifetime

A lexical scope and a reference's inferred lifetime are related but not always
identical.

### Lifetime annotation extends a lifetime

Avoid this claim.

Prefer:
- the annotation expresses a relationship;
- the borrow checker verifies that relationship.

### Every macro uses `!`

Avoid this claim.

Only function-like macro invocations use `!`. Custom derives and attribute-like
procedural macros use attributes.

### Panic as a normal recoverable error

Prefer:
- recoverable error represented by `Result`;
- panic for the panic mechanism.

### Unrecoverable error as an exact synonym for panic

Prefer:
- panic

*Unrecoverable error* is acceptable introductory wording, but a panic may unwind or
abort and is the precise mechanism being discussed.

### Slice as an owned collection

Prefer:
- borrowed slice;
- boxed slice;
- `Vec<T>`, when ownership and growth are intended.

### String as an unqualified type name

Prefer:
- `String` for the owned type;
- `&str` or string slice for borrowed text.

### Shadowing described as mutation

Avoid:
- “the variable was mutated” when a new binding was introduced with `let`.

Prefer:
- “the new binding shadows the previous binding.”

### Expression statement evaluates to `()`

Avoid this formulation.

Prefer:
- the expression's value is discarded;
- when a block has no tail expression, the block has the value `()`.

### Copying when the value is actually moved

Avoid:
- “the value is copied” for an assignment of a non-`Copy` value.

Prefer:
- “the value is moved”;
- “ownership is transferred.”

Use **copied** when the type implements `Copy`, and **cloned** when an explicit
`Clone` operation occurs.

### Smart pointer as a synonym for reference

Avoid this equivalence.

Prefer:
- **reference** for `&T` or `&mut T`;
- **smart pointer** for an owning or managing pointer-like type such as `Box<T>`,
  `Rc<T>`, or `Arc<T>`.

### Interior mutability described as a mutable reference

Avoid:
- “`RefCell` gives us a mutable reference through `&T`” without qualification.

Prefer:
- “`RefCell` provides interior mutability and dynamically checks its borrows.”

### Filesystem path when discussing Rust name resolution

Prefer:
- **path** or **module path** for `crate::module::Item`;
- **filesystem path** only for an actual file or directory.

### `Self` and `self` used interchangeably

Avoid this.

Prefer:
- **`Self`** for the current type alias;
- **`self`** for the method receiver value or parameter.

### Unchecked code as a synonym for unsafe code

Avoid this phrase.

Prefer the exact construct:
- unsafe block;
- unsafe function;
- unsafe trait;
- unsafe operation.

Unsafe Rust still receives normal parsing, type checking, and most other compiler
checks.

### Module folder

Prefer:
- module

Mention the source file or directory separately when discussing layout.

### Package as a synonym for crate

Prefer the correct Cargo term:
- package
- target
- crate

These concepts are related but not interchangeable.

## Constructor terminology

Rust has no dedicated constructor declaration syntax comparable to constructors in
class-based languages.

Use:

- **tuple struct constructor** for the generated constructor of a tuple struct;
- **variant constructor** for tuple or unit enum variants where relevant;
- **associated function used as a constructor** for functions such as `Type::new`.

It is acceptable to call `Type::new` a *constructor* pedagogically, provided the
material does not imply that `new` has special language semantics.

## Capitalization and formatting

- Format Rust syntax, identifiers, types, traits, and paths as code:
  `Iterator::Item`, `Option<T>`, `self`, `crate`, `std::mem::drop`.
- Use lowercase for ordinary terminology: item, field, element, variant.
- Capitalize `Item` only when referring to the actual associated type
  `Iterator::Item` or another identifier named `Item`.
- Preserve compiler terminology when quoting diagnostics verbatim.
- Do not silently rewrite diagnostic wording to match this glossary.

## Review checklist

Before approving Rust training material, check that:

- [ ] module and crate declarations are called **items**;
- [ ] struct, tuple, union, and variant components are called **fields**;
- [ ] array, slice, and vector contents are called **elements**;
- [ ] enum alternatives are called **variants**;
- [ ] iterator outputs are called **items** when tied to `Iterator::Item`;
- [ ] declarations use **parameters** and calls use **arguments**;
- [ ] generic definitions use **generic parameters** and instantiations use
      **generic arguments**;
- [ ] a behavioral abstraction is called a **trait**;
- [ ] a restriction such as `T: Display` is called a **trait bound**;
- [ ] functions with a `self` receiver are **methods**;
- [ ] functions without `self` inside an `impl` are **associated functions**;
- [ ] anonymous function-like values that capture their environment are called
      **closures**;
- [ ] *capture* and *environment* are explained without implying hidden global
      state;
- [ ] **lifetime**, **lifetime annotation**, and **lifetime parameter** are not used
      interchangeably;
- [ ] lifetime annotations are not said to extend lifetimes;
- [ ] `macro_rules!` macros are called **declarative macros**;
- [ ] custom derives, attribute-like macros, and function-like procedural macros are
      distinguished;
- [ ] expected failures represented by `Result` are called **recoverable errors**;
- [ ] **panic** is not presented as ordinary recoverable error handling;
- [ ] `[T]`, `&[T]`, and `&mut [T]` are described as slice forms rather than owned
      vectors;
- [ ] `String` and `&str` are distinguished when ownership matters;
- [ ] shadowing is not described as mutation;
- [ ] `if`, `match`, and loops are described as **expressions**;
- [ ] an **expression statement** is described as discarding an expression's value;
- [ ] statements are not said to produce values;
- [ ] `match` branches are called **arms**;
- [ ] package, target, crate, and module are not used interchangeably;
- [ ] move, `Copy`, and `Clone` are distinguished;
- [ ] `Copy` and `Clone` are capitalized and formatted as trait names;
- [ ] ownership transfer is called a **move**, not an implicit clone;
- [ ] borrowing is distinguished from ownership;
- [ ] `&T` and `&mut T` are called **references**, not smart pointers;
- [ ] `Box<T>`, `Rc<T>`, and `Arc<T>` are described as smart pointers when the
      category is useful;
- [ ] mutation through `Cell`, `RefCell`, `Mutex`, or similar types is called
      **interior mutability**;
- [ ] Rust name-resolution paths are distinguished from filesystem paths;
- [ ] absolute and relative paths are named consistently;
- [ ] `Self` is described as a type alias and is not confused with `self`;
- [ ] unsafe code is described using **unsafe block**, **unsafe function**, **unsafe
      trait**, or **unsafe operation**;
- [ ] unsafe code is not described as disabling all compiler checks;
- [ ] *member*, *case*, *class*, and unqualified *object* are avoided.

## Reference sources

- [The Rust Reference - Glossary](https://doc.rust-lang.org/reference/glossary.html)
- [The Rust Reference - Items](https://doc.rust-lang.org/reference/items.html)
- [The Rust Reference - Associated Items](https://doc.rust-lang.org/reference/items/associated-items.html)
- [The Rust Reference - Tuple Types](https://doc.rust-lang.org/reference/types/tuple.html)
- [The Rust Reference - Array Types](https://doc.rust-lang.org/reference/types/array.html)
- [The Rust Reference - Slice Types](https://doc.rust-lang.org/reference/types/slice.html)
- [The Rust Reference - Enumerations](https://doc.rust-lang.org/reference/items/enumerations.html)
- [The Rust Reference - Expressions](https://doc.rust-lang.org/reference/expressions.html)
- [The Rust Reference - Closure Expressions](https://doc.rust-lang.org/reference/expressions/closure-expr.html)
- [The Rust Reference - Statements](https://doc.rust-lang.org/reference/statements.html)
- [The Rust Reference - Functions](https://doc.rust-lang.org/reference/items/functions.html)
- [The Rust Reference - Patterns](https://doc.rust-lang.org/reference/patterns.html)
- [The Rust Reference - Paths](https://doc.rust-lang.org/reference/paths.html)
- [The Rust Reference - Unsafe Blocks](https://doc.rust-lang.org/reference/unsafe-keyword.html)
- [The Rust Reference - Special Types and Traits](https://doc.rust-lang.org/reference/special-types-and-traits.html)
- [The Rust Book - Ownership](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
- [The Rust Book - References and Borrowing](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)
- [The Rust Book - `Rc<T>`](https://doc.rust-lang.org/book/ch15-04-rc.html)
- [The Rust Book - Interior Mutability](https://doc.rust-lang.org/book/ch15-05-interior-mutability.html)
- [Standard library - `Copy`](https://doc.rust-lang.org/std/marker/trait.Copy.html)
- [Standard library - `Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html)
- [The Rust Reference - Trait and Lifetime Bounds](https://doc.rust-lang.org/reference/trait-bounds.html)
- [The Rust Reference - Macros](https://doc.rust-lang.org/reference/macros.html)
- [The Rust Reference - Procedural Macros](https://doc.rust-lang.org/reference/procedural-macros.html)
- [The Rust Book - Validating References with Lifetimes](https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html)
- [The Rust Book - Unrecoverable Errors with `panic!`](https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html)
- [The Rust Book - Recoverable Errors with `Result`](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Standard library - `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Standard library - `str`](https://doc.rust-lang.org/std/primitive.str.html)
- [The Cargo Book - Cargo Targets](https://doc.rust-lang.org/cargo/reference/cargo-targets.html)

## Version and change history

### 1.2.0 - 2026-07-13

Added guidance for AI-assisted glossary maintenance.

Changes included in version 1.2.0:

- documented that the glossary has been developed and refined with generative-AI
  assistance;
- recommended using any capable generative-AI tool for future structured updates;
- added a reusable update prompt with placeholders for additional requests;
- defined mandatory rules for alphabetical ordering, deduplication, synchronized
  terminology, semantic versioning, and changelog maintenance;
- added a human-review workflow and validation checklist;
- added the new maintenance section to the table of contents.

### 1.1.3 - 2026-07-13

Alphabetical-order correction.

Changes included in version 1.1.3:

- moved **Module** before **Move** in the Core terms section to preserve strict
  alphabetical ordering.

### 1.1.2 - 2026-07-13

Scope cleanup.

Changes included in version 1.1.2:

- removed the **Initial slide audit** section because slide-specific remediation does
  not belong in a normative terminology glossary;
- removed the corresponding table-of-contents entry;
- kept the document focused on terminology definitions, usage rules, review checks,
  and references.

### 1.1.1 - 2026-07-13

Structural correction to version 1.1.

Changes included in version 1.1.1:

- removed duplicate **Interior mutability** and **Smart pointer** entries from the
  Quick terminology guide;
- moved the detailed **Interior mutability** and **Smart pointer** definitions into
  their alphabetized locations in Core terms;
- also moved the accidentally misplaced detailed **Borrow / borrowing** and
  **`Clone`** definitions into Core terms;
- retained only concise trainer lookup entries in the Quick terminology guide;
- verified that each of these terms appears exactly once in each intended section.

### 1.1 - 2026-07-13

Terminology expansion focused on ownership, borrowing, memory-management concepts,
name resolution, and Unsafe Rust.

Changes included in version 1.1:

- defined **ownership**, **move**, **borrow**, and **reference**;
- distinguished moves from implicit `Copy` duplication and explicit `Clone`
  operations;
- standardized capitalization and formatting for the `Copy` and `Clone` traits;
- defined **smart pointer** and distinguished smart pointers from references;
- defined **interior mutability** and clarified that it is not ordinary mutable
  borrowing;
- defined **path**, **absolute path**, and **relative path**;
- distinguished the `Self` type alias from the lowercase `self` receiver;
- standardized **unsafe block**, **unsafe function**, **unsafe operation**, and
  **Unsafe Rust** terminology;
- added corresponding avoid-rules, review checks, and official references.

### 1.0 - 2026-07-13

First normative release.

Changes included in version 1.0:

- established the trainer-facing terminology policy and source hierarchy;
- retained the editor-friendly, non-tabular layout;
- added a Markdown table of contents;
- alphabetized the complete Core Terms section;
- defined **trait** and **trait bound** separately;
- defined **closure**, **capture**, and **environment**;
- distinguished **lifetime**, **lifetime annotation**, and **lifetime parameter**;
- distinguished declarative and procedural macros;
- standardized recoverable-error and panic terminology;
- defined slices and clarified their ownership model;
- distinguished `String`, `str`, and `&str`;
- added the **expression statement** term and corrected the `5;` / `()` explanation;
- distinguished shadowing from mutation;
- expanded the review checklist and reference sources;
- retained the initial audit of terminology in the Rust Essentials slides.

Future changes should add a new version entry summarizing normative terminology changes.
