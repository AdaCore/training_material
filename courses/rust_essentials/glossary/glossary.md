# Rust Terminology Guide for Training Material

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

Use **argument** for an expression supplied in a function, method, or closure
call.

Example:
- `5` in `f(5)`

### Generic parameter and generic argument

Use **generic parameter** for a placeholder declared by a generic definition.

Example:
- `T` in `struct Box<T>`

Use **generic argument** for a type, lifetime, or const supplied at a use site.

Example:
- `u8` in `Vec<u8>`

A generic argument may itself refer to a generic parameter.

### Trait and trait bound

Use **trait** for the interface or behavioral abstraction itself.

Example:
- `Display`

Use **trait bound** for a constraint involving a trait, such as requiring a type
to implement it.

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

Use **lifetime bound** for an outlives relationship, such as `'a: 'b` or `T: 'a`,
not for a trait requirement.

Do not imply that writing an annotation extends a reference's lifetime.

### Macro terminology

Use **declarative macro** for a `macro_rules!` macro.

Use **procedural macro** for:
- custom derive macros;
- attribute-like macros;
- function-like procedural macros.

Do not imply that every macro invocation uses `!`: custom derives and
attribute-like procedural macros use attributes.

Macro input follows the macro's syntax; it is not necessarily a list of
expressions.

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
- `&mut [T]` is a mutable borrowed slice;
- `Box<[T]>` is an owned boxed slice.

A borrowed slice (`&[T]` or `&mut [T]`) does not own its elements.

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

Use **`Self`** for the current or implementing type: an implicit type parameter
in a trait, and an implicit type alias in an `impl`.

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

Use for Rust code that evaluates to a value or diverges without producing one.

Examples:
- `if` expression
- `match` expression
- block expression
- function call expression

### Expression statement

Use for an expression placed in statement position; any value it produces is
discarded.

Example:
- `5;`

Do not say that the statement itself evaluates to `()`. More precisely:

- `{ 5 }` evaluates to `5`;
- `{ 5; }` discards `5` and evaluates to `()` because it has no tail expression
  and completes normally.

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

An expression supplied in a function, method, or closure call.

```rust
draw(point, 3);
```

`point` and `3` are arguments.

Do not use *argument* for the names declared in a function signature.

Macro arguments follow the macro's input syntax rather than a universal
expression-based argument list. Depending on the macro, its input may include
expressions, patterns, types, items, or other token sequences.

For example, in `matches!(value, Some(_))`, `value` is an expression and `Some(_)`
is a pattern.

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

A block with no tail expression evaluates to `()` if it completes normally. A
diverging block does not produce a value.

```rust
fn stop() -> ! {
    loop {};
}
```

`loop {};` is an expression statement, so this function-body block has no tail
expression. The block diverges instead of evaluating to `()`. The `!` return type
indicates that `stop` never returns normally.

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

Rust syntax whose evaluation may produce a value or diverge, and may have side
effects.

Examples include:

- literals and paths;
- function and method calls;
- blocks;
- `if`;
- `match`;
- `loop`, `while`, and `for`.

Use **if expression**, **match expression**, and **loop expression**.

An expression **diverges** when its evaluation does not complete normally and
therefore produces no value, for example `return`, `panic!()`, or a `loop` without
a reachable `break`.

When an expression's result is ignored, say that the expression is *used in
statement position* rather than renaming it an “if statement” or “match statement.”

### Expression statement

A statement formed from an expression, discarding its value if evaluation
completes normally.

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
produce values; a containing block may produce a value if it completes normally.

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

A type, lifetime, or const supplied at a use site of a generic construct.

```rust
Vec::<u8>::new()
```

`u8` is a generic type argument.

A generic argument need not be concrete; it may itself refer to a generic
parameter:

```rust
fn wrap<T>(value: T) -> Option<T> {
    Some(value)
}
```

`T` is declared as a type parameter of `wrap`. Its occurrence in `Option<T>`
supplies a type argument to `Option`. The distinction is declaration versus use,
not generic versus concrete.

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

`Self`, written with a capital `S`, denotes the current or implementing type.

In an inherent or trait implementation, it acts as an implicit type alias for
the implementing type. For example, in an inherent implementation:

```rust
impl Buffer {
    fn new() -> Self {
        Self { data: Vec::new() }
    }
}
```

`Self` denotes `Buffer`.

In a trait definition, `Self` is an implicit type parameter representing the
type that implements the trait. It is not a type alias for one particular
implementing type.

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

A semicolon commonly forms an expression statement; any value the expression
produces is discarded.

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

A bound involving a trait, normally requiring a type to implement that trait.

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

Lifetime bounds, such as `'a: 'b` and `T: 'a`, express outlives relationships
rather than trait implementation requirements. Lifetimes do not implement traits.

The special bound `?Sized` relaxes the implicit `Sized` trait bound; it does not
require a type to be unsized.

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

### Generic argument as necessarily concrete

Avoid this restriction.

A generic argument can itself refer to a generic parameter. Distinguish a
parameter declaration from an argument supplied at a use site.

### Trait when referring to a restriction

Prefer:
- trait bound, for a restriction involving a trait

Example:
- `T: Display` is a trait bound;
- `Display` is the trait.

For an outlives requirement such as `'a: 'b` or `T: 'a`, use **lifetime bound**,
not **trait bound**.

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

### Macro arguments are always expressions

Avoid this claim.

Macro arguments follow the macro's input syntax, which may accept patterns,
types, items, or other token sequences as well as expressions.

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
- borrowed slice, for `&[T]` or `&mut [T]`;
- owned boxed slice, for `Box<[T]>`;
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
- any value produced by the expression is discarded;
- a block with no tail expression evaluates to `()` if it completes normally;
- a diverging block does not produce a value.

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
- **`Self`** for the current or implementing type;
- **`self`** for the method receiver value or parameter.

In traits, describe `Self` as an implicit type parameter, not as a type alias.

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
