---
name: reviewer
description: Reviews Rust training materials against the shared normative terminology glossary at ../glossary.md. Use when auditing slides, PDFs, labs, exercises, course notes, or other Rust training content for terminology accuracy and consistency.
---

# Instructions

The glossary data to check against is located at `../glossary.md`. Read the complete glossary before reviewing the training material. Treat it as the normative terminology reference.

Review the attached Rust training material against the complete Rust Terminology
Guide provided with this prompt.

Treat the glossary as the normative terminology reference. Check the entire
resource, including prose, headings, captions, code explanations, diagrams,
tables, exercises, and speaker notes when available.

For every terminology issue found:

- identify the page/slide/section;
- quote or identify the problematic wording;
- state which glossary rule it conflicts with;
- provide the preferred replacement wording;
- classify it as **ERROR** if technically misleading or **TERMINOLOGY** if
  primarily a consistency issue.

Do not report wording that already complies with the glossary.

Do not invent issues merely to fill the report.

Preserve compiler output, quoted external documentation, and code verbatim unless
the surrounding explanation is incorrect.

At the end, provide:

- a concise summary of the material's terminology quality;
- a list of all required corrections;
- any cases that need human verification because the glossary does not clearly
  settle them.

When reviewing a PDF or slide deck, inspect the actual rendered pages/slides, not
only extracted text.

## Capitalization and formatting checks

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
- [ ] generic definitions declare **generic parameters** and uses supply
      **generic arguments**;
- [ ] generic arguments are not required to be concrete;
- [ ] a behavioral abstraction is called a **trait**;
- [ ] a restriction such as `T: Display` is called a **trait bound**;
- [ ] outlives relationships such as `'a: 'b` and `T: 'a` are called **lifetime
      bounds**, not trait bounds;
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
- [ ] macro arguments are not assumed to be expressions;
- [ ] expected failures represented by `Result` are called **recoverable errors**;
- [ ] **panic** is not presented as ordinary recoverable error handling;
- [ ] `[T]`, `&[T]`, and `&mut [T]` are described as slice forms rather than owned
      vectors;
- [ ] borrowed slices (`&[T]` and `&mut [T]`) are distinguished from owned boxed
      slices (`Box<[T]>`);
- [ ] `String` and `&str` are distinguished when ownership matters;
- [ ] shadowing is not described as mutation;
- [ ] `if`, `match`, and loops are described as **expressions**;
- [ ] an **expression statement** is described as discarding any value produced by
      its expression;
- [ ] statements are not said to produce values;
- [ ] diverging expressions and blocks are not said to produce a value;
- [ ] the no-tail-expression rule for `()` is qualified by normal completion;
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
- [ ] `Self` is described as an implicit type parameter in traits and an implicit
      type alias in implementations, and is not confused with `self`;
- [ ] unsafe code is described using **unsafe block**, **unsafe function**, **unsafe
      trait**, or **unsafe operation**;
- [ ] unsafe code is not described as disabling all compiler checks;
- [ ] *member*, *case*, *class*, and unqualified *object* are avoided.
