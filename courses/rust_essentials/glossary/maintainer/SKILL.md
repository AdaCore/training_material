---
name: maintainer
description: Maintains the shared normative Rust terminology glossary at ../glossary.md. Use when adding, correcting, reorganizing, or validating Rust terms and definitions, or when a requested terminology change must be applied consistently across the glossary.
---

# Instructions

The glossary data to update is located at `../glossary.md`. Read the complete file before making any change. Treat it as a normative, trainer-facing Rust terminology glossary.

Preserve the glossary's scope, tone, structure, and text-editor-friendly Markdown style. Apply requested additions, corrections, or editorial directives to the complete glossary rather than returning isolated fragments.

## Architecture note

This Agent Skills refactor deliberately keeps `../glossary.md` data-only. The original file's AI prompts, maintenance meta-text, table of contents, review checklist, reference list, version metadata, and change history are not part of the data file. The original embedded maintenance rules are preserved below in full. Where a legacy rule refers to a section that was intentionally moved out of `../glossary.md`, preserve the rule's intent without reintroducing prompt/meta content into the data file. Use version-control history and the human review workflow for revision traceability.

## Original maintenance rules

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
12. Update the date-based version using `YYYY.MM.DD.NN`:
    - `YYYY.MM.DD` is the date of the revision;
    - `NN` is a two-digit sequence starting at `01` for the first revision made on
      that date;
    - increment `NN` for each additional revision made on the same date.
13. Add an entry at the top of Version and change history that clearly
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

When applying the rules above in this refactored layout:

- `Quick terminology guide`, `Core terms`, `Terms to avoid or qualify`, and `Constructor terminology` remain in `../glossary.md`.
- The training-material review checklist and capitalization/formatting review rules are maintained in `../reviewer/SKILL.md`.
- The source hierarchy and official reference list are maintained below in this skill.
- Do not add AI prompts, maintenance workflow text, or change-history prose back into `../glossary.md`.

## Recommended update workflow

1. Give the AI tool the complete current version of this file.
2. Add the specific requested changes or editorial directives.
3. Ask the tool to update the whole document rather than returning isolated snippets.
4. Review the generated diff.
5. Verify terminology against official Rust documentation when semantics changed.
6. Commit the updated glossary together with its version and changelog entry.

For this refactored layout, step 1 means reading `../glossary.md`; review the generated diff before accepting it.

## Human review checklist

Before accepting an AI-generated revision, confirm that:

- the requested changes were actually applied;
- no unrelated definitions disappeared;
- no detailed entry was accidentally inserted into the Quick guide;
- every Core term remains in strict alphabetical order;
- similar concepts remain distinct rather than being merged incorrectly;
- the date-based version matches the revision date and sequence;
- the newest changelog entry accurately describes the diff;
- all technical wording remains defensible against official Rust documentation.

## Source hierarchy

Use the following sources in this order:

1. **The Rust Reference** for language terminology.
2. **The Rust Programming Language** for pedagogical wording.
3. **The Cargo Book** for Cargo concepts such as packages and targets.
4. Standard-library documentation for API-specific terminology.

When a familiar term from another language conflicts with Rust terminology, prefer
the Rust term.

## Official reference sources

- [The Rust Reference - Glossary](https://doc.rust-lang.org/reference/glossary.html)
- [The Rust Reference - Items](https://doc.rust-lang.org/reference/items.html)
- [The Rust Reference - Associated Items](https://doc.rust-lang.org/reference/items/associated-items.html)
- [The Rust Reference - Traits](https://doc.rust-lang.org/reference/items/traits.html)
- [The Rust Reference - Generic Parameters](https://doc.rust-lang.org/reference/items/generics.html)
- [The Rust Reference - Type Parameters](https://doc.rust-lang.org/reference/types/parameters.html)
- [The Rust Reference - Tuple Types](https://doc.rust-lang.org/reference/types/tuple.html)
- [The Rust Reference - Array Types](https://doc.rust-lang.org/reference/types/array.html)
- [The Rust Reference - Slice Types](https://doc.rust-lang.org/reference/types/slice.html)
- [The Rust Reference - Enumerations](https://doc.rust-lang.org/reference/items/enumerations.html)
- [The Rust Reference - Expressions](https://doc.rust-lang.org/reference/expressions.html)
- [The Rust Reference - Block Expressions](https://doc.rust-lang.org/reference/expressions/block-expr.html)
- [The Rust Reference - Never Type](https://doc.rust-lang.org/reference/types/never.html)
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
- [The Rust Reference - Macros by Example](https://doc.rust-lang.org/reference/macros-by-example.html)
- [The Rust Reference - Procedural Macros](https://doc.rust-lang.org/reference/procedural-macros.html)
- [The Rust Book - Validating References with Lifetimes](https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html)
- [The Rust Book - Unrecoverable Errors with `panic!`](https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html)
- [The Rust Book - Recoverable Errors with `Result`](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Standard library - `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Standard library - `str`](https://doc.rust-lang.org/std/primitive.str.html)
- [Standard library - `matches!`](https://doc.rust-lang.org/std/macro.matches.html)
- [Standard library - `Self`](https://doc.rust-lang.org/std/keyword.SelfTy.html)
- [The Cargo Book - Cargo Targets](https://doc.rust-lang.org/cargo/reference/cargo-targets.html)

Supplementary terminology clarification, subordinate to the source hierarchy above:

- [Rust FLS - Glossary (`Self`)](https://rust-lang.github.io/fls/glossary.html)
