# Rust Terminology Agent Skills

This folder is structured using the [Agent Skills](https://agentskills.io/) standard so it is ready for AI agents. The `maintainer/` and `reviewer/` directories are separate skills, each with its own `SKILL.md`, while `glossary.md` is the shared Rust terminology data source. Keep the complete `glossary/` directory tree together so both skills can resolve the relative path `../glossary.md`.

## Directory layout

```text
/glossary/
├── README.md
├── maintainer/
│   └── SKILL.md
├── reviewer/
│   └── SKILL.md
└── glossary.md
```

## Using the skills with an Agent Skills-compatible agent

Give the agent access to the complete `glossary/` directory. Select or invoke the `maintainer` skill when the task is to add, correct, or reorganize glossary terminology. Select or invoke the `reviewer` skill when the task is to audit Rust training material against the glossary.

Each skill's YAML `name` matches its directory name, and its `description` explains when the skill should be activated. The skill then reads the shared glossary from `../glossary.md`.

## Manual use with a standard web chatbot

You do not need Agent Skills support to use these files manually.

### Maintain or update the glossary

1. Open `maintainer/SKILL.md` and copy its instructions into a new chatbot conversation. You may copy the whole file; the YAML frontmatter is harmless in a normal chat.
2. Upload `glossary.md` to the same conversation.
3. Add your requested terminology additions, corrections, or editorial changes in your own message.
4. Ask the chatbot to apply the maintainer instructions to the uploaded glossary and return the complete updated `glossary.md`, not isolated snippets.
5. Review the diff carefully, especially technical Rust claims, alphabetical ordering, duplicates, and formatting, before committing the change.

### Review training material against the glossary

1. Open `reviewer/SKILL.md` and copy its instructions into a new chatbot conversation.
2. Upload `glossary.md`.
3. Upload the Rust training material you want to review, such as a PDF, slide deck, lab, exercise, or source file.
4. Ask the chatbot to review the material using the copied reviewer instructions and the uploaded glossary as the normative terminology reference.
5. For PDFs or slide decks, use a chatbot that can inspect the rendered pages/slides, not only extracted text.
6. Review the reported `ERROR` and `TERMINOLOGY` findings, apply required corrections, and manually verify any case the glossary does not clearly settle.

## File roles

- `glossary.md` contains the shared Rust terminology data only: the quick guide, detailed core definitions, terms to avoid or qualify, and constructor terminology.
- `maintainer/SKILL.md` contains the glossary-maintenance workflow, all original maintenance constraints, validation checks, source hierarchy, and official references.
- `reviewer/SKILL.md` contains the training-material review workflow, finding format, capitalization rules, and the review checklist.
- `README.md` explains the layout and manual usage.
