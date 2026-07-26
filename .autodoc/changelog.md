# Changelog

Append-only log of changes, newest on top. This first revision backfills the whole
history (`63b33da` … present) since no changelog existed before.

## 2026-07-26 — autodoc tooling added; CI triggers scoped to main and develop
- What:
  - CI trigger scoping (`aa306ff`): `.github/workflows/main.yml` no longer fires on the bare `on: [push, pull_request]` (which ran on every branch). `push` and `pull_request` are now branch-filtered to `main` and `develop`.
  - autodoc tooling (this commit): added `.claude/hooks/autodoc-scope.js`, a `PreToolUse` hook on Bash wired through `.claude/settings.json`. It injects the actual contents of a push (`git log HEAD --not --remotes`) next to the core plugin's autodoc gate, which says "record ONE entry summarizing this whole update" without defining the range. The hook never denies — it only adds context: the commit list over the gate's own range, capped at 25, with retargeting to the repo actually being pushed (`-C` / `--git-dir` / leading `cd`), a nested-shell guard, and multi-author detection. Adapted from the facade repo's version with its repo-specific `pulse/` / `market-parser/` references removed and the shared-working-tree wording genericized.
  - This changelog file (`.autodoc/changelog.md`) and its `.autodoc/index.md` were created in the same commit, backfilling the prior history.
- Why: pushes were documenting only the current session's work; the hook makes the whole push range visible at the moment the entry is written.
- Affects: `.github/workflows/main.yml` (CI trigger scope), `.claude/` (agent tooling only, no runtime effect on the machine), `.autodoc/` (docs).
- By: Efremov Mark

## 2025-04-14 — packaging metadata fixed; GitHub Actions CI added
- What:
  - GitHub Actions workflow added (`42430e8`): `ocaml/setup-ocaml@v3` on ubuntu, a `build-and-test` job (`opam install --deps-only --with-test`, `dune build`, `dune runtest`) plus three lint jobs (`lint-doc`, `lint-fmt`, `lint-opam`).
  - opam/dune metadata corrected across `bb6ebb9` (license field), `62a1b63` and `94488ef` (dependencies), with a `main`-branch merge (`6e366bc`) in between. `dune-project` generates `gullwing.opam`; dependencies settled on Core plus the ppx suite (`ppx_deriving`, `ppx_fields_conv`, `ppx_assert`, `ppx_sexp_conv`, `ppx_inline_test`, `ppx_jane`, `sexplib`).
- Affects: `.github/workflows/main.yml`, `dune-project`, `gullwing.opam`.
- By: Efremov Mark

## 2025-04-14 — basic implementation of the stack machine
- What (`84b63ef`): first working implementation of the Gullwing stack-based machine.
  - `lib/isa.ml` / `.mli` — the instruction set: arithmetic/stack opcodes (`+`, `2/`, `2*`), data- and return-stack transfers (`>R`/`R>`, `>A`/`A>`), memory access with auto-increment (`@A+`, `@R+`, `@A`, `!A+`, `!R+`, `!A`), and control flow (`CALL`, `JMP`, `RET`, `HLT`), with `[@@deriving]` machinery for encoding/showing.
  - `lib/machine.ml` / `.mli` — the machine `state` (memory, data stack `ds`, return stack `rs`, `pc`, `mar`, instruction-batch register `isr`, `top`, `a`) and the `step` interpreter, dispatching each opcode.
  - `lib/pipeline.ml` / `.mli` — the assembler: `parse_source` → `group_instructions` (packing opcodes into batched words) → `resolve_labels` → `encode_instructions`, plus `pipe` / `verbose_pipe` entry points.
  - `lib/util.ml` — shared list/string helpers.
  - `bin/main.ml` — a runnable driver executing a triangle-number program.
  - `test/` — execution tests with `triangle`, `triangle_iterative`, and `triangle_tailrec` sample programs.
- Affects: `lib/`, `bin/`, `test/`, `README.md`.
- By: Efremov Mark

## 2025-04-11 — project scaffolding
- What: repository initialized (`63b33da`, MIT `LICENSE`) and set up as a dune project (`5f6c6b1`): `dune-project` (lang dune 3.17, `generate_opam_files`), initial `gullwing.opam`, and `.ocamlformat`.
- Affects: `dune-project`, `gullwing.opam`, `.ocamlformat`, `LICENSE`.
- By: Efremov Mark
