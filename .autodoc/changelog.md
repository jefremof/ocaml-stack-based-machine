# Changelog

Append-only log of changes, newest on top. This first revision backfills the whole
history (`63b33da` … present) since no changelog existed before.

## 2026-07-26 — CI caches the opam switch to skip the dependency recompile
- What: each build/lint job in `.github/workflows/main.yml` now caches its `_opam` switch via `actions/cache@v4`, keyed on `dune-project` + `gullwing.opam` (the fmt lane also on `.ocamlformat`), with a `restore-keys` fallback. On a warm cache `opam install . --deps-only` becomes "Nothing to do" instead of recompiling the Core/ppx tree.
- Why: measured step timing showed `opam install` taking ~118s per lane — the dependency tree building from source. setup-ocaml deliberately does not cache `--deps-only` results (so it always picks up opam-repository updates), so the switch is cached here explicitly. Trade-off: cached dependencies do not pick up new opam-repository versions until the key changes; the key rehashes when deps change, and the `v1` version segment can be bumped to force a refresh. `lint-opam` has no switch cache — plain metadata lint needs no dependencies.
- Affects: `.github/workflows/main.yml` only (CI configuration). Expected warm wall-clock ≈ 1m30s (was ~3m30s after parallelization, ~7m49s before).
- By: Efremov Mark

## 2026-07-26 — CI parallelized; opam-dune-lint recompile removed
- What: `.github/workflows/main.yml` split back into four parallel jobs (`build-test`, `lint-fmt`, `lint-doc`, `lint-opam`), each setting up OCaml from the same pinned/cached switch, so wall-clock is the slowest single lane instead of the sum of all steps. The `lint-opam` job no longer uses `ocaml/setup-ocaml/lint-opam@v3` — that action installs `opam-dune-lint`, which pins an older dune and forces a downgrade + recompile of ~67 packages (~3m20s per run, the single biggest step). Replaced with a plain `opam lint gullwing.opam`.
- Why: measured step timing on the prior single-job run showed ~7m49s total dominated by `lint-opam` (200s) and the sequential lint chain; the compiler setup itself was already cached (~64s). Removing the recompile and hiding the lints under the build lane targets ~3.5m warm.
- Affects: `.github/workflows/main.yml` only (CI configuration). Trade-off: `opam lint` validates package metadata but drops opam-dune-lint's dune/opam dependency-consistency check, which is low value here since the opam file is generated from `dune-project` (`generate_opam_files`).
- By: Efremov Mark

## 2026-07-26 — usage examples in README
- What: added a `Примеры работы` section to `README.md` — assembler syntax (labels, opcodes, `+`=`PLUS`, `main` entry), assembling via `Pipeline.pipe` (`[0xDA9AD; 0x5; 0x3]`), running with a trace via `pipe_main` → `Util.fill_rest` → `Machine.step`, and `dune build` / `dune test`. Snippets and outputs were captured from a live `dune utop` run; the trace ends at `HLT` (the caught `Machine.Halt` prints nothing).
- Why: the README had no runnable examples of how to write, assemble, and execute a program.
- Affects: `README.md` (docs only).
- By: Efremov Mark

## 2026-07-26 — instruction-format diagram in README
- What: added `docs/instruction-format.svg` — an original redraw of the Gullwing instruction word layout (six 5-bit slots S0–S5 + 2 unused bits; data words occupy a full word), and embedded it in a new `Формат инструкций` section of `README.md` that ties it to `word_to_batch`. The SVG is our own drawing, marked "адаптировано из … Fig. 6.3", per the attribution policy.
- Why: the packing of instructions into a machine word is the core idea the code implements; a diagram makes it legible.
- Affects: `README.md`, `docs/instruction-format.svg` (docs only).
- By: Efremov Mark

## 2026-07-26 — Russian source attribution in README; .DS_Store ignored
- What: `README.md` gained an `Источники и атрибуция` section — a full citation of Charles Eric LaForest's thesis *Second-Generation Stack Computer Architecture* (University of Waterloo, 2007) that the machine is based on, plus a short Russian policy for how the work's material is reused (own-words explanations, redrawn/re-derived figures marked "адаптировано из", short attributed quotes). `.gitignore` now ignores `.DS_Store`.
- Why: the thesis' own terms require that quotations and derived information be acknowledged; the citation is that acknowledgment. Verbatim reproduction of its figures/tables is avoided in favour of redrawing, which its terms permit.
- Affects: `README.md`, `.gitignore` (docs and repo hygiene only; no source or runtime change).
- By: Efremov Mark

## 2026-07-26 — CI consolidated into one job; compiler set up once
- What: `.github/workflows/main.yml` collapsed the four jobs (`build-and-test`, `lint-doc`, `lint-fmt`, `lint-opam`) — each of which stood up its own OCaml via `ocaml/setup-ocaml@v3` — into a single `build-test-lint` job that sets up OCaml once and runs build, test, and the three lints as steps. The compiler is pinned to an exact `"5.2.1"` (was a floating `4`) so the cache key stays stable, and `dune-cache: true` is enabled. Lint steps carry `if: ${{ !cancelled() }}` so they still report independently when an earlier step fails.
- Why: setup ran four times per CI run, each building the OCaml compiler from source and installing the Core/ppx dependency tree — the bulk of the ~20-minute wall time. Setting up once, on a stable pinned cache key, removes the 4× duplication; warm runs skip the compiler build entirely.
- Affects: `.github/workflows/main.yml` only (CI configuration; no source or runtime change). `5.2.1` matches the local toolchain the project already builds on.
- By: Efremov Mark

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
