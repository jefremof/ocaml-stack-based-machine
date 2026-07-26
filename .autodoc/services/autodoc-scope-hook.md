# autodoc-scope hook

`.claude/hooks/autodoc-scope.js` — a `PreToolUse` hook on the `Bash` tool, wired via
`.claude/settings.json`. Adapted from the facade repo, with that repo's `pulse/` /
`market-parser/` specifics removed and the shared-working-tree wording genericized.

## What it does
The core plugin's autodoc gate denies `git push` with "record ONE entry summarizing
this whole update" but never defines what "update" spans, and an agent's only context
is its own session. This hook injects the missing facts next to the gate: the commit
list over the same range the gate uses, and an explicit instruction that the entry
must cover the whole range, not just this session's work.

It **never denies** — blocking stays the gate's job; the hook only adds context via
`hookSpecificOutput.additionalContext`.

## How it resolves the push
- Detects a real push by testing each `&& || ; |`-separated segment for *starting*
  with `git … push` (allowing global git options), so `git log --grep=push` and
  commit messages containing the word do not trigger it.
- Bails out silently on a nested shell (`bash -c "…"`), whose inner `cd` it cannot see.
- Retargets to the repo actually being pushed: the pushing segment's own
  `-C` / `--git-dir`, else the last bare `cd` before it; `--git-dir=X/.git` resolves
  to the work tree. If the target can't be resolved, it stays silent rather than guess.

## Range and caps
- Range: `HEAD --not --remotes` — commits on HEAD absent from every remote-tracking
  branch (no upstream needed). Exits silently when empty (nothing unpushed).
- Commit list capped at 25 with a remainder line; authors computed over the full range
  so a multi-author push past the cap is not mistaken for single-author.

## Known limits
- Only surfaces **unpushed** commits — already-pushed history is out of range.
- Segment splitting can mis-cut inside quotes; the gate fires on those cases too, so
  the cost is at most a stray block of context, never a wrong denial.
