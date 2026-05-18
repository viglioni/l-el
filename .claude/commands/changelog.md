---
description: Append entries to * Unreleased in changelog.org based on changes since the last tag
argument-hint: [free-form instructions]
---

# Input

The user typed:

$ARGUMENTS

Treat as free-form instructions (e.g. "ignore the docs reshuffle", "group all release-script changes as Internal", "this was a security fix").  No required argument.

# Goal

Fill the `* Unreleased` section of `changelog.org` with bullets describing everything that has happened since the last release tag and isn't already documented there.

# Phase 1 — gather (parallel)

- `git describe --tags --abbrev=0` → CURRENT-TAG.
- `git log CURRENT-TAG..HEAD --pretty=format:"%h %s"` → committed changes since tag.
- `git diff CURRENT-TAG..HEAD` (or per-file diffs as needed) → committed file changes.
- `git status --porcelain` and `git diff HEAD` → uncommitted changes (these count too).
- Read `changelog.org` and extract the current `* Unreleased` block (the bullets already there).

# Phase 2 — analyse (single agent, no sub-agents)

1. Merge committed + uncommitted into a unified list of changes since CURRENT-TAG.  Group changes that belong to the same logical feature/fix (one bullet per concept, not one bullet per file).
2. Subtract anything already in `* Unreleased`.  Do not propose duplicates.
3. Classify each remaining change:
   - **Added** — new user-visible feature, function, file, type, command.
   - **Changed** — behaviour or signature change to existing public surface.
   - **Fixed** — bug fix (a user could observe the bug).
   - **Removed** — public symbol/feature deleted.
   - **Internal** — refactors, test infra, CI, scripts, dev tooling, build changes.  Things users don't see.
   - **Breaking Changes** — anything that forces a downstream caller to edit their code.
4. Draft each bullet:
   - One line, max ~120 chars.
   - org-style: `=foo=` for code/symbol references.
   - **No implementation details.**  Describe the user-visible effect.
   - Bad: "Refactored `l-release--rewrite-l-version` to use a tighter regex with `[ \t\n\r]` for portability."
   - Good: "Release script now bumps the smoke-test assertion alongside `l-version`."
   - If a change is ambiguous between two categories, pick the one closer to user perception and flag it in your proposal so the user can override.
   - Skip purely mechanical churn (version bumps, `;; updated-at:` housekeeping).

# Phase 3 — merging into `* Unreleased`

- If `** Category` already exists under `* Unreleased`, **append** new bullets to it.  Never duplicate the subsection header.
- If it doesn't exist, create it.  Use this order (matching released sections): Breaking Changes, Added, Changed, Removed, Fixed, Internal.

# Approval flow

1. Print the **proposed final `* Unreleased` block** — existing bullets plus new ones, with new bullets marked `[new]`.
2. End with: "Reply 'apply' to write, or list which entries to skip / edit."
3. **Wait for approval.**  Do NOT call Edit/Write yet.
4. After approval, Edit `changelog.org`.  Print a one-line confirmation.

# Style reference

Read `changelog.org`'s existing released entries to match phrasing, tone, and length.  Bullets are concise — the changelog is a fast context source for humans and LLMs, not a design document.
