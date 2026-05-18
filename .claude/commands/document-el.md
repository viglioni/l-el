---
description: Document undocumented or stale Emacs Lisp code (a file or a symbol)
argument-hint: <file path or symbol name> [+ free-form instructions]
---

# Input

The user typed:

$ARGUMENTS

Treat this as free-form prose.  Extract from it:

- **Target(s)** — a path ending in `.el`, OR a symbol name (function/macro/const/var).  More than one is allowed (e.g. "the file x function y").
- **Extra instructions** — anything else (e.g. "dont forget the FOO behavior", "skip private helpers").  Pass these through verbatim to every sub-agent you spawn.

If no target can be identified, ask the user which file/symbol to document.  Don't guess.

# Project conventions (read before drafting)

- Scope: any `.el` file under this repo (`l.el`, `lib/**`, `test/**`, `scripts/**`).
- Public vs internal naming:
  - `l-foo` (single dash) → **public API**.  Docstring is **user-focused**: what it does, when to use it, what each parameter means, plus at least one concrete example.  Avoid implementation talk.
  - `l--foo` (double dash) → **internal**.  Terse but complete: what it does, why it exists, a sentence of implementation if non-obvious.  Examples optional.
- Same cut applies to `defmacro`, `defconst`, `defvar`, `defcustom`.
- Preserve existing `since:` / `updated-at:` annotations.  Never strip them.

# When the target is a file

1. Read the file.
2. Inspect `;;; Commentary:`:
   - Missing/empty → needs new commentary (file's purpose, main entry points, non-obvious constraints).
   - Stale (claims behaviour the code no longer has, or omits major features) → needs rewrite.
   - Accurate and complete → leave alone.
3. Walk every top-level `defun` / `defmacro` / `defconst` / `defvar` / `defcustom` / `cl-defstruct` / `cl-defmethod`.  For each:
   - **No docstring** → needs one.
   - **Stale docstring** (mentions removed params, missing newly-added params, contradicts current code) → needs rewrite.
   - **Accurate, not too terse** → leave alone.  Don't churn for cosmetic reasons.
4. Build a **work-unit list**: one entry per symbol/commentary that needs work.

# When the target is a single symbol

Find its defining file via Grep (`defun SYMBOL`, etc.).  Build a one-item list.

# Spawning sub-agents

- **2+ work units** → spawn one sub-agent per work unit in parallel (single message, multiple Agent tool calls).
- **1 work unit** → draft inline.  No sub-agent.

Each sub-agent's prompt must include:

- Absolute file path.
- Exact symbol name (or "FILE COMMENTARY" for the file header).
- Classification (API / internal / test / script).
- Current state ("no docstring" / "stale, here's what's wrong: ...").
- The source code of the symbol (or the full file for a commentary task), pasted in.
- The "Project conventions" section above, verbatim.
- The user's extra instructions, verbatim.
- This rule, verbatim: **Do NOT call Edit or Write.  Return the proposed docstring (or commentary block) as plain text only.**

# Approval flow

1. Collect every proposal.
2. Print a single summary to the user, grouped by file.  For each unit:
   - Symbol + classification
   - Why it needs work (missing / stale + how)
   - Proposed new docstring (or commentary) in a code fence
3. End with: "Reply 'apply' to write all, or list which to skip (e.g. 'apply except lfoo, lbar')."
4. **Wait for the user's approval.**  Do NOT call Edit/Write yet.
5. After approval, use Edit to apply only the approved items.  Show one-line confirmation per file.

# Style notes

- Read 2-3 existing docstrings in the same file for tone/length.
- For API examples in docstrings:

      Example:
        (l-foo 1 2) ; => 3

- Don't write filler ("This function is used to...").  Start with the verb.
- Capitalise parameter references inside docstrings (Emacs Lisp convention).
