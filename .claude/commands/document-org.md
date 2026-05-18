---
description: Detect drift in .org documentation vs the public Elisp API and propose updates
argument-hint: [optional .org file] [+ free-form instructions]
---

# Input

The user typed:

$ARGUMENTS

Treat this as free-form prose.  Extract:

- **Target file(s)** (optional) — a path ending in `.org`.  If present, scan only those.  If absent, scan every `.org` file in the repo (excluding `.cask/`, `.git/`, `straight/`, `node_modules/`).
- **Extra instructions** — pass through verbatim to every sub-agent.

# Goal

`.org` files are the documentation users read.  They drift from the code over time.  This command finds and fixes that drift.

# Phase 1 — gather

Run these in parallel where possible:

1. List `.org` files (Glob `**/*.org` minus the excludes above).
2. Build the project's **public API surface** from `.el` source: every top-level `defun` / `defmacro` matching `l-[a-z]` (single-dash; exclude `l--`).  For each: name, arglist, current docstring, source file.

# Phase 2 — sub-agents

Spawn **one sub-agent per .org file** in parallel (single message, multiple Agent tool calls).

Each sub-agent's prompt must include:

- Absolute path to its `.org` file.
- Full content of that file (paste it in).
- The public API surface from Phase 1 (name + arglist + docstring + file).
- The user's extra instructions, verbatim.
- This instruction block, verbatim:

  > Compare the .org content against the API surface.  Identify drift:
  >
  > - **Missing entries:** public API symbols not mentioned.  Only flag this for files whose purpose is to document the API (e.g. `docs/api.org`, `readme.org`); skip files that clearly aren't API references.
  > - **Stale entries:** symbols whose described behaviour, signature, or example no longer matches the current `.el` docstring or arglist.
  > - **Removed symbols:** symbols documented here that no longer exist in `.el`.
  > - **Broken examples:** code blocks that reference removed functions or wrong signatures.
  >
  > For `api.org` specifically: every public API entry must include at least one **example** of usage.  Add one if missing.
  >
  > Return a structured proposal in this format:
  >
  >     ## File: <absolute path>
  >     ### Add: <symbol>
  >     <proposed org-mode section>
  >     ### Update: <symbol>
  >     Reason: <one-line reason>
  >     Replace:
  >       <old lines>
  >     With:
  >       <new lines>
  >     ### Remove: <symbol>
  >     Reason: <one-line reason>
  >
  > If the file needs no changes, return: "## File: <path>\nNo changes needed."
  >
  > **Do NOT call Edit or Write.**  Return plain text only.
  > Match the existing tone and structure of the file (read its headings before drafting).
  > Use `=code=` for inline literals and `#+begin_src emacs-lisp ... #+end_src` for examples.

# Approval flow

1. Collect every sub-agent's proposal.
2. Print one consolidated report, grouped by `.org` file.  Files with no drift get a single "no changes needed" line.
3. End with: "Reply 'apply' to write all, or list which items to skip."
4. **Wait for approval.**  Do NOT call Edit/Write yet.
5. After approval, apply with Edit.  Print one confirmation line per file modified.
