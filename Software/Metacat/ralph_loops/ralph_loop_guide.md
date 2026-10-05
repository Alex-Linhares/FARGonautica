# Ralph Loop Setup Guide

The **Fresh Context Pattern** (aka "Ralph Loop") prevents context window degradation by spawning fresh agent sessions for each task while maintaining state through file-based persistence.

## Why Ralph Loops?

As sessions progress, context accumulates and performance degrades:

| Context Usage | Performance Impact | Symptoms |
|--------------|-------------------|----------|
| 0-50% | Baseline | Fast, accurate |
| 50-70% | 10-15% degradation | Slower, occasional repetition |
| 70-85% | 20-30% degradation | Confused about prior decisions |
| 85%+ | 40-50% degradation | Tool hallucination, missed requirements |

Ralph Loops solve this by resetting to 0% context at each iteration while preserving state in files.

---

## Folder Structure

```
RalphLoops/
├── RALPH_LOOP_GUIDE.md    # This file
└── loop0001/              # First loop instance
    ├── TASK.md            # Immutable goal definition (read-only)
    ├── PROGRESS.md        # Mutable state (updated each iteration)
    ├── iterations.md      # Ordered work items (replaces near_misses.md)
    └── [optional files]   # Additional context files
```

> **Naming convention**: The work-item list is called `iterations.md` (not `near_misses.md`).
> This makes loops general — they can track architectural tasks, problem-solving runs, or anything else.
> `loop.py` falls back to `near_misses.md` for backward compatibility with older loops.

Each new loop gets its own folder: `loop0001`, `loop0002`, etc.

---

## File Templates

### TASK.md (Immutable)

```markdown
# TASK: [Brief Title]

## Philosophy
[Key principles for this work - guide agent behavior]

## Current Focus
[What we're trying to achieve]

## Target Problems (in order)
[List of specific items to complete]

## Acceptance Criteria (per item)
- [ ] Criterion 1
- [ ] Criterion 2
- [ ] No regressions

## Completion Conditions
An item is DONE when either:
1. **Solved**: Meets all criteria, OR
2. **Blocked**: Documented with specific blockers

## Context
- **Key files**: [relevant files]
- **Testing**: [how to verify]
- **Constraints**: [what must not break]

## Important Notes
[Any critical guidance]
```

### PROGRESS.md (Mutable)

```markdown
# Progress Log

## Ralph Loop [NNNN] Status
- **Started**: YYYY-MM-DD
- **Target**: [N] items
- **Current**: 0/[N] SOLVED

---

## Iteration 1 — YYYY-MM-DD HH:MM
### Completed
- [What was done]

### Blockers
- [Any issues encountered]

### Next
- [What the next iteration should do]

---
```

Each iteration appends a new section. When all tasks complete:

```markdown
## Iteration N — YYYY-MM-DD HH:MM
### Completed
- All tasks verified

LOOP_COMPLETE
```

---

## Running a Ralph Loop

### Option 1: Manual (Recommended for Complex Work)

1. **Create the loop folder:**
   ```bash
   mkdir -p RalphLoops/loop0001
   ```

2. **Create TASK.md** with your goals

3. **Create empty PROGRESS.md:**
   ```markdown
   # Progress Log
   
   ## Ralph Loop 0001 Status
   - **Started**: 2026-02-09
   - **Current**: 0/N SOLVED
   
   ---
   ```

4. **Run iterations manually:**
   - Start fresh agent session
   - Agent reads TASK.md + PROGRESS.md
   - Agent works on ONE item
   - Agent updates PROGRESS.md
   - Agent commits changes
   - Repeat until LOOP_COMPLETE

### Option 2: Automated Script (`loop.py`)

Each loop folder contains a `loop.py` script that handles the full iteration cycle: spawning a fresh Claude session, running regression tests, auto-fixing regressions (up to 3 attempts), and committing results.

```bash
# Run from the loop folder:
cd RalphLoops/loop0007

# Run 10 iterations (default), resuming from current PROGRESS.md
python3 loop.py

# Run 60 iterations
python3 loop.py 60

# Run 60 iterations with a fresh start (resets PROGRESS.md)
python3 loop.py 60 --fresh

# Custom TASK.md and PROGRESS.md paths
python3 loop.py 20 /path/to/TASK.md /path/to/PROGRESS.md
```

**What `loop.py` does each iteration:**
1. Reads `TASK.md` + `PROGRESS.md` and finds the next task from `iterations.md` (falls back to `near_misses.md`)
2. Spawns a fresh `claude -p` session with the combined prompt
3. Runs regression tests after Claude finishes
4. If tests pass: commits all changes
5. If tests fail: asks Claude to fix (up to 3 attempts), then reverts code but keeps PROGRESS.md findings
6. Checks for `LOOP_COMPLETE` sentinel and exits if found

---

## Key Principles

### 1. ONE Task Per Iteration
Each iteration tackles exactly ONE item. This:
- Keeps iterations short (5-15 min)
- Prevents scope creep
- Makes progress visible

### 2. File-Based State
All state lives in files, not memory:
- **TASK.md**: Immutable goals
- **PROGRESS.md**: Accumulated results
- **Git commits**: Audit trail + rollback

### 3. Fresh Context Every Time
Each iteration starts with 0% context:
- No session continuation (`-c` flag)
- State injected via file reading
- No accumulated confusion

### 4. Completion Sentinel
The exact string `LOOP_COMPLETE` signals done:
- Must be added by agent, not script
- Agent verifies completion before adding
- Script checks and exits

---
