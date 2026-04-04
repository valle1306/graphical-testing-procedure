# Contributing

This repository follows a lightweight workflow so changes stay reviewable and do not surprise other collaborators.

## Branch Workflow

- Create a branch for each feature, bug fix, or cleanup instead of working directly on `main`.
- Keep `main` stable and use branch names that describe the change.
- Open a pull request or otherwise share the branch before merging user-visible work.

## Commit Hygiene

- Make incremental commits so each commit captures one clear step.
- Write commit messages that explain what changed and why.
- Prefer a few focused commits over one large mixed commit.

## Code Comments

- Add informative comments when logic is not obvious from the code alone.
- Keep comments focused on intent, assumptions, or tricky behavior.
- Update comments when the implementation changes so they stay trustworthy.

## UI Changes

- Discuss user-visible UI changes before merging them, especially when they alter existing interaction patterns.
- When a UI change is experimental, keep it isolated on a branch until the team agrees to adopt it.

## Verification Before Commit

- Run the relevant verification scripts before committing.
- Re-test existing behavior that could be affected by the change, not just the new feature.
- If verification is blocked in your environment, note that clearly in your commit, PR, or handoff summary.
