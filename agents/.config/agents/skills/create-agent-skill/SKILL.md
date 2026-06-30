---
name: create-agent-skill
description: >
  Creates and edits Agent Skills (SKILL.md files) according to the Agent Skills
  specification. Use when the user wants to create a new skill, scaffold a skill,
  write a SKILL.md, make an agent skill, define a reusable agent capability, or
  update an existing skill's instructions.
---

## Skill creation workflow

Follow this process when creating or editing an Agent Skill.

### Step 1: Understand the task

Ask the user clarifying questions until you can answer:

- What specific task should this skill help agents perform?
- What domain knowledge, conventions, or gotchas does the agent need that it wouldn't know from general training?
- What are the inputs and expected outputs?
- What tools, scripts, or references would a skill need?

If the user doesn't have a clear idea, suggest they walk through a real task first, then extract the reusable pattern.

### Step 2: Choose a name

Pick a kebab-case name that matches these rules:

- 1–64 characters
- Lowercase alphanumeric characters and single hyphens only (`[a-z0-9]+(-[a-z0-9]+)*`)
- Must not start or end with a hyphen
- Must not contain consecutive hyphens (`--`)
- Must match the directory name that will contain `SKILL.md`

Good: `pdf-fill-form`, `code-review`, `csv-analyzer`
Bad: `MySkill`, `-pdf-tool`, `data--cleaner`

### Step 3: Write the description

The description is the **only** thing agents read at discovery time, so it carries the full burden of triggering. Follow these principles:

- **Use imperative phrasing.** "Use when the user..." rather than "This skill helps with..."
- **Focus on user intent, not implementation.** Describe what the user is trying to achieve.
- **Be explicit about when to activate.** List specific contexts and keywords, including cases where the user doesn't name the domain directly: "even if they don't explicitly mention 'CSV' or 'analysis.'"
- **Keep it under 1024 characters.** A few sentences to a short paragraph.
- **Err on the side of being pushy** about when the skill should trigger.

Example of a good description:

```yaml
description: >
  Analyze CSV and tabular data files — compute summary statistics, add derived
  columns, generate charts, and clean messy data. Use when the user has a CSV,
  TSV, or Excel file and wants to explore, transform, or visualize the data,
  even if they don't explicitly mention "CSV" or "analysis."
```

### Step 4: Plan the body content

The SKILL.md body contains the instructions the agent follows. Structure it around these patterns (use only the ones that fit):

**Gotchas** — environment-specific facts that defy reasonable assumptions. Add these whenever an agent makes a mistake you have to correct: "The `users` table uses soft deletes. Queries must include `WHERE deleted_at IS NULL`."

**Step-by-step instructions** — numbered workflow with concrete commands and file paths.

**Templates** — when output must follow a specific format, provide a template. Inline for short ones; reference `assets/templates/` for longer ones.

**Checklists** — for multi-step workflows with dependencies between steps, use a progress checklist the agent can track.

**Validation loops** — "Make your edits → run `scripts/validate.py` → fix issues → repeat until validation passes."

**Plan-validate-execute** — for destructive or batch operations: create an intermediate plan artifact, validate it, then execute.

### Step 5: Apply best practices

When writing the body:

- **Add what the agent lacks, omit what it knows.** Skip general knowledge (how HTTP works, what a PDF is) and include only project-specific conventions, APIs, and non-obvious edge cases.
- **Provide defaults, not menus.** Pick one recommended approach and mention alternatives as fallbacks only when needed.
- **Favor procedures over declarations.** Teach *how to approach* a class of problems, not *what to produce* for one instance.
- **Match specificity to fragility.** Be prescriptive for fragile operations (exact commands, exact flags). Give freedom when multiple approaches are valid.
- **Keep SKILL.md under 500 lines.** Move detailed reference material to `references/` files and tell the agent when to load them: "Read `references/api-errors.md` if the API returns a non-200 status code."

### Step 6: Decide on directory structure

Start with just `SKILL.md`. Add optional directories only when needed:

```
skill-name/
├── SKILL.md          # Required: metadata + instructions
├── scripts/          # Optional: executable code (Python, Bash, JS)
├── references/       # Optional: detailed docs loaded on demand
└── assets/           # Optional: templates, data files, images
```

### Step 7: Create the skill

Create the directory and `SKILL.md` file at the appropriate location:

- Project-local: `.opencode/skills/<name>/SKILL.md` (or `.claude/skills/`, `.agents/skills/`)
- Global (user-wide): `~/.config/opencode/skills/<name>/SKILL.md` (or `~/.claude/skills/`, `~/.agents/skills/`)

Write the YAML frontmatter with at minimum `name` and `description`. Include optional fields as needed:

```yaml
---
name: skill-name
description: What the skill does and when to use it.
license: MIT
compatibility: Requires Python 3.11+ and uv
metadata:
  author: your-name
  version: "1.0"
---
```

Then write the Markdown body.

### Step 8: Validate (optional)

If `skills-ref` is available, validate the skill:

```bash
skills-ref validate path/to/skill-name
```

Otherwise, manually check: name is kebab-case, matches directory name, description is non-empty and under 1024 chars, SKILL.md starts with `---` frontmatter.

### Step 9: Report

Tell the user where the skill was created and how to verify it's discoverable:

- Verify the path is recognized by opencode
- Suggest the user test with a relevant prompt to confirm the skill triggers
- Remind them that skills improve with iteration — they should run it against real tasks and refine
