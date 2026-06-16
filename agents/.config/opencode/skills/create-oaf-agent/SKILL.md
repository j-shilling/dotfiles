---
name: create-oaf-agent
description: >
  Creates Open Agent Format (OAF) compliant agent directories with AGENTS.md
  manifests, skills, MCP configs, and version history. Use when the user wants
  to create an OAF agent, scaffold an agent directory, generate an AGENTS.md
  file, set up Open Agent Format configuration, build a multi-agent package,
  or create a portable agent definition that works across Claude Code, Goose,
  Deep Agents, and Letta.
---

## OAF Agent Creation Workflow

Follow this process to scaffold an Open Agent Format (OAF v0.8.0) compliant agent directory.

### Step 1: Gather requirements

Ask the user until you can answer:

- **Agent name** — display name (1–100 chars), vendor key (kebab-case), agent key (kebab-case), version (semver)
- **Agent type** — "main agent" (structured Markdown with sections) or "sub-agent" (simplified, direct system prompt)
- **Purpose** — what the agent does, core responsibilities
- **Model** — provider and name, or a simplified alias like `sonnet`/`opus`/`haiku`
- **Composition** — any skills, packs, weblets, MCP servers, or sub-agents it composes
- **Tools** — explicit tool access list (e.g., `["Read", "Edit", "Bash", "Glob", "Grep"]`)
- **Output directory** — where to create the agent directory (default: `./` in the current project)

If the user doesn't specify, use these defaults:
- vendor: `"local"`
- version: `"0.1.0"`
- license: `"MIT"`
- model: `"sonnet"` for sub-agents, full object for main agents

### Step 2: Validate identifiers

- `vendorKey` and `agentKey` must be kebab-case: `[a-z0-9]+(-[a-z0-9]+)*`
- `version` must be valid semver: `MAJOR.MINOR.PATCH`
- `slug` is always `vendorKey/agentKey`
- `license` must be an SPDX identifier (e.g., `"MIT"`, `"Apache-2.0"`, `"GPL-3.0"`)
- `tags` must be an array of strings

### Step 3: Create the directory structure

Always create the agent directory. Create optional subdirectories only when the user provides data for them:

```
agent-name/
├── AGENTS.md                    # Required: main manifest
├── README.md                    # Auto-generated if missing
├── LICENSE                      # Auto-generated if missing
├── versions/                    # Only if version history is requested
│   └── v0.1.0/
│       └── AGENTS.md
├── skills/                      # Only if local skills exist (source: "local")
│   └── skill-name/
│       └── SKILL.md
├── mcp-configs/                 # Only if MCP servers are referenced
│   └── server-name/
│       ├── ActiveMCP.json
│       └── config.yaml
├── examples/                    # Only if examples are provided
├── tests/                       # Only if tests are provided
├── docs/                        # Only if additional docs are provided
└── assets/                      # Only if media files are provided
```

### Step 4: Generate AGENTS.md

The AGENTS.md file is the primary manifest. It has two sections: YAML frontmatter (between `---` delimiters) and a Markdown body.

#### Required frontmatter (identity + metadata)

```yaml
---
name: "Display Name"
vendorKey: "vendor-namespace"
agentKey: "agent-identifier"
version: "1.0.0"
slug: "vendor-namespace/agent-identifier"
description: "Brief description of agent purpose and capabilities"
author: "@vendor-handle"
license: "MIT"
tags: ["tag1", "tag2"]
---
```

#### Optional frontmatter sections

Add these only when the user provides data:

**Skills** — references to skills (local, registry, or well-known URL):
```yaml
skills:
  - name: "skill-name"
    source: "local"                         # or a well-known URL
    version: "1.0.0"
    required: true
```

**Packs** — collections of skills:
```yaml
packs:
  - vendor: "vendor"
    pack: "pack-name"
    version: "1.0.0"
    required: false
```

**Weblets** — web-based tools:
```yaml
weblets:
  - vendor: "vendor"
    weblet: "weblet-name"
    version: "1.0.0"
    launch: "onDemand"              # "onDemand", "background", "foreground"
```

**MCP Servers** — Model Context Protocol servers:
```yaml
mcpServers:
  - vendor: "vendor"
    server: "server-name"
    version: "1.0.0"
    configDir: "mcp-configs/server-name"
    required: true
```

**Sub-Agents** — nested agent delegation:
```yaml
agents:
  - vendor: "vendor"
    agent: "agent-name"
    version: "1.0.0"
    role: "reviewer"
    delegations: ["code-quality"]
    required: false
```

**Orchestration** — entrypoint, fallback, triggers:
```yaml
orchestration:
  entrypoint: "main"
  fallback: "error-handler"
  triggers:
    - event: "code-change"
      action: "review"
```

**Tools** — explicit tool access (Claude Code sub-agent style):
```yaml
tools: ["Read", "Edit", "Bash", "Glob", "Grep"]
```

**Configuration** — model params and tool policy:
```yaml
config:
  temperature: 0.7
  max_tokens: 4096
  require_confirmation: false
  tools:
    allowed: ["bash", "python", "edit", "read"]
    denied: ["system-admin"]
```

**Memory** — for stateful agents:
```yaml
memory:
  type: "editable"
  blocks:
    personality: "default"
    user_context: "default"
```

**Model** — full format or simplified alias:
```yaml
# Full format:
model:
  provider: "anthropic"
  name: "claude-sonnet-4-5"
  embedding: "voyage-2"

# OR simplified alias (for sub-agents):
model: "sonnet"
```

**Harness-specific config** — free-form section per harness:
```yaml
harnessConfig:
  claude-code:
    allowed-tools: ["bash", "edit", "read"]
    progressive-disclosure: true
  goose:
    docker-image: "python:3.11"
```

#### Body format

The body starts after the closing `---` of the frontmatter.

**For main agents** (structured format) — the body MUST start with a `#` heading. Use these sections:

```markdown
# Agent Purpose

Describe the agent's primary role, expertise, and what problems it solves.

## Core Responsibilities

- Primary responsibility 1
- Primary responsibility 2

## Capabilities

### Domain Knowledge
What the agent knows about and can assist with.

### Technical Skills
- Programming languages, tools, frameworks, protocols

### Operational Skills
- Task planning, error handling, context management

## Communication Style

- **Tone**: Professional, friendly, concise
- **Verbosity**: Detailed explanations with examples
- **Format**: Structured responses with code blocks

## Decision-Making Framework

1. **Analysis**: Understand requirements and constraints
2. **Planning**: Break down into subtasks
3. **Execution**: Implement with best practices
4. **Validation**: Test and verify results
5. **Iteration**: Refine based on feedback

## Behavioral Guidelines

### Do:
- Always explain reasoning
- Ask clarifying questions when uncertain
- Follow project conventions

### Don't:
- Make assumptions about requirements
- Skip error handling
- Ignore security best practices

## Tool Usage Patterns

Describe how the agent uses available tools — file operations, code execution, external APIs.

## Delegation Strategy

When this agent delegates to sub-agents or skills:
- **Condition**: Describe when delegation occurs
- **Handoff**: How context is provided
- **Monitoring**: How progress is tracked
- **Integration**: How results are merged

## Limitations & Boundaries

- List limitations
- Define boundaries
- Note restrictions

## Examples

Provide usage examples.

### Example 1: [Scenario Name]
```markdown
**User**: [User request]

**Agent**: [Agent response with reasoning]
```

## Version History

- **1.0.0** (YYYY-MM-DD): Initial release
```

**For sub-agents** (simplified format) — the body does NOT start with `#`. Write a direct system prompt:

```markdown
You are a code reviewer. When invoked, analyze the code and provide specific, actionable feedback on:

- Code quality and readability
- Security vulnerabilities
- Performance issues
- Best practices adherence
- Potential bugs

Be concise but thorough. Prioritize critical issues first.
```

### Step 5: Auto-generate README.md and LICENSE

**README.md** — derive from AGENTS.md content. Include the agent name, description, version, tags, capabilities, and usage examples. Format:

```markdown
# {name}

{description}

**Version:** {version}
**Author:** {author}
**License:** {license}
**Tags:** {tags}

## Capabilities

{derived from AGENTS.md capabilities section}

## Usage

{derived from AGENTS.md examples section}

## License

{license reference}
```

**LICENSE** — if the license is MIT, generate the standard MIT license text. For other SPDX identifiers, use the standard text for that license. If unsure, write `{license}` as a placeholder.

### Step 6: Create optional directories

Only create these when the user provides data:

**skills/** — for each skill with `source: "local"`, create `skills/<skill-name>/SKILL.md` using the AgentSkills.io format:

```yaml
---
name: "skill-name"
description: "Brief description of what this skill does"
license: "MIT"
metadata:
  author: "vendor-name"
  version: "1.0.0"
allowed-tools: ["bash", "python", "edit"]
---

# Skill Purpose

Instructions for using this skill.
```

**mcp-configs/** — for each MCP server, create a subdirectory with two files:

`ActiveMCP.json`:
```json
{
  "vendor": "vendor",
  "server": "server-name",
  "version": "1.0.0",
  "selectedTools": [
    {
      "name": "tool_name",
      "enabled": true,
      "description": "What the tool does",
      "required": true
    }
  ],
  "excludedTools": [],
  "contextStrategy": "subset"
}
```

`config.yaml`:
```yaml
vendor: "vendor"
server: "server-name"
version: "1.0.0"

connection:
  type: "sse"
  url: "http://localhost:8811/sse"
  timeout: 60

auth:
  type: "bearer"
  token: "${ENV_VAR_NAME}"

permissions:
  allow_paths: ["/workspace", "/tmp"]
  deny_paths: ["/system", "/etc"]
  read_only: false

rate_limit:
  requests_per_minute: 60
  burst: 10
```

**versions/** — if the user wants version history, create `versions/v{version}/AGENTS.md` with a copy of the current AGENTS.md.

### Step 7: Validate

Check the generated AGENTS.md:

1. YAML frontmatter is between `---` delimiters and parses correctly
2. Required identity fields are present: `name`, `vendorKey`, `agentKey`, `version`, `slug`
3. Required metadata fields are present: `description`, `author`, `license`, `tags`
4. `version` matches semver format
5. `vendorKey` and `agentKey` are kebab-case
6. `slug` equals `vendorKey/agentKey`
7. `license` is a valid SPDX identifier
8. For structured agents, body starts with `#`
9. Body has at least one `##` heading (for structured agents)

### Step 8: Report

Tell the user:
- The agent directory path and what was created
- The full path to AGENTS.md
- How to test the agent (e.g., with opencode or another harness)
- That the agent follows OAF v0.8.0 and is compatible with Claude Code, Goose, Deep Agents, and Letta