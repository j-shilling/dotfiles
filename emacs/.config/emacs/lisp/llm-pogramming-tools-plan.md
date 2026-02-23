# LLM Integration Plan for Emacs Programming Tools

## Overview

This document outlines a plan for creating LLM tool integrations for Emacs programming features: Eglot (LSP client), Flymake (syntax checking), Xref (code navigation), and ElDoc (documentation lookup).

## Package Summaries

### 1. Eglot - Language Server Protocol Client
**Purpose**: Provides IDE-like features by communicating with language servers via LSP protocol.

**Key Capabilities**:
- Manages language server processes for different programming languages
- Provides code completion, diagnostics, formatting, and refactoring
- Integrates with Xref for go-to-definition and find-references
- Integrates with Flymake for diagnostics
- Integrates with ElDoc for documentation at point

**Key Variables/Functions**:
- `eglot-server-programs` - Associates major modes with language servers
- `eglot` - Start/connect to language server
- `eglot-shutdown` - Stop language server
- `eglot-reconnect` - Reconnect to language server
- `eglot-rename` - Rename symbol across project
- `eglot-format` - Format buffer or region
- `eglot-code-actions` - Show available code actions

### 2. Flymake - On-the-fly Syntax Checking
**Purpose**: Displays syntax errors and warnings in buffers as you edit.

**Key Capabilities**:
- Runs diagnostic backends (including Eglot) to check code
- Displays errors/warnings with overlays in buffer
- Provides navigation between diagnostics
- Integrates with ElDoc to show diagnostic messages

**Key Variables/Functions**:
- `flymake-mode` - Enable/disable Flymake
- `flymake-diagnostic-functions` - Hook for backend functions
- `flymake-start` - Manually trigger check
- `flymake-goto-next-error` - Navigate to next diagnostic
- `flymake-goto-prev-error` - Navigate to previous diagnostic
- `flymake-diagnostics` - Get list of diagnostics in buffer

### 3. Xref - Cross-Reference Navigation
**Purpose**: Find definitions and references of identifiers (functions, variables, classes, etc.).

**Key Capabilities**:
- Find where identifiers are defined
- Find all references to an identifier
- Navigate through definition/reference history
- Search and replace across identifiers
- Works with multiple backends (Eglot, etags, built-in modes)

**Key Variables/Functions**:
- `xref-find-definitions` (M-.) - Go to definition
- `xref-find-references` (M-?) - Find all references
- `xref-find-apropos` (C-M-.) - Find identifiers matching pattern
- `xref-go-back` (M-,) - Go back in navigation history
- `xref-go-forward` (C-M-,) - Go forward in navigation history
- `xref-query-replace-in-results` - Replace in xref results

### 4. ElDoc - Documentation at Point
**Purpose**: Shows documentation for symbol at point in echo area or dedicated buffer.

**Key Capabilities**:
- Displays function signatures, variable documentation
- Works with multiple backends (Eglot, built-in modes, Flymake)
- Can show documentation in echo area or dedicated buffer
- Configurable display strategies

**Key Variables/Functions**:
- `eldoc-mode` - Enable/disable ElDoc in buffer
- `global-eldoc-mode` - Enable/disable globally
- `eldoc-print-current-symbol-info` - Manually trigger documentation
- `eldoc-doc-buffer` - Show documentation in dedicated buffer
- `eldoc-documentation-functions` - Hook for backend functions
- `eldoc-documentation-strategy` - How to combine multiple backends

## Tool Design Plan

### Phase 1: Information Retrieval Tools

#### 1.1 Eglot Tools

**Tool: `eglot_get_status`**
- **Purpose**: Check if Eglot is active in a buffer and get server info
- **Parameters**: 
  - `buffer` (optional) - Buffer name, defaults to current
- **Returns**: 
  - `active` (boolean)
  - `server_name` (string or nil)
  - `server_status` (string: "connected", "disconnected", etc.)
  - `project_root` (string or nil)

**Tool: `eglot_list_servers`**
- **Purpose**: List all active Eglot servers
- **Parameters**: None
- **Returns**: List of server info objects with:
  - `server_name`
  - `major_modes` (list)
  - `project_root`
  - `status`

**Tool: `eglot_get_capabilities`**
- **Purpose**: Get LSP capabilities of the server for a buffer
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: Object with boolean flags for capabilities:
  - `completion`
  - `hover`
  - `signature_help`
  - `definition`
  - `references`
  - `formatting`
  - `rename`
  - `code_actions`

#### 1.2 Flymake Tools

**Tool: `flymake_get_diagnostics`**
- **Purpose**: Get all diagnostics (errors/warnings) in a buffer
- **Parameters**: 
  - `buffer` (optional)
  - `severity` (optional) - Filter by severity: "error", "warning", "note"
- **Returns**: List of diagnostic objects:
  - `line` (integer)
  - `column` (integer)
  - `severity` (string)
  - `message` (string)
  - `source` (string) - Backend that reported it

**Tool: `flymake_get_status`**
- **Purpose**: Check Flymake status in buffer
- **Parameters**: 
  - `buffer` (optional)
- **Returns**:
  - `active` (boolean)
  - `running` (boolean) - Currently checking
  - `backends` (list of strings)
  - `error_count` (integer)
  - `warning_count` (integer)
  - `note_count` (integer)

#### 1.3 Xref Tools

**Tool: `xref_find_definitions`**
- **Purpose**: Find definitions of an identifier
- **Parameters**: 
  - `identifier` (string)
  - `buffer` (optional) - Context buffer
- **Returns**: List of location objects:
  - `file` (string)
  - `line` (integer)
  - `column` (integer)
  - `summary` (string) - Line content preview

**Tool: `xref_find_references`**
- **Purpose**: Find all references to an identifier
- **Parameters**: 
  - `identifier` (string)
  - `buffer` (optional)
- **Returns**: List of location objects (same format as definitions)

**Tool: `xref_find_apropos`**
- **Purpose**: Find identifiers matching a pattern
- **Parameters**: 
  - `pattern` (string) - Regexp pattern
  - `buffer` (optional)
- **Returns**: List of identifier matches with locations

**Tool: `xref_get_identifier_at_point`**
- **Purpose**: Get the identifier at point in a buffer
- **Parameters**: 
  - `buffer` (optional)
  - `position` (optional) - Point position
- **Returns**:
  - `identifier` (string or nil)
  - `bounds` (start, end positions)

#### 1.4 ElDoc Tools

**Tool: `eldoc_get_documentation`**
- **Purpose**: Get documentation for symbol at point
- **Parameters**: 
  - `buffer` (optional)
  - `position` (optional)
- **Returns**:
  - `documentation` (string or nil)
  - `symbol` (string)
  - `source` (string) - Which backend provided it

**Tool: `eldoc_get_status`**
- **Purpose**: Check ElDoc status
- **Parameters**: 
  - `buffer` (optional)
- **Returns**:
  - `active` (boolean)
  - `backends` (list of strings)

### Phase 2: Action Tools

#### 2.1 Eglot Actions

**Tool: `eglot_start`**
- **Purpose**: Start Eglot in a buffer
- **Parameters**: 
  - `buffer` (required)
- **Returns**: Success status and server info

**Tool: `eglot_shutdown`**
- **Purpose**: Shutdown Eglot server
- **Parameters**: 
  - `buffer` (optional) - Shutdown server for this buffer's project
- **Returns**: Success status

**Tool: `eglot_rename_symbol`**
- **Purpose**: Rename a symbol across the project
- **Parameters**: 
  - `old_name` (string)
  - `new_name` (string)
  - `buffer` (optional)
- **Returns**: 
  - `success` (boolean)
  - `files_modified` (list)
  - `changes_count` (integer)

**Tool: `eglot_format_buffer`**
- **Purpose**: Format buffer using language server
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: Success status

**Tool: `eglot_code_actions`**
- **Purpose**: Get available code actions at point
- **Parameters**: 
  - `buffer` (optional)
  - `position` (optional)
- **Returns**: List of action objects:
  - `title` (string)
  - `kind` (string)
  - `action_id` (string) - For executing

**Tool: `eglot_execute_code_action`**
- **Purpose**: Execute a specific code action
- **Parameters**: 
  - `action_id` (string)
  - `buffer` (optional)
- **Returns**: Success status and changes made

#### 2.2 Flymake Actions

**Tool: `flymake_start_check`**
- **Purpose**: Manually trigger Flymake check
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: Success status

**Tool: `flymake_goto_next_error`**
- **Purpose**: Navigate to next diagnostic
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: 
  - `moved` (boolean)
  - `diagnostic` (object) - The diagnostic moved to

**Tool: `flymake_goto_prev_error`**
- **Purpose**: Navigate to previous diagnostic
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: Same as next_error

#### 2.3 Xref Actions

**Tool: `xref_goto_definition`**
- **Purpose**: Navigate to definition of identifier
- **Parameters**: 
  - `identifier` (string)
  - `buffer` (optional) - Source buffer
  - `display_mode` (optional) - "current", "other-window", "other-frame"
- **Returns**: 
  - `success` (boolean)
  - `location` (object) - Where we navigated to

**Tool: `xref_goto_reference`**
- **Purpose**: Navigate to a specific reference
- **Parameters**: 
  - `identifier` (string)
  - `index` (integer) - Which reference to go to
  - `buffer` (optional)
- **Returns**: Location object

**Tool: `xref_go_back`**
- **Purpose**: Go back in xref navigation history
- **Parameters**: None
- **Returns**: Location we went back to

**Tool: `xref_go_forward`**
- **Purpose**: Go forward in xref navigation history
- **Parameters**: None
- **Returns**: Location we went forward to

### Phase 3: Integration Patterns

#### Pattern 1: Code Understanding Workflow
```
1. LLM asks: "What does function X do?"
2. Use xref_find_definitions to locate function
3. Read buffer content at that location
4. Use eldoc_get_documentation for additional context
5. Use xref_find_references to see how it's used
6. Synthesize answer
```

#### Pattern 2: Error Fixing Workflow
```
1. Use flymake_get_diagnostics to find errors
2. For each error:
   - Use eldoc_get_documentation for context
   - Use eglot_code_actions to get fix suggestions
   - Present options to user
   - Use eglot_execute_code_action to apply fix
3. Use flymake_start_check to verify fixes
```

#### Pattern 3: Refactoring Workflow
```
1. Use xref_find_references to find all uses
2. Analyze usage patterns
3. Suggest refactoring
4. Use eglot_rename_symbol for safe renaming
5. Use eglot_format_buffer to clean up
```

#### Pattern 4: Code Navigation Workflow
```
1. Use xref_get_identifier_at_point to get current symbol
2. Use xref_find_definitions to explore definition
3. Use xref_find_references to see usage
4. Use xref_go_back/forward to navigate history
5. Build mental model of code structure
```

## Implementation Strategy

### File Structure
```
~/.config/emacs/lisp/
├── llm-eglot.el      # Eglot integration tools
├── llm-flymake.el    # Flymake integration tools
├── llm-xref.el       # Xref integration tools
├── llm-eldoc.el      # ElDoc integration tools
└── llm-programming.el # Combined interface and workflows
```

### Development Order

1. **Start with Xref** (simplest, most self-contained)
   - Implement information retrieval tools
   - Implement navigation tools
   - Test with Emacs Lisp code

2. **Add ElDoc** (simple, complements Xref)
   - Implement documentation retrieval
   - Test integration with Xref

3. **Add Flymake** (moderate complexity)
   - Implement diagnostic retrieval
   - Implement navigation
   - Test with various backends

4. **Add Eglot** (most complex, ties everything together)
   - Implement status and capability queries
   - Implement LSP actions
   - Test with multiple language servers

5. **Create Unified Interface** (llm-programming.el)
   - Combine tools into coherent workflows
   - Add helper functions for common patterns
   - Document usage examples

### Key Design Principles

1. **Non-Intrusive**: Tools should query state without modifying it unless explicitly requested
2. **Context-Aware**: Default to current buffer/point when not specified
3. **Structured Output**: Return data in consistent, parseable formats
4. **Error Handling**: Gracefully handle cases where features aren't available
5. **Backend Agnostic**: Work with whatever backends are configured (Eglot, etags, etc.)

### Testing Strategy

1. Test each tool individually with simple cases
2. Test with multiple programming languages
3. Test with and without Eglot active
4. Test error conditions (no backend, invalid identifiers, etc.)
5. Test integration workflows end-to-end

## Next Steps

1. Review this plan and adjust based on feedback
2. Start implementation with llm-xref.el
3. Create test files for validation
4. Iterate and expand based on real usage
5. Document patterns and best practices

## Notes

- All these systems work together: Eglot provides backends for Xref, Flymake, and ElDoc
- Without Eglot, fallback backends still provide useful functionality
- The xref system is the foundation for code navigation
- ElDoc and Flymake provide real-time feedback while editing
- These tools enable sophisticated code understanding and manipulation by LLMs


---

## ADDENDUM: Points, Marks, Registers, and Bookmarks

### Overview

After researching the Emacs documentation, I've identified that **points, marks, registers, and bookmarks** are fundamental navigation and position-tracking mechanisms that are essential for programming tool integration. These features work hand-in-hand with Xref, Eglot, Flymake, and ElDoc.

### Key Concepts

#### Point
- **Definition**: The current cursor position in a buffer (between characters)
- **Representation**: An integer (1-indexed position)
- **Scope**: Each buffer has its own point; each window showing a buffer can have different point values
- **Key Functions**: `point`, `point-min`, `point-max`

#### Mark and Region
- **Mark**: A saved position in a buffer that, together with point, defines a region
- **Region**: The text between point and mark
- **Active vs Inactive**: Mark can be active (region highlighted) or inactive
- **Mark Ring**: Each buffer maintains a ring of previous mark positions (default: 16 entries)
- **Global Mark Ring**: Separate ring for marks across all buffers
- **Key Functions**: `mark`, `set-mark`, `push-mark`, `pop-mark`
- **Key Commands**: 
  - `C-SPC C-SPC` - Set mark without activating
  - `C-u C-SPC` - Jump to previous mark position
  - `C-x C-x` - Exchange point and mark

#### Registers
- **Definition**: Named storage compartments (single character names: 'a', '1', etc.)
- **Can Store**: 
  - Positions (buffer + point)
  - Text
  - Rectangles
  - Numbers
  - Window/frame configurations
  - Buffer names
  - File names
  - Keyboard macros
- **Persistence**: Session-only (not saved between Emacs sessions)
- **Key Commands**:
  - `C-x r SPC R` - Save position to register R
  - `C-x r j R` - Jump to position in register R
  - `M-x view-register` - View register contents

#### Bookmarks
- **Definition**: Like registers but with long names and persistent across sessions
- **Persistence**: Saved to `~/.emacs.d/bookmarks` file
- **Features**:
  - Can have annotations
  - Saved with surrounding context for robustness
  - Visual indicators on fringe
- **Key Commands**:
  - `C-x r m` - Set bookmark
  - `C-x r b` - Jump to bookmark
  - `C-x r l` - List all bookmarks
  - `M-x bookmark-save` - Save bookmarks

### Integration with Programming Tools

#### How Xref Uses These Features

1. **Navigation History**: Xref maintains its own history stack (similar to mark ring)
   - `M-.` (xref-find-definitions) pushes current position before jumping
   - `M-,` (xref-go-back) pops from history
   - `C-M-,` (xref-go-forward) moves forward in history

2. **Mark Integration**: When jumping to definitions, Xref pushes a mark so you can use `C-u C-SPC` to return

3. **Multiple Results**: When there are multiple definitions/references, point moves through them

#### How These Help with Eglot/LSP

1. **Code Actions**: Often need to know the current point to determine available actions
2. **Hover Documentation**: ElDoc needs point position to show documentation
3. **Diagnostics**: Flymake associates errors with specific point ranges
4. **Refactoring**: Operations like rename need to track multiple positions across buffers

### Tool Design: Position and Navigation Tools

#### Core Position Tools

**Tool: `get_point_info`**
- **Purpose**: Get detailed information about point in a buffer
- **Parameters**: 
  - `buffer` (optional)
- **Returns**:
  - `point` (integer) - Current position
  - `line` (integer) - Line number
  - `column` (integer) - Column number
  - `char_after` (string) - Character after point
  - `char_before` (string) - Character before point
  - `symbol_at_point` (string or nil) - Identifier at point
  - `in_string` (boolean) - Whether point is in a string
  - `in_comment` (boolean) - Whether point is in a comment

**Tool: `get_region_info`**
- **Purpose**: Get information about the current region
- **Parameters**: 
  - `buffer` (optional)
- **Returns**:
  - `active` (boolean) - Is region active
  - `start` (integer or nil) - Region start position
  - `end` (integer or nil) - Region end position
  - `text` (string or nil) - Text in region
  - `line_start` (integer)
  - `line_end` (integer)

**Tool: `set_point`**
- **Purpose**: Move point to a specific position
- **Parameters**: 
  - `buffer` (required)
  - `position` (integer, required) - Target position
  - `push_mark` (boolean, optional) - Whether to push current position to mark ring
- **Returns**: 
  - `success` (boolean)
  - `new_point` (integer)

**Tool: `goto_line`**
- **Purpose**: Move point to a specific line
- **Parameters**: 
  - `buffer` (required)
  - `line` (integer, required)
  - `column` (integer, optional) - Column on that line
  - `push_mark` (boolean, optional)
- **Returns**: 
  - `success` (boolean)
  - `point` (integer) - New point position

#### Mark Ring Tools

**Tool: `get_mark_ring`**
- **Purpose**: Get the mark ring for a buffer
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: List of mark positions:
  - `position` (integer)
  - `line` (integer)
  - `column` (integer)
  - `context` (string) - Surrounding text for context

**Tool: `push_mark_at_point`**
- **Purpose**: Push current point onto mark ring
- **Parameters**: 
  - `buffer` (optional)
  - `activate` (boolean, optional) - Whether to activate the mark
- **Returns**: 
  - `success` (boolean)
  - `mark_position` (integer)

**Tool: `pop_mark`**
- **Purpose**: Jump to previous mark position
- **Parameters**: 
  - `buffer` (optional)
- **Returns**: 
  - `success` (boolean)
  - `position` (integer) - Position we jumped to
  - `line` (integer)
  - `column` (integer)

**Tool: `get_global_mark_ring`**
- **Purpose**: Get marks from all buffers
- **Parameters**: None
- **Returns**: List of marks with:
  - `buffer` (string)
  - `position` (integer)
  - `line` (integer)
  - `file` (string or nil)

#### Register Tools

**Tool: `list_registers`**
- **Purpose**: List all registers and their contents
- **Parameters**: 
  - `type_filter` (optional) - Filter by type: "position", "text", "number", etc.
- **Returns**: List of register objects:
  - `name` (string) - Register name (single char)
  - `type` (string) - "position", "text", "rectangle", "number", etc.
  - `value` (varies by type)
  - `description` (string) - Human-readable description

**Tool: `save_position_to_register`**
- **Purpose**: Save current position to a register
- **Parameters**: 
  - `register` (string, required) - Single character register name
  - `buffer` (optional)
- **Returns**: 
  - `success` (boolean)
  - `register` (string)
  - `position` (integer)
  - `buffer` (string)

**Tool: `jump_to_register`**
- **Purpose**: Jump to position saved in register
- **Parameters**: 
  - `register` (string, required)
- **Returns**: 
  - `success` (boolean)
  - `buffer` (string) - Buffer we jumped to
  - `position` (integer)
  - `line` (integer)
  - `column` (integer)

**Tool: `save_text_to_register`**
- **Purpose**: Save region text to a register
- **Parameters**: 
  - `register` (string, required)
  - `buffer` (optional)
  - `start` (integer, optional) - If not provided, uses region
  - `end` (integer, optional)
- **Returns**: 
  - `success` (boolean)
  - `text_length` (integer)

**Tool: `insert_from_register`**
- **Purpose**: Insert text from register at point
- **Parameters**: 
  - `register` (string, required)
  - `buffer` (optional)
- **Returns**: 
  - `success` (boolean)
  - `text_inserted` (string)

#### Bookmark Tools

**Tool: `list_bookmarks`**
- **Purpose**: List all bookmarks
- **Parameters**: None
- **Returns**: List of bookmark objects:
  - `name` (string)
  - `file` (string)
  - `position` (integer)
  - `line` (integer)
  - `annotation` (string or nil)
  - `context` (string) - Surrounding text

**Tool: `set_bookmark`**
- **Purpose**: Create a bookmark at current position
- **Parameters**: 
  - `name` (string, required)
  - `buffer` (optional)
  - `annotation` (string, optional)
  - `overwrite` (boolean, optional) - Whether to overwrite existing
- **Returns**: 
  - `success` (boolean)
  - `name` (string)
  - `file` (string)
  - `position` (integer)

**Tool: `jump_to_bookmark`**
- **Purpose**: Jump to a bookmark
- **Parameters**: 
  - `name` (string, required)
- **Returns**: 
  - `success` (boolean)
  - `buffer` (string)
  - `file` (string)
  - `position` (integer)
  - `line` (integer)

**Tool: `delete_bookmark`**
- **Purpose**: Delete a bookmark
- **Parameters**: 
  - `name` (string, required)
- **Returns**: 
  - `success` (boolean)

**Tool: `save_bookmarks`**
- **Purpose**: Save bookmarks to disk
- **Parameters**: None
- **Returns**: 
  - `success` (boolean)
  - `file` (string) - Path to bookmark file
  - `count` (integer) - Number of bookmarks saved

### Enhanced Workflow Patterns

#### Pattern 1: Code Exploration with Position Tracking
```
1. LLM asks: "Show me how function X is used"
2. Use xref_find_references to find all uses
3. For each reference:
   - Use push_mark_at_point to save current position
   - Use goto_line to jump to reference
   - Use get_point_info to get context
   - Read surrounding code
4. Use pop_mark repeatedly to return through visited locations
5. Synthesize usage patterns
```

#### Pattern 2: Multi-Location Refactoring
```
1. Identify locations that need changes
2. Save each location to a register (a, b, c, etc.)
3. For each register:
   - Use jump_to_register
   - Make changes
   - Use eglot_code_actions if available
4. Use get_mark_ring to review all visited locations
```

#### Pattern 3: Persistent Code Review Points
```
1. User asks: "Mark these functions for later review"
2. For each function:
   - Use xref_find_definitions to locate
   - Use set_bookmark with descriptive name
   - Add annotation with review notes
3. Later: Use list_bookmarks to see review queue
4. Use jump_to_bookmark to visit each one
5. Use delete_bookmark when review complete
```

#### Pattern 4: Region-Based Operations
```
1. Use get_region_info to check if region is active
2. If active:
   - Use flymake_get_diagnostics with region bounds
   - Use eglot_format_buffer on region only
   - Use save_text_to_register to save original
3. If not active:
   - Use get_point_info to get current symbol
   - Use xref_find_definitions on that symbol
```

#### Pattern 5: Cross-Buffer Navigation
```
1. Use get_global_mark_ring to see recent positions across buffers
2. Identify related code locations
3. Use save_position_to_register for key locations
4. Build a "tour" of related code
5. Use jump_to_register to navigate the tour
6. Use set_bookmark for important discoveries
```

### Integration Points with Programming Tools

#### With Xref
- **Before Jump**: Save position to mark ring or register
- **After Jump**: Get point info to understand context
- **Multiple Results**: Use registers to mark interesting results

#### With Eglot
- **Code Actions**: Need point position to determine available actions
- **Hover**: Use get_point_info to get symbol for documentation
- **Rename**: Track all positions that will change
- **Format**: Can format region if active, otherwise whole buffer

#### With Flymake
- **Diagnostics**: Associate with point/line positions
- **Navigation**: Use goto_line to jump to errors
- **Context**: Use get_point_info to understand error location

#### With ElDoc
- **Documentation**: Triggered by point position
- **Symbol Lookup**: Use get_point_info to get current symbol

### Implementation Priority

1. **Phase 1**: Core position tools (get_point_info, set_point, goto_line)
2. **Phase 2**: Mark ring tools (essential for navigation history)
3. **Phase 3**: Register tools (powerful for multi-location workflows)
4. **Phase 4**: Bookmark tools (persistent navigation)

### Key Design Considerations

1. **Coordinate Systems**: 
   - Emacs uses 1-indexed positions
   - Lines and columns are also 1-indexed
   - Be consistent in all tool interfaces

2. **Context Preservation**:
   - Always provide surrounding text for context
   - Include line/column in addition to raw position
   - Consider buffer narrowing (point-min/point-max)

3. **Safety**:
   - Validate positions before jumping
   - Handle killed buffers gracefully
   - Don't lose user's position without saving to mark ring

4. **Integration**:
   - These tools should be used by higher-level programming tools
   - Xref tools should automatically use mark ring
   - Eglot actions should report positions affected

5. **User Experience**:
   - Preserve navigation history
   - Make it easy to return to previous locations
   - Provide context in all position reports

### File Structure Update

```
~/.config/emacs/lisp/
├── llm-eglot.el      # Eglot integration tools
├── llm-flymake.el    # Flymake integration tools
├── llm-xref.el       # Xref integration tools
├── llm-eldoc.el      # ElDoc integration tools
├── llm-position.el   # NEW: Point, mark, region tools
├── llm-registers.el  # NEW: Register and bookmark tools
└── llm-programming.el # Combined interface and workflows
```

### Summary

Points, marks, registers, and bookmarks are the **foundation** of navigation in Emacs. They are essential for:

1. **Tracking where you've been** (mark ring, global mark ring)
2. **Saving important locations** (registers, bookmarks)
3. **Defining regions for operations** (point and mark)
4. **Building navigation workflows** (combining all of the above)

These tools will make the programming tool integrations much more powerful by enabling:
- Multi-location workflows
- Navigation history
- Context preservation
- Persistent location tracking
- Region-based operations

The LLM can use these tools to build sophisticated code exploration and modification workflows that feel natural and preserve the user's context.
