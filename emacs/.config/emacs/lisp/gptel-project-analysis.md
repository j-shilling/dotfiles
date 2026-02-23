# GPTel Project Integration Analysis

## Overview

This document analyzes the current `gptel-project.el` implementation against the Emacs manual's Projects documentation (Info node: emacs, Projects) and suggests improvements.

## Current Implementation

The current `gptel-project.el` provides the following tools:

1. **get_project_root** - Returns the project root directory
2. **get_project_name** - Returns the project name
3. **list_project_buffers** - Lists buffers associated with the project
4. **list_project_files** - Lists all files in the project
5. **get_project_info** - Returns summary info (name, root, file count, buffer count)
6. **find_file_in_project** - Finds files matching a regex pattern

## Emacs Projects Functionality (from Manual)

According to the Emacs manual (node 30.2 Projects), project.el provides:

### Core Concepts
- **Project**: Collection of files for producing programs
- **Project Root**: Top-level directory of the project hierarchy
- **Project Back-ends**: VC-aware (Git, etc.) and EDE
- **File Membership**: Determined by back-end (e.g., respects .gitignore)

### File Operations (30.2.1)
- `project-find-file` (C-x p f) - Visit project files with completion
- `project-find-regexp` (C-x p g) - Search for regexp in project files
- `project-search` - Sequential regexp search
- `project-query-replace-regexp` (C-x p r) - Query-replace in project
- `project-find-dir` (C-x p d) - Choose and open a project directory
- `project-dired` (C-x p D) - Dired in project root
- `project-vc-dir` (C-x p v) - VC directory mode for project
- `project-shell` (C-x p s) - Shell in project root
- `project-eshell` (C-x p e) - Eshell in project root
- `project-compile` (C-x p c) - Compile in project root
- `project-shell-command` (C-x p !) - Run shell command
- `project-async-shell-command` (C-x p &) - Async shell command

### Buffer Operations (30.2.2)
- `project-switch-to-buffer` (C-x p b) - Switch to project buffer
- `project-list-buffers` (C-x p C-b) - List project buffers
- `project-kill-buffers` (C-x p k) - Kill all project buffers

### Project Management (30.2.3, 30.2.4)
- `project-switch-project` (C-x p p) - Switch to another project
- `project-forget-project` - Remove project from known list
- `project-list-file` - File storing known projects

### Configuration
- `project-vc-include-untracked` - Whether to include untracked files
- `project-mode-line` - Display project name in mode line
- `project-switch-commands` - Commands available when switching projects
- `project-kill-buffer-conditions` - Conditions for killing buffers

## Gap Analysis

### Missing Functionality

1. **Search/Replace Operations**
   - No tool for searching within project files (project-find-regexp)
   - No tool for query-replace across project files
   - These are critical for code refactoring and analysis

2. **Directory Operations**
   - No tool to list project directories
   - No tool to find specific directories in the project
   - Useful for understanding project structure

3. **Project Switching**
   - No tool to list known projects
   - No tool to get information about other projects
   - No tool to switch context to another project

4. **External Files**
   - No tool to list external dependencies or roots
   - Some projects have external dependencies tracked separately

5. **Ignored/Untracked Files**
   - No way to query which files are ignored
   - No way to list untracked files separately
   - Useful for understanding what's not in version control

6. **Project Configuration**
   - No way to query project-specific settings
   - No way to check if untracked files are included

## Suggested Improvements

### High Priority

#### 1. Add Search Functionality
```elisp
(defun gptel-project--search-regexp (pattern &optional buffer)
  "Search for PATTERN in all project files.
Returns matches with file paths and line numbers."
  ...)

(gptel-make-tool
 :name "search_project_files"
 :function #'gptel-project--search-regexp
 :description "Search for a regular expression pattern across all files in the project..."
 :args (list (list :name "pattern" :type 'string :description "Regular expression to search for")
             (list :name "buffer" :type 'string :optional t)))
```

#### 2. Add Directory Listing
```elisp
(defun gptel-project--list-directories (&optional buffer)
  "List all directories in the project."
  ...)

(gptel-make-tool
 :name "list_project_directories"
 :function #'gptel-project--list-directories
 :description "List all directories within the project..."
 :args (list (list :name "buffer" :type 'string :optional t)))
```

#### 3. Add Known Projects Management
```elisp
(defun gptel-project--list-known-projects ()
  "List all known projects from project-list-file."
  ...)

(gptel-make-tool
 :name "list_known_projects"
 :function #'gptel-project--list-known-projects
 :description "List all projects known to Emacs..."
 :args nil)
```

### Medium Priority

#### 4. Add External Roots Support
```elisp
(defun gptel-project--get-external-roots (&optional buffer)
  "Get external dependency roots for the project."
  ...)
```

#### 5. Add Ignored Files Query
```elisp
(defun gptel-project--list-ignored-files (&optional buffer)
  "List files that are ignored by the project (e.g., in .gitignore)."
  ...)
```

#### 6. Enhance find_file_in_project
- Add option to search by file extension
- Add option to search in specific subdirectories
- Return more metadata (file size, modification time)

### Low Priority

#### 7. Add Project Configuration Query
```elisp
(defun gptel-project--get-project-config (&optional buffer)
  "Get project configuration settings."
  ...)
```

#### 8. Add Buffer Filtering
```elisp
(defun gptel-project--list-project-buffers-by-mode (mode &optional buffer)
  "List project buffers filtered by major mode."
  ...)
```

## Implementation Considerations

### Performance
- `list_project_files` can be slow for large projects
  - Consider adding pagination or limiting results
  - Add option to list only recently modified files
  - Cache results with invalidation strategy

### Search Implementation
- For `search_project_files`, consider:
  - Using `project-find-regexp` internally
  - Limiting number of results returned
  - Providing context lines around matches
  - Supporting case-sensitive/insensitive options

### Error Handling
- Current implementation properly handles missing projects
- Should add validation for regex patterns
- Should handle permission errors when accessing files

### Structured Content
- Current tools return both text and structured content
- Maintain this pattern for new tools
- Structured content is more useful for programmatic access

## Recommended Next Steps

1. **Immediate**: Add `search_project_files` tool (most valuable for LLM use cases)
2. **Short-term**: Add `list_project_directories` and `list_known_projects`
3. **Medium-term**: Enhance `find_file_in_project` with more options
4. **Long-term**: Add ignored files and external roots support

## Additional Notes

### Integration with Other Tools
- Project tools should work well with existing buffer and file tools
- Consider how search results could be used with `read_text_file`
- Project switching could affect which buffers are visible

### Documentation
- Current tool descriptions are clear and comprehensive
- Maintain the pattern of explaining the BUFFER parameter
- Include examples of regex patterns in search tool descriptions

### Testing Considerations
- Test with various project types (Git, non-Git, EDE)
- Test with large projects (performance)
- Test with projects containing special characters in paths
- Test error cases (no project, invalid patterns)
