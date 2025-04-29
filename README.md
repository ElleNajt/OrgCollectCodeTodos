# org-collect-code-todos

An Emacs package that automatically collects TODO comments from your code files and organizes them in a central org-mode file.

## Features

- Automatically scans code files for TODO comments when saving
- Collects TODOs from both regular comments and string literals
- Organizes TODOs in a central `~/code-todos.org` file
- Creates links back to the original source location
- Synchronizes TODO/DONE state between org file and source code

## Installation

### Manual Installation

1. Download `org-collect-code-todos.el` to your load path
2. Add to your Emacs configuration:

```elisp
(require 'org-collect-code-todos)
```

### Using use-package

```elisp
(use-package org-collect-code-todos
  :load-path "/path/to/org-collect-code-todos"
  :hook (after-save . collect-todos-and-add-to-code-todos))
```

## Usage

The package works automatically:

1. When you save a file in `prog-mode`, it scans for TODO comments
2. TODOs are added to `~/code-todos.org` with links back to source
3. When you mark a TODO as DONE in the org file, the state is updated in the source

## Customization

You can customize the following variables:

```elisp
;; Change the location of the TODOs org file
(setq org-collect-code-todos-file "~/my-todos.org")
```

By default, the package:
- Looks for "TODO" markers in comments and string literals
- Saves collected TODOs to `~/code-todos.org`

## License

This project is licensed under the terms of the MIT license.
