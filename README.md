# supreme-brackets.el

`supreme-brackets` is an intelligent, context-aware bracket insertion and wrapping package for Emacs.

It's designed to provide the _smartest_ behavior with the _fewest_ keys, adapting to your current context (active region, at a word, at the start of a line) and programming language (Lisp vs. non-Lisp). It works great with Python, PHP, TypeScript, Go, SQL, C/C++, Rust, and all other programming languages.

---

## 🌟 Key Features

- **Context-Aware:**
  - **Region:** If a region is active, it wraps it with brackets.
  - **Word:** If at a word, it wraps the word.
  - **Line:** If at the beginning of a non-empty line, it wraps the whole line.
  - **Empty:** If at a word boundary (like `my_var|`), it inserts an empty pair `()` (for non-Lisp modes).
- **Language-Aware:** Automatically provides "wrap" behavior in Lisp-family modes and "insert empty" behavior in most other programming modes.
- **Rich Library:** Supports a massive range of brackets, including:
  - **ASCII:** `()`, `[]`, `{}`, `<>`
  - **Quotes:** `""`, `''`, `     `
  - **Documentation:** `"""..."""` (Python), `/** ... */` (JSDoc, etc.), Markdown code blocks.
  - **Typographic:** `“”`, `‘’`, `«»`, `‹›`
  - **CJK:** `「」`, `『』`, `《》`, `【】`, and more.
- **Powerful Editing:** Provides commands for smart deletion, navigation, and replacement of bracket pairs.
- **Simple Keybindings:** Designed to be ergonomic, with simple `Super` key bindings (`s-(`) and optional prefix maps (`s-i`, `s-b`) for extended functionality.

---

## 💾 Installation & Keybindings

The recommended installation is with Elpaca and `use-package`.

To activate the keybindings, select one of the two options available:

```elisp
(use-package supreme-brackets
  :ensure (:host github :repo "J4VMC/supreme-brackets")
  :config
  ;; Choose ONE of the following setup functions:

  ;; 1. For just the default (s-(, s-[, s-{) bindings:
  (supreme-brackets-setup-default-keybindings)

  ;; 2. (Recommended) For all bindings (s-(, s-i, s-b):
  (supreme-brackets-setup-extended-keybindings))
```

### Default Bindings (`s-` is the Super/Windows/Cmd key)

These bindings trigger the main context-aware wrapping function.

| Key   | Command                                   | Function                           |
| :---- | :---------------------------------------- | :--------------------------------- |
| `s-(` | `supreme-brackets-insert-parentheses`     | Smartly inserts or wraps with `()` |
| `s-[` | `supreme-brackets-insert-square-brackets` | Smartly inserts or wraps with `[]` |
| `s-{` | `supreme-brackets-insert-curly-braces`    | Smartly inserts or wraps with `{}` |

### Extended Insertion (Prefix: `s-i`)

These bindings are available after calling `supreme-brackets-setup-extended-keybindings`.

| Key      | Command                                       | Function                                 |
| :------- | :-------------------------------------------- | :--------------------------------------- |
| `s-i '`  | `supreme-brackets-insert-single-quotes`       | Smartly inserts or wraps with `''`       |
| `s-i "`  | `supreme-brackets-insert-double-quotes`       | Smartly inserts or wraps with `""`       |
| `s-i \`` | `supreme-brackets-insert-backticks`           | Smartly inserts or wraps with `` \`\` `` |
| `s-i \<` | `supreme-brackets-insert-angle-brackets`      | Smartly inserts or wraps with`\<\>`      |
| `s-i e`  | `supreme-brackets-insert-emacs-quotes`        | Smartly inserts or wraps with `` `'`     |
| `s-i m`  | `supreme-brackets-insert-markdown-code-block` | Wraps with `` \`\`\` ``                  |
| `s-i 3\` | `supreme-brackets-insert-python-docstring`    | Wraps with `` `"""..."""` ``             |
| `s-i $`  | `supreme-brackets-insert-php-variable`        | Inserts `$` and wraps `($...)`           |

### Bracket Editing (Prefix: `s-b`)

These powerful editing commands are available after calling `supreme-brackets-setup-extended-keybindings`.

| Key     | Group       | Command                                          | Function                                                                         |
| :------ | :---------- | :----------------------------------------------- | :------------------------------------------------------------------------------- |
| `s-b d` | Deletion    | `supreme-brackets-delete-smart-backward`         | Deletes char, or kills entire S-expression (or just pairs with prefix arg).      |
| `s-b k` | Deletion    | `supreme-brackets-delete-backward-pairs`         | Deletes only the surrounding bracket pair, leaving text.                         |
| `s-b m` | Navigation  | `supreme-brackets-nav-goto-matching-bracket`     | Jumps to the matching bracket.                                                   |
| `s-b s` | Navigation  | `supreme-brackets-nav-select-text-in-delimitors` | Selects the text inside the nearest delimiters.                                  |
| `s-b l` | Navigation  | `supreme-brackets-nav-backward-left-bracket`     | Searches backward for any left bracket.                                          |
| `s-b r` | Navigation  | `supreme-brackets-nav-forward-right-bracket`     | Searches forward for any right bracket.                                          |
| `s-b c` | Replacement | `supreme-brackets-replace-change-bracket-pairs`  | Interactively replaces bracket pairs (e.g., `()` -\> `[]`) in the current block. |

### All Interactive Commands

You can also run any command directly with `M-x`.

#### Basic

- `supreme-brackets-insert-parentheses`
- `supreme-brackets-insert-square-brackets`
- `supreme-brackets-insert-curly-braces`
- `supreme-brackets-insert-angle-brackets`

#### Quotes

- `supreme-brackets-insert-double-quotes`
- `supreme-brackets-insert-single-quotes`
- `supreme-brackets-insert-backticks`
- `supreme-brackets-insert-emacs-quotes`

#### Markdown & Docstrings

- `supreme-brackets-insert-markdown-code-inline`
- `supreme-brackets-insert-markdown-code-block`
- `supreme-brackets-insert-python-docstring`
- `supreme-brackets-insert-python-triple-single-quotes`

#### Language-Specific

- `supreme-brackets-insert-template-literal` (JS/TS `` `${}` ``)
- `supreme-brackets-insert-rust-raw-string` (`r#"..."#`)
- `supreme-brackets-insert-go-raw-string` (backticks)
- `supreme-brackets-insert-sql-identifier` (backticks)
- `supreme-brackets-insert-postgres-identifier` (double-quotes)
- `supreme-brackets-insert-php-variable`

#### Typographic (Curly/Smart Quotes)

- `supreme-brackets-insert-curly-double-quotes` (`“”`)
- `supreme-brackets-insert-curly-single-quotes` (`‘’`)
- `supreme-brackets-insert-single-guillemets` (`‹›`)
- `supreme-brackets-insert-double-guillemets` (`«»`)

#### CJK Brackets

- `supreme-brackets-insert-corner-brackets` (`「」`)
- `supreme-brackets-insert-white-corner-brackets` (`『』`)
- `supreme-brackets-insert-cjk-angle-brackets` (`〈〉`)
- `supreme-brackets-insert-cjk-double-angle-brackets` (`《》`)
- `supreme-brackets-insert-white-lenticular-brackets` (`〖〗`)
- `supreme-brackets-insert-black-lenticular-brackets` (`【】`)
- `supreme-brackets-insert-tortoise-shell-brackets` (`〔〕`)

#### Explicit Wrapping

- `supreme-brackets-wrap-line-with-parentheses`
- `supreme-brackets-wrap-block-with-parentheses`
- `supreme-brackets-wrap-line-with-braces`
- `supreme-brackets-wrap-block-with-braces`

---

## 🔧 Customization

You can customize the behavior of the package by modifying these variables (e.g., with `M-x customize-group RET supreme-brackets RET`).

- `supreme-brackets-lisp-modes`:
  A list of major modes that should be treated as "Lisp-like." In these modes, pressing `(` at the end of a word (e.g., `hello|`) will wrap the word (`(hello)`) instead of inserting an empty pair (`hello()`).

  ```elisp
  (setq supreme-brackets-lisp-modes
      '(emacs-lisp-mode
        lisp-mode
        clojure-mode
        scheme-mode
        racket-mode))
  ```

- `supreme-brackets-string-modes`:
  A list of major modes that are primarily for text/strings.
  ```elisp
  (setq supreme-brackets-string-modes
      '(text-mode
        markdown-mode
        org-mode
        latex-mode))
  ```

You can also customize the keybindings if you prefer to use the Meta or Control keys instead of Super:

```elisp
;; Basic insertion
(global-set-key (kbd "C-c (") #'supreme-brackets-insert-parentheses)
(global-set-key (kbd "C-c [") #'supreme-brackets-insert-square-brackets)
(global-set-key (kbd "C-c {") #'supreme-brackets-insert-curly-braces)

;; Navigation
(global-set-key (kbd "C-c m") #'supreme-brackets-nav-goto-matching-bracket)
(global-set-key (kbd "C-c s") #'supreme-brackets-nav-select-text-in-delimiters)

;; Deletion
(global-set-key (kbd "C-c d") #'supreme-brackets-delete-smart-backward)
```

You can also configure it for Evil mode (I've not tested this myself as I'm not an Evil user):

```elisp
(use-package supreme-brackets
  :config
  (define-key evil-normal-state-map (kbd "s-(") #'supreme-brackets-insert-parentheses)
  (define-key evil-normal-state-map (kbd "s-[") #'supreme-brackets-insert-square-brackets)
  (define-key evil-visual-state-map (kbd "s-(") #'supreme-brackets-insert-parentheses))
```

---

## Comparison with Other Packages

### vs. `electric-pair-mode`

- Supreme Brackets provides word wrapping and line wrapping
- Context-aware behavior (end-of-word insertion vs wrapping)
- Navigation and deletion commands included
- Bracket transformation functionality
- Support for Unicode and decorative brackets

### vs. `smartparens`

- Simpler, focused functionality - easier to understand and customize
- Language-agnostic by design - works consistently across all languages
- Explicit commands rather than automatic transformations
- Lightweight with minimal configuration required

### vs. `paredit`

- Supreme Brackets works well in all programming modes, not just Lisp
- Less opinionated - doesn't enforce structural editing
- Can be used alongside paredit in Lisp modes
- Provides Unicode bracket support that paredit lacks

---

## ❤️ Credits

This package was heavily inspired by the philosophy and original code of [Xah Lee](http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html).

## ⚖️ License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY. See the GNU General Public License for more details.
