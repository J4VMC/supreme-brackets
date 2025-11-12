;;; supreme-brackets.el --- Smart bracket insertion and wrapping -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Javier Miranda
;; Version: 1.0.0
;; Keywords: convenience, editing, brackets
;; URL: https://github.com/J4VMC/supreme-brackets
;; Package-Name: supreme-brackets
;; Package-Version: 1.0.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1"))

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides intelligent bracket insertion that adapts to context
;; and programming language. Works great with Python, PHP, TypeScript, Go, SQL,
;; and all other programming languages.

;; This package has been heavily inspired by the work of Xah Lee.

;; Key features:
;; - Wraps selected regions
;; - Wraps current line or block
;; - Inserts empty brackets or wraps words at point
;; - Language-aware behavior (e.g., different handling in Lisp vs other languages)
;; - Handles various bracket types: ASCII, Unicode, Markdown, and language-specific

;;; Code:

;;;###autoload
(defgroup supreme-brackets nil
  "Smart bracket insertion and wrapping."
  :group 'editing
  :prefix "supreme-brackets-")

(defcustom supreme-brackets-lisp-modes '(emacs-lisp-mode
					 lisp-mode
					 lisp-interaction-mode
					 common-lisp-mode
					 clojure-mode
					 clojurescript-mode
					 clojurec-mode
					 scheme-mode
					 racket-mode
					 hy-mode)
  "List of major modes considered to be Lisp modes.
In these modes, brackets behave differently,
like wrapping instead of inserting empty."
  :type '(repeat symbol)
  :group 'supreme-brackets)

(defcustom supreme-brackets-string-modes '(fundamental-mode
					   text-mode
					   markdown-mode
					   org-mode
					   latex-mode
					   tex-mode)
  "List of major modes where bracket insertion is primarily for text/strings.
In these modes, bracket behavior may differ from programming modes."
  :type '(repeat symbol)
  :group 'supreme-brackets)

;;; Helper Functions

(defun supreme-brackets--in-lisp-mode-p ()
  "Return non-nil if current buffer is in a Lisp-related mode."
  (or (memq major-mode supreme-brackets-lisp-modes)
      (derived-mode-p 'lisp-mode)))

(defun supreme-brackets--in-string-mode-p ()
  "Return non-nil if current buffer is primarily for text/strings."
  (or (memq major-mode supreme-brackets-string-modes)
      (derived-mode-p 'text-mode)))

(defun supreme-brackets--find-block-boundaries ()
  "Find the boundaries of the current text block.
A block is defined as text between blank lines.
Returns a cons cell (BEG . END)."
  (let ((beg (save-excursion
	       (if (re-search-backward "\\`\\|\n[[:space:]]*\n" nil t)
		   (match-end 0)
		 (point-min))))
	(end (save-excursion
	       (if (re-search-forward "\n[[:space:]]*\n\\|\\''" nil t)
		   (match-beginning 0)
		 (point-max)))))
    (cons beg end)))

(defun supreme-brackets--word-boundaries-at-point ()
  "Find word boundaries at point.
A word consists of alphanumeric characters, hyphens, and underscores.
This works well for variable names in Python, PHP, JavaScript, Go, etc.
Returns a cons cell (BEG . END)."
  (let ((beg (save-excursion
	       (skip-chars-backward "-_[:alnum:]")
	       (point)))
	(end (save-excursion
	       (skip-chars-forward "-_[:alnum:]")
	       (point))))
    (cons beg end)))

(defun supreme-brackets--at-word-boundary-p ()
  "Return non-nil if point is at the end of a word or at buffer end."
  (or (looking-at "[^-_[:alnum:]]")
      (eobp)))

;;; Core Function

(defun supreme-brackets-wrap-with-brackets (open-bracket close-bracket &optional wrap-style)
  "Insert OPEN-BRACKET and CLOSE-BRACKET around text intelligently.

OPEN-BRACKET and CLOSE-BRACKET are strings to insert.
WRAP-STYLE can be:
  nil       - Use smart contextual wrapping (default)
  'line     - Wrap the entire current line
  'block    - Wrap the current block (text between blank lines)

Behavior when WRAP-STYLE is nil:
  • If region is active, wrap around the region
  • If at beginning of non-empty line, wrap the entire line
  • If at end of word in non-Lisp mode, insert empty brackets
  • Otherwise, wrap around the word at point or insert empty brackets

After insertion, point is positioned after the opening bracket.

Works intelligently with all programming languages including Python, PHP,
TypeScript, JavaScript, Go, Rust, SQL, and many others."
  (let (beg end offset)
    (cond
     ;; Case 1: Active region - wrap it
     ((use-region-p)
      (setq beg (region-beginning)
	    end (region-end))
      (goto-char end)
      (insert close-bracket)
      (goto-char beg)
      (insert open-bracket)
      (setq offset (length open-bracket)))

     ;; Case 2: Explicit line wrap requested
     ((eq wrap-style 'line)
      (setq beg (line-beginning-position)
	    end (line-end-position))
      (goto-char end)
      (insert close-bracket)
      (goto-char beg)
      (insert open-bracket)
      (setq offset (length open-bracket)))

     ;; Case 3: Explicit block wrap requested
     ((eq wrap-style 'block)
      (let ((bounds (supreme-brackets--find-block-boundaries)))
	(setq beg (car bounds)
	      end (cdr bounds))
	(goto-char end)
	(insert close-bracket)
	(goto-char beg)
	(insert open-bracket)
	(setq offset (length open-bracket))))

     ;; Case 4: At beginning of non-empty line - wrap line
     ((and (bolp)
	   (not (eolp)))
      (setq beg (point))
      (insert open-bracket)
      (end-of-line)
      (insert close-bracket)
      (goto-char (+ beg (length open-bracket))))
     ;; No 'offset' set; this case handles its own cursor.

     ;; Case 5: At word boundary in non-Lisp mode - insert empty brackets
     ;; This is the common case for Python, JS, PHP, Go, etc.
     ((and (supreme-brackets--at-word-boundary-p)
	   (not (supreme-brackets--in-lisp-mode-p)))
      (setq beg (point))
      (insert open-bracket close-bracket)
      (goto-char (+ beg (length open-bracket))))
     ;; No 'offset' set; this case handles its own cursor.

     ;; Case 6: Default - wrap word at point or insert empty brackets
     (t
      (let ((bounds (supreme-brackets--word-boundaries-at-point)))
	(setq beg (car bounds)
	      end (cdr bounds))
	(goto-char end)
	(insert close-bracket)
	(goto-char beg)
	(insert open-bracket)
	(setq offset (length open-bracket)))))

    ;; Position cursor after opening bracket (for cases 1, 2, 3, 6)
    (when offset
      (goto-char (+ beg offset)))))

;;; Interactive Commands - Basic ASCII Brackets

;;;###autoload
(defun supreme-brackets-insert-parentheses ()
  "Insert or wrap with parentheses ().
Great for function calls in Python, JavaScript, PHP, Go, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "(" ")"))

;;;###autoload
(defun supreme-brackets-insert-square-brackets ()
  "Insert or wrap with square brackets [].
Perfect for array/list indexing in Python, PHP, JavaScript, TypeScript, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "[" "]"))

;;;###autoload
(defun supreme-brackets-insert-curly-braces ()
  "Insert or wrap with curly braces {}.
Essential for blocks in C, C++, Java, JavaScript,
TypeScript, Go, Rust, PHP, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "{" "}"))

;;;###autoload
(defun supreme-brackets-insert-angle-brackets ()
  "Insert or wrap with angle brackets <>.
Useful for generics in TypeScript, Java, C++, Rust, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "<" ">"))

;;; Interactive Commands - Quotes (Essential for all languages)

;;;###autoload
(defun supreme-brackets-insert-double-quotes ()
  "Insert or wrap with double quotes \"\".
Standard string delimiter in most languages: Python, JavaScript, PHP, Go, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "\"" "\""))

;;;###autoload
(defun supreme-brackets-insert-single-quotes ()
  "Insert or wrap with single quotes ''.
String delimiter in Python, JavaScript, SQL, PHP, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "'" "'"))

;;;###autoload
(defun supreme-brackets-insert-backticks ()
  "Insert or wrap with backticks ``.
Template literals in JavaScript/TypeScript, command
substitution in shell scripts."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "`"))

;;;###autoload
(defun supreme-brackets-insert-emacs-quotes ()
  "Insert or wrap with Emacs-style quotes `'.
For documentation and quoting in Elisp."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "'"))

;;; Interactive Commands - Markdown

;;;###autoload
(defun supreme-brackets-insert-markdown-code-inline ()
  "Insert or wrap with single backticks for inline code.
Standard Markdown inline code syntax."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "`"))

;;;###autoload
(defun supreme-brackets-insert-markdown-code-block ()
  "Insert or wrap with Markdown triple backticks.
Creates a code block in Markdown, perfect for documentation."
  (interactive)
  (supreme-brackets-wrap-with-brackets "```\n" "\n```"))

;;;###autoload
(defun supreme-brackets-insert-python-docstring ()
  "Insert or wrap with Python triple double-quotes.
Standard Python docstring format."
  (interactive)
  (supreme-brackets-wrap-with-brackets "\"\"\"" "\"\"\""))

;;;###autoload
(defun supreme-brackets-insert-python-triple-single-quotes ()
  "Insert or wrap with Python triple single-quotes.
Alternative Python string/docstring format."
  (interactive)
  (supreme-brackets-wrap-with-brackets "'''" "'''"))

;;; Interactive Commands - Language-Specific

;;;###autoload
(defun supreme-brackets-insert-template-literal ()
  "Insert or wrap with JavaScript/TypeScript template literal syntax.
Uses backticks with ${} placeholders."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "`"))

;;;###autoload
(defun supreme-brackets-insert-rust-raw-string ()
  "Insert or wrap with Rust raw string syntax r#\"...\"#."
  (interactive)
  (supreme-brackets-wrap-with-brackets "r#\"" "\"#"))

;;;###autoload
(defun supreme-brackets-insert-go-raw-string ()
  "Insert or wrap with Go raw string literal (backticks).
Multi-line strings in Go."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "`"))

;;;###autoload
(defun supreme-brackets-insert-sql-identifier ()
  "Insert or wrap with SQL identifier quotes (backticks).
For MySQL column/table names with special characters."
  (interactive)
  (supreme-brackets-wrap-with-brackets "`" "`"))

;;;###autoload
(defun supreme-brackets-insert-postgres-identifier ()
  "Insert or wrap with PostgreSQL identifier quotes.
For PostgreSQL case-sensitive or special identifiers."
  (interactive)
  (supreme-brackets-wrap-with-brackets "\"" "\""))

;;;###autoload
(defun supreme-brackets-insert-php-variable ()
  "Insert $ and wrap for PHP variable.
Useful for quickly creating PHP variables."
  (interactive)
  (supreme-brackets-wrap-with-brackets "$" ""))

;;; Interactive Commands - Curly/Smart Quotes

;;;###autoload
(defun supreme-brackets-insert-curly-double-quotes ()
  "Insert or wrap with curly double quotes “”.
Typographically correct quotes for prose."
  (interactive)
  (supreme-brackets-wrap-with-brackets "“" "”"))

;;;###autoload
(defun supreme-brackets-insert-curly-single-quotes ()
  "Insert or wrap with curly single quotes ‘’.
Typographically correct single quotes."
  (interactive)
  (supreme-brackets-wrap-with-brackets "‘" "’"))

;;;###autoload
(defun supreme-brackets-insert-single-guillemets ()
  "Insert or wrap with single guillemets ‹›.
European quotation marks."
  (interactive)
  (supreme-brackets-wrap-with-brackets "‹" "›"))

;;;###autoload
(defun supreme-brackets-insert-double-guillemets ()
  "Insert or wrap with double guillemets «».
French/European quotation marks."
  (interactive)
  (supreme-brackets-wrap-with-brackets "«" "»"))

;;; Interactive Commands - CJK Brackets

;;;###autoload
(defun supreme-brackets-insert-corner-brackets ()
  "Insert or wrap with corner brackets 「」.
Japanese quotation marks."
  (interactive)
  (supreme-brackets-wrap-with-brackets "「" "」"))

;;;###autoload
(defun supreme-brackets-insert-white-corner-brackets ()
  "Insert or wrap with white corner brackets 『』.
Japanese emphasis quotation marks."
  (interactive)
  (supreme-brackets-wrap-with-brackets "『" "』"))

;;;###autoload
(defun supreme-brackets-insert-cjk-angle-brackets ()
  "Insert or wrap with CJK angle brackets 〈〉.
Chinese/Japanese angle brackets."
  (interactive)
  (supreme-brackets-wrap-with-brackets "〈" "〉"))

;;;###autoload
(defun supreme-brackets-insert-cjk-double-angle-brackets ()
  "Insert or wrap with CJK double angle brackets 《》.
Chinese book title marks."
  (interactive)
  (supreme-brackets-wrap-with-brackets "《" "》"))

;;;###autoload
(defun supreme-brackets-insert-white-lenticular-brackets ()
  "Insert or wrap with white lenticular brackets 〖〗."
  (interactive)
  (supreme-brackets-wrap-with-brackets "〖" "〗"))

;;;###autoload
(defun supreme-brackets-insert-black-lenticular-brackets ()
  "Insert or wrap with black lenticular brackets 【】.
Chinese emphasis brackets."
  (interactive)
  (supreme-brackets-wrap-with-brackets "【" "】"))

;;;###autoload
(defun supreme-brackets-insert-tortoise-shell-brackets ()
  "Insert or wrap with tortoise shell brackets 〔〕.
Chinese annotation brackets."
  (interactive)
  (supreme-brackets-wrap-with-brackets "〔" "〕"))

;;; Interactive Commands - Decorative Brackets

;;;###autoload
(defun supreme-brackets-insert-heavy-angle-brackets ()
  "Insert or wrap with heavy angle quotation mark ❮❯."
  (interactive)
  (supreme-brackets-wrap-with-brackets "❮" "❯"))

;;;###autoload
(defun supreme-brackets-insert-heavy-angle-ornament-brackets ()
  "Insert or wrap with heavy angle ornament brackets ❰❱."
  (interactive)
  (supreme-brackets-wrap-with-brackets "❰" "❱"))

;;; Line and Block Wrapping Commands

;;;###autoload
(defun supreme-brackets-wrap-line-with-parentheses ()
  "Wrap current line with parentheses.
Useful for wrapping function arguments, etc."
  (interactive)
  (supreme-brackets-wrap-with-brackets "(" ")" 'line))

;;;###autoload
(defun supreme-brackets-wrap-block-with-parentheses ()
  "Wrap current block with parentheses."
  (interactive)
  (supreme-brackets-wrap-with-brackets "(" ")" 'block))

;;;###autoload
(defun supreme-brackets-wrap-line-with-braces ()
  "Wrap current line with curly braces.
Useful for quickly adding block structure in C-style languages."
  (interactive)
  (supreme-brackets-wrap-with-brackets "{" "}" 'line))

;;;###autoload
(defun supreme-brackets-wrap-block-with-braces ()
  "Wrap current block with curly braces."
  (interactive)
  (supreme-brackets-wrap-with-brackets "{" "}" 'block))


;;; Navigation

(defvar supreme-brackets-nav-bracket-pairs
  '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›"
    "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "㘘〙" "｢｣" "⟦⟧"
    "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵"
    "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌"
    "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
  "A list of 2-character strings, each defining a bracket pair.
Used by `supreme-brackets-nav-goto-matching-bracket'.")

(defconst supreme-brackets-nav-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) supreme-brackets-nav-bracket-pairs)
  "List of left bracket strings, derived from `supreme-brackets-nav-bracket-pairs'.")

(defconst supreme-brackets-nav-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) supreme-brackets-nav-bracket-pairs)
  "List of right bracket strings, derived from `supreme-brackets-nav-bracket-pairs'.")

;;;###autoload
(defun supreme-brackets-nav-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets is defined by `supreme-brackets-nav-left-brackets' and
`supreme-brackets-nav-right-brackets'."
  (interactive)
  (if (nth 3 (syntax-ppss)) ; In a string
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ;; Handle standard double quotes
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ;; Handle all other brackets from our list
     ((looking-at (regexp-opt supreme-brackets-nav-left-brackets))
      (forward-sexp))
     ((prog2 (backward-char) (looking-at (regexp-opt supreme-brackets-nav-right-brackets)) (forward-char))
      (backward-sexp))
     ;; Default fallback
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

;;;###autoload
(defun supreme-brackets-nav-backward-left-bracket ()
  "Move cursor to the previous occurrence of a left bracket.
The list of brackets is defined by `supreme-brackets-nav-left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt supreme-brackets-nav-left-brackets) nil t))

;;;###autoload
(defun supreme-brackets-nav-forward-right-bracket ()
  "Move cursor to the next occurrence of a right bracket.
The list of brackets is defined by `supreme-brackets-nav-right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt supreme-brackets-nav-right-brackets) nil t))

;;;###autoload
(defun supreme-brackets-nav-select-text-in-delimiters ()
  "Select text between the nearest left and right delimiters.
Delimiters include \" and `, and all pairs in `supreme-brackets-nav-bracket-pairs'.
This command ignores nesting."
  (interactive)
  (let ((delimiter-chars (concat "^\"`" (mapconcat #'identity supreme-brackets-nav-bracket-pairs ""))))
    ;; 1. Go backward until we hit a delimiter
    (skip-chars-backward delimiter-chars)
    (push-mark (point) t t)
    ;; 2. Go forward until we hit a delimiter
    ;;    (Use substring to remove the '^' from the char set)
    ;; --- FIX ---  <-- This was the stray comment I mentioned
    ;; Use the full 'delimiter-chars' string to skip NON-delimiters
    (skip-chars-forward delimiter-chars)))


;;; Deletion

;;;###autoload
(defun supreme-brackets-delete-forward-sexp-or-pairs (pairs-only-p)
  "Delete matching brackets to the right of point.

If PAIRS-ONLY-P is nil (default), delete the brackets and the
text inside (kills the whole S-expression).
If PAIRS-ONLY-P is non-nil, delete only the surrounding brackets.

Assumes point is just before an opening bracket or quote."
  (interactive "P")
  (if pairs-only-p
      ;; Delete pairs only
      (let ((start-point (point)))
	(forward-sexp)
	(delete-char -1)
	(push-mark (point) t)
	(goto-char start-point)
	(delete-char 1))
    ;; Delete entire sexp
    (progn
      (mark-sexp)
      (kill-region (region-beginning) (region-end)))))

;;;###autoload
(defun supreme-brackets-delete-backward-sexp ()
  "Delete matching S-expression to the left of point, including inner text.
Assumes point is just after a closing bracket or quote."
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

;;;###autoload
(defun supreme-brackets-delete-backward-pairs ()
  "Delete matching brackets/quotes to the left of point, leaving inner text.
After call, mark is set at the matching bracket position.
Assumes point is just after a closing bracket or quote."
  (interactive)
  (let ((end-point (point))
	start-point)
    (forward-sexp -1)
    (setq start-point (point))
    (goto-char end-point)
    (delete-char -1)
    (goto-char start-point)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- end-point 2))))

;;;###autoload
(defun supreme-brackets-delete-smart-backward (&optional arg)
  "Delete 1 char, or delete quote/bracket pair and inner text.

If char to the left of point is a closing bracket, delete it
and its matching opener, along with the inner text.
With a prefix argument delete only the brackets (ARG non-nil).

If char to the left is an opening bracket or quote, behaves
similarly but may also handle comment-aware deletion.

When called interactively with prefix argument, ARG is non-nil.
When called from Lisp, pass non-nil ARG to delete only brackets."
  (interactive "P")
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    ;; --- FIX: Use (char-before) for robust checking ---
    (let ((char-before (and (> (point) (point-min)) (char-before))))
      (cond
       ;; Case 1: Point is after a closing bracket. e.g. (a)▮
       ((and char-before (string-match-p (regexp-opt supreme-brackets-nav-right-brackets) (string char-before)))
	(if arg ; <-- Changed from prefix-arg to arg
	    (supreme-brackets-delete-backward-pairs)
	  (supreme-brackets-delete-backward-sexp)))

       ;; Case 2: Point is after an opening bracket. e.g. (▮a)
       ((and char-before (string-match-p (regexp-opt supreme-brackets-nav-left-brackets) (string char-before)))
	(let (start-point
	      (end-point (point))
	      is-comment)
	  (backward-char)
	  (setq start-point (point))
	  (goto-char end-point)
	  (forward-char)
	  (setq is-comment (nth 4 (syntax-ppss)))
	  (if is-comment
	      (progn
		(goto-char start-point)
		(if (forward-comment 1)
		    (kill-region (point) start-point)
		  (message "Error: Failed to parse comment.")))
	    (progn
	      (goto-char start-point) ; Go to just before opening bracket
	      (forward-sexp) ; Move to end of sexp (a)▮
	      ;; Point is at end, so we call backward delete
	      (if arg ; <-- Changed from prefix-arg to arg
		  (supreme-brackets-delete-backward-pairs)
		(supreme-brackets-delete-backward-sexp))))))

       ;; Case 3: Point is after a quote. e.g. "a"▮ or ▮"a"
       ((and char-before (string-match-p "[\"']" (string char-before)))
	(if (nth 3 (syntax-ppss)) ; Inside a string? e.g. "a▮"
	    (progn
	      (backward-char) ; Move to ▮"a"
	      ;; With prefix, delete pairs only.
	      (supreme-brackets-delete-forward-sexp-or-pairs arg)) ; <-- Changed from prefix-arg to arg
	  ;; Not in a string. e.g. "a"▮
	  (if arg ; <-- Changed from prefix-arg to arg
	      (supreme-brackets-delete-backward-pairs)
	    (supreme-brackets-delete-backward-sexp))))

       ;; Default case:
       (t
	(delete-char -1))))))


;;; Replacement

(defconst supreme-brackets-replace--change-bracket-pairs-list
  '(
    "square [ ]"
    "brace { }"
    "paren ( )"
    "less greater than < >"
    "QUOTATION MARK \" \""
    "APOSTROPHE ' '"
    "emacs ` '"
    "GRAVE ACCENT ` `"
    "double square [[ ]]"
    "tilde ~ ~"
    "equal = ="
    "double curly quote “ ”"
    "single curly quote ‘ ’"
    "french angle quote ‹ ›"
    "french double angle « »"
    "corner 「 」"
    "white corner 『 』"
    "lenticular 【 】"
    "white lenticular 〖 〗"
    "title angle 〈 〉"
    "double angle 《 》"
    "tortoise 〔 〕"
    "white tortoise 〘 〙"
    "white square 〚 〛"
    "white paren ⦅ ⦆"
    "white curly bracket ⦃ ⦄"
    "pointing angle 〈 〉"
    "angle with dot ⦑ ⦒"
    "curved angle ⧼ ⧽"
    "math square ⟦ ⟧"
    "math angle ⟨ ⟩"
    "math double angle ⟪ ⟫"
    "math flattened parenthesis ⟮ ⟯"
    "math white tortoise shell ⟬ ⟭"
    "heavy single quotation mark ornament ❛ ❜"
    "heavy double turned comma quotation mark ornament ❝ ❞"
    "medium parenthesis ornament ❨ ❩"
    "medium flattened parenthesis ornament ❪ ❫"
    "medium curly ornament ❴ ❵"
    "medium pointing angle ornament ❬ ❭"
    "heavy pointing angle quotation mark ornament ❮ ❯"
    "heavy pointing angle ornament ❰ ❱"
    "none (delete pairs)")
  "A list of bracket pair definitions for `supreme-brackets-change-bracket-pairs'.")

;;;###autoload
(defun supreme-brackets-replace-change-bracket-pairs (from-chars to-chars)
  "Change bracket pairs to another type or delete them.
Works on the current block (text between blank lines) or active region.

FROM-CHARS and TO-CHARS are strings selected from a prompt.
The last two space-separated words are used as the bracket pairs.
If TO-CHARS is \"none...\", the brackets are replaced with empty strings."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (completing-read "Replace this:" supreme-brackets--change-bracket-pairs-list nil t nil nil (car supreme-brackets--change-bracket-pairs-list))
      (completing-read "To:" supreme-brackets--change-bracket-pairs-list nil t nil nil (car (last supreme-brackets--change-bracket-pairs-list))))))

  (let* ((bounds (if (use-region-p)
		     (cons (region-beginning) (region-end))
		   (supreme-brackets--find-block-boundaries)))
	 (xbeg (car bounds))
	 (xend (cdr bounds))
	 (xs-from (last (split-string from-chars " ") 2))
	 (xleft (car xs-from))
	 (xright (car (cdr xs-from)))
	 xtol xtor)

    ;; Correctly set replacement strings, handling the "none" case
    (if (string-prefix-p "none" to-chars)
	(setq xtol "" xtor "")
      (let ((xs-to (last (split-string to-chars " ") 2)))
	(setq xtol (car xs-to)
	      xtor (car (cdr xs-to)))))

    (save-excursion
      (save-restriction
	(narrow-to-region xbeg xend)
	(let ((case-fold-search nil))
	  (if (string-equal xleft xright)
	      ;; Symmetric pairs (e.g., " " or ` `)
	      (let ((re-quoted-bracket (regexp-quote xleft)))
		(goto-char (point-min))
		(while
		    (re-search-forward
		     ;; --- BUG FIX ---
		     ;; Use non-greedy match-anything instead of negated char class
		     (format "%s\\(.*?\\)%s"
			     re-quoted-bracket
			     re-quoted-bracket)
		     nil t)
		  (let ((matched-text (match-string 1)))
		    (replace-match (concat xtol matched-text xtor) t t))))
	    ;; Asymmetric pairs (e.g., ( ) or [ ])
	    (progn
	      (goto-char (point-min))
	      (while (search-forward xleft nil t)
		(replace-match xtol t t))
	      (goto-char (point-min))
	      (while (search-forward xright nil t)
		(replace-match xtor t t)))))))))


;;; Keybindings

;;;###autoload
(defun supreme-brackets-setup-default-keybindings ()
  "Set up default keybindings for bracket insertion."
  (interactive)
  ;; Binds s-(, s-{, s-[
  (global-set-key (kbd "s-(") #'supreme-brackets-insert-parentheses)
  (global-set-key (kbd "s-{") #'supreme-brackets-insert-curly-braces)
  (global-set-key (kbd "s-[") #'supreme-brackets-insert-square-brackets))

;;;###autoload
(defun supreme-brackets-setup-extended-keybindings ()
  "Set up extended keybindings for different operations:
insertion, navigation, deletion, and replacement."
  (interactive)

  ;; 1. Setting up the default bindings (s-(, s-[, s-{)
  (supreme-brackets-setup-default-keybindings)

  ;; 2. Setting up the 's-i' (insert) prefix map
  (let ((insert-map (make-sparse-keymap)))
    ;; Binding the prefix key 's-i' to our new map
    (global-set-key (kbd "s-i") insert-map)

    ;; Defining the keys *within* that map
    (define-key insert-map (kbd "'") #'supreme-brackets-insert-single-quotes)
    (define-key insert-map (kbd "\"") #'supreme-brackets-insert-double-quotes)
    (define-key insert-map (kbd "`") #'supreme-brackets-insert-backticks)
    (define-key insert-map (kbd "<") #'supreme-brackets-insert-angle-brackets)
    (define-key insert-map (kbd "e") #'supreme-brackets-insert-emacs-quotes)
    (define-key insert-map (kbd "m") #'supreme-brackets-insert-markdown-code-block)
    (define-key insert-map (kbd "3") #'supreme-brackets-insert-python-docstring)
    (define-key insert-map (kbd "$") #'supreme-brackets-insert-php-variable))

  ;; 3. Setting up the 's-b' (brackets) prefix map for editing
  (let ((bracket-map (make-sparse-keymap)))
    ;; Binding the prefix key 's-b' to our new map
    (global-set-key (kbd "s-b") bracket-map)

    ;; --- Deletion ---
    (define-key bracket-map (kbd "d") #'supreme-brackets-delete-smart-backward)
    (define-key bracket-map (kbd "k") #'supreme-brackets-delete-backward-pairs)

    ;; --- Navigation ---
    (define-key bracket-map (kbd "m") #'supreme-brackets-nav-goto-matching-bracket)
    (define-key bracket-map (kbd "s") #'supreme-brackets-nav-select-text-in-delimiters)
    (define-key bracket-map (kbd "l") #'supreme-brackets-nav-backward-left-bracket)
    (define-key bracket-map (kbd "r") #'supreme-brackets-nav-forward-right-bracket)

    ;; --- Replacement ---
    (define-key bracket-map (kbd "c") #'supreme-brackets-replace-change-bracket-pairs)))

(provide 'supreme-brackets)

;;; supreme-brackets.el ends here
