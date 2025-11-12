;;; supreme-brackets-test.el --- Tests for Supreme Brackets -*- lexical-binding: t -*-

;; Author: Javier Miranda
;; Version: 1.0.0
;; Keywords: convenience, editing, brackets
;; URL: https://github.com/J4VMC/supreme-brackets
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

;; To run these tests:
;; 1. Make sure 'supreme-brackets.el' is in your load-path.
;; 2. Run 'M-x load-file' and load this file.
;; 3. Run 'M-x ert' and run all tests for 'supreme-brackets'.

;;; Code:

(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (when (and current-dir (not (member current-dir load-path)))
    (add-to-list 'load-path current-dir)))

(require 'ert)
(require 'supreme-brackets)


;;; Test Helper Macro

(defmacro supreme-brackets-test-with-mode (mode &rest body)
  "Run BODY in a temp buffer with MODE."
  `(with-temp-buffer
     (funcall ,mode)  ; Actually activate the mode properly
     ,@body))

;;;; Core Tests (supreme-brackets.el)

(ert-deftest supreme-brackets-test-wrap-region ()
  "Test wrapping an active region."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "hello")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max)) ; Select "hello"
				   (supreme-brackets-wrap-with-brackets "(" ")")
				   (should (string= (buffer-string) "(hello)"))
				   (should (= (point) 2)))) ; Point should be at "(▮hello)" which is position 2

(ert-deftest supreme-brackets-test-wrap-word-at-point ()
  "Test wrapping the word at point."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "pre hello post")
				   (goto-char 9) ; "pre he▮llo post" (9 is in middle of "hello")
				   (supreme-brackets-wrap-with-brackets "[" "]")
				   (should (string= (buffer-string) "pre [hello] post"))
				   (should (= (point) 6)))) ; "pre [▮hello] post"

(ert-deftest supreme-brackets-test-wrap-word-with-hyphen-underscore ()
  "Test wrapping word at point with special characters."
  (supreme-brackets-test-with-mode 'python-mode
				   (insert "my-variable my_variable")
				   (goto-char 4) ; "my-▮variable"
				   (supreme-brackets-wrap-with-brackets "(" ")")
				   (should (string= (buffer-string) "(my-variable) my_variable"))
				   (goto-char 18) ; "my_v▮ariable"
				   (supreme-brackets-wrap-with-brackets "[" "]")
				   (should (string= (buffer-string) "(my-variable) [my_variable]"))
				   (should (= (point) 16)))) ; "... [▮my_variable]"

(ert-deftest supreme-brackets-test-insert-empty-pair-non-lisp ()
  "Test inserting empty pair at end of word (non-Lisp mode)."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "hello")
				   (goto-char (point-max)) ; "hello▮"
				   (supreme-brackets-wrap-with-brackets "(" ")")
				   (should (string= (buffer-string) "hello()"))
				   (should (= (point) 7)))) ; "hello(▮)"

(ert-deftest supreme-brackets-test-wrap-word-lisp-mode ()
  "Test wrapping word at end of word (Lisp mode)."
  (supreme-brackets-test-with-mode 'emacs-lisp-mode
				   (insert "hello")
				   (goto-char (point-max)) ; "hello▮"
				   (supreme-brackets-wrap-with-brackets "(" ")")
				   (should (string= (buffer-string) "(hello)"))
				   (should (= (point) 2)))) ; "(▮hello)"

(ert-deftest supreme-brackets-test-wrap-line-at-bol ()
  "Test wrapping a line when at the beginning."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "this is a line")
				   (goto-char (point-min)) ; "▮this is a line"
				   (supreme-brackets-wrap-with-brackets "{" "}")
				   (should (string= (buffer-string) "{this is a line}"))
				   (should (= (point) 2)))) ; "{▮this is a line}"

(ert-deftest supreme-brackets-test-wrap-style-line-and-block ()
  "Test explicit 'line and 'block wrap styles."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "line one\n\nline two\nline three")
				   (goto-char 4) ; "lin▮e one..."
				   (supreme-brackets-wrap-with-brackets "[" "]" 'line)
				   (should (string= (buffer-string) "[line one]\n\nline two\nline three"))
				   (goto-char 16) ; In "line two" block
				   (supreme-brackets-wrap-with-brackets "{" "}" 'block)
				   (should (string= (buffer-string) "[line one]\n\n{line two\nline three}"))))

(ert-deftest supreme-brackets-test-wrap-asymmetric-and-one-sided ()
  "Test wrappers with asymmetric or single-sided brackets."
  (supreme-brackets-test-with-mode 'emacs-lisp-mode
				   ;; Test 1: Asymmetric (emacs-quotes)
				   (erase-buffer)
				   (insert "hello")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max)) ; Select "hello"
				   (supreme-brackets-insert-emacs-quotes)
				   (should (string= (buffer-string) "`hello'"))
				   (should (= (point) 2)) ; "`▮hello'"

				   ;; Test 2: One-sided (php-variable)
				   (erase-buffer)
				   (insert "varname")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max)) ; Select "varname"
				   (supreme-brackets-insert-php-variable)
				   (should (string= (buffer-string) "$varname"))
				   (should (= (point) 2)))) ; "$▮varname"

;;;; Deletion Tests (supreme-brackets-delete.el)

(ert-deftest supreme-brackets-test-smart-delete-backward-cases ()
  "Test supreme-brackets-smart-delete-backward in various contexts."
  (supreme-brackets-test-with-mode 'python-mode
				   ;; Test 1: Default char delete
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "abc")
				   (goto-char (point-max))
				   (supreme-brackets-delete-smart-backward)
				   (should (string= (buffer-string) "ab"))

				   ;; Test 2: Sexp delete (point after closing bracket)
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "ab(def)")
				   (goto-char (point-max))
				   (supreme-brackets-delete-smart-backward)
				   (should (string= (buffer-string) "ab"))

				   ;; Test 3: Pairs-only delete (point after closing bracket)
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "ab(ghi)")
				   (goto-char (point-max))
				   (supreme-brackets-delete-smart-backward t)
				   (should (string= (buffer-string) "abghi"))

				   ;; Test 4: Sexp delete (point after opening bracket) e.g. (▮a)
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "(hello)")
				   (goto-char 2) ; "(▮hello)" - point 2 is after the opening paren
				   (supreme-brackets-delete-smart-backward)
				   (should (string= (buffer-string) ""))

				   ;; Test 5: Pairs-only delete (point after opening bracket)
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "(world)")
				   (goto-char 2) ; "(▮world)" - point 2 is after the opening paren
				   (supreme-brackets-delete-smart-backward t) ; t = pairs-only
				   (should (string= (buffer-string) "world"))

				   ;; Test 6: Sexp delete (point inside string after opening quote) e.g. "▮a"
				   (erase-buffer)
				   (deactivate-mark)
				   (insert "\"foo\"")
				   (goto-char 2) ; "\"▮foo\"" - point 2 is after the opening quote
				   (supreme-brackets-delete-smart-backward)
				   (should (string= (buffer-string) ""))))

(ert-deftest supreme-brackets-test-delete-forward ()
  "Test supreme-brackets-delete-forward-sexp-or-pairs."
  (supreme-brackets-test-with-mode 'text-mode
				   ;; Test 1: Sexp delete
				   (erase-buffer)
				   (insert "(hello)")
				   (goto-char (point-min)) ; "▮(hello)"
				   (supreme-brackets-delete-forward-sexp-or-pairs nil) ; nil = not pairs-only
				   (should (string= (buffer-string) ""))

				   ;; Test 2: Pairs-only delete
				   (erase-buffer)
				   (insert "(world)")
				   (goto-char (point-min)) ; "▮(world)"
				   (supreme-brackets-delete-forward-sexp-or-pairs t) ; t = pairs-only
				   (should (string= (buffer-string) "world"))))

;;;; Navigation Tests (supreme-brackets-nav.el)

(ert-deftest supreme-brackets-test-goto-matching-bracket ()
  "Test supreme-brackets-goto-matching-bracket."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "(a [b] c)")
				   (goto-char 1) ; "▮(a [b] c)" (point 1 is before first char)
				   (supreme-brackets-nav-goto-matching-bracket)
				   (should (= (point) 10)) ; "(a [b] c)▮" (after last paren)
				   (supreme-brackets-nav-goto-matching-bracket)
				   (should (= (point) 1)) ; "▮(a [b] c)"

				   (goto-char 4) ; "(a ▮[b] c)"
				   (supreme-brackets-nav-goto-matching-bracket)
				   (should (= (point) 7)))) ; "(a [b]▮ c)"

(ert-deftest supreme-brackets-test-select-delimiters-correct ()
  "Test supreme-brackets-select-text-in-delimiters."
  (supreme-brackets-test-with-mode 'text-mode
				   ;; This tests the intended behavior based on the code's implementation
				   ;; (which selects text *between* the nearest delimiters)
				   (insert "foo(bar)baz")
				   (goto-char 6) ; "foo(ba▮r)baz"
				   (supreme-brackets-nav-select-text-in-delimiters)
				   ;; skip-backward stops at '(', point is 5
				   ;; mark is 5
				   ;; skip-forward stops at ')', point is 8
				   (should (string= (buffer-substring (region-beginning) (region-end)) "bar"))
				   (should (= (mark) 5))
				   (should (= (point) 8))))

(ert-deftest supreme-brackets-test-search-brackets ()
  "Test supreme-brackets-backward-left-bracket and supreme-brackets-forward-right-bracket."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "(a) [b] {c}")
				   (goto-char (point-max))
				   (supreme-brackets-nav-backward-left-bracket)
				   (should (= (point) 9)) ; "(a) [b] ▮{c}" (before {)
				   (supreme-brackets-nav-backward-left-bracket)
				   (should (= (point) 5)) ; "(a) ▮[b] {c}" (before [)
				   (supreme-brackets-nav-forward-right-bracket)
				   (should (= (point) 8)) ; "(a) [b]▮ {c}" (after ])
				   (supreme-brackets-nav-forward-right-bracket)
				   (should (= (point) 12)))) ; "(a) [b] {c}▮" (after })

;;;; Replacement Tests (supreme-brackets-replace.el)

(ert-deftest supreme-brackets-test-change-bracket-pairs-asymmetric ()
  "Test supreme-brackets-change-bracket-pairs for () -> []."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "foo(bar)baz")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max))
				   (activate-mark)
				   (supreme-brackets-replace-change-bracket-pairs "paren ( )" "square [ ]")
				   (should (string= (buffer-string) "foo[bar]baz"))))

(ert-deftest supreme-brackets-test-change-bracket-pairs-symmetric ()
  "Test supreme-brackets-change-bracket-pairs for \"\" -> '' (tests regex fix)."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "foo\"bar\"baz")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max))
				   (activate-mark)
				   (supreme-brackets-replace-change-bracket-pairs "QUOTATION MARK \" \"" "APOSTROPHE ' '")
				   (should (string= (buffer-string) "foo'bar'baz"))))

(ert-deftest supreme-brackets-test-change-bracket-pairs-none ()
  "Test supreme-brackets-change-bracket-pairs for () -> 'none'."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "foo(bar)baz")
				   (goto-char (point-min))
				   (set-mark (point))
				   (goto-char (point-max))
				   (activate-mark)
				   (supreme-brackets-replace-change-bracket-pairs "paren ( )" "none (delete pairs)")
				   (should (string= (buffer-string) "foobarbaz"))))

(ert-deftest supreme-brackets-test-change-bracket-pairs-no-region ()
  "Test change-bracket-pairs using block boundaries (no region)."
  (supreme-brackets-test-with-mode 'text-mode
				   (insert "first line\n\nfoo(bar)baz\n(more)\n\nlast line")
				   (goto-char 18) ; "foo(ba▮r)baz..."
				   (deactivate-mark)
				   (supreme-brackets-replace-change-bracket-pairs "paren ( )" "square [ ]")
				   ;; Should only change the block "foo[bar]baz\n[more]"
				   (should (string= (buffer-string) "first line\n\nfoo[bar]baz\n[more]\n\nlast line"))))

(provide 'supreme-brackets-test)

;;; supreme-brackets-test.el ends here
