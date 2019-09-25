;;; ma-pytest.el --- minor mode for working with meilleursAgents pytest tests.

;; Copyright (C) 2019 Julien Paille

;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defun execute-test(name command args)
  "This is the main entry point for sub-processes.
It creates a comint interaction buffer, called `name', running
`command', called with `args'"
  (setq *last-test-args* args)
  (setq *last-python-interpreter* command)
  (ansi-color-for-comint-mode-on)
  (apply 'make-comint name command nil args)
  )

;;;;;;;;;;;; BUILD TEST STRING

(defun pytest-inner-testable ()
  "Find the function name for `pytest-one'."
  (save-excursion
    (re-search-backward
     "^[ \t]\\{0,4\\}\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun pytest-outer-testable ()
  "Find the class for the `pytest-one'."
  (save-excursion
    (re-search-backward
     "^\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun pytest-py-testable ()
  "Create a path to a test.
This uses the `::` delimiter between the
filename, class and method in order to find the specific test
case.  This requires pytest >= 1.2."
  (let* ((inner-obj (pytest-inner-testable))
         (outer (pytest-outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (concat
     (buffer-file-name)
     (cond ((equal outer-def "def") (format "::%s" outer-obj))
       ((equal inner-obj outer-obj) (format "::%s" outer-obj))
       (t (format "::%s::%s" outer-obj inner-obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-pytest ()
  "Run the test(s) given by `command'."
  (setq command (pytest-py-testable))
  (refresh-buffer)
  (setenv pytest-venv-key pytest-venv-value)
  (if (eq current-prefix-arg nil)
      (execute-test "ponytests" pytest-binary (append (list command) (split-string pytest-args)))
    ;; optional command
    )
  )

(defun refresh-buffer()
  (let ((buffer (get-buffer "*ponytests*")))
    (when buffer
      (save-excursion
        (pop-to-buffer buffer)
        (erase-buffer))))
  )

(defun run-pytest-file ()
  "Run the test(s) given by `command'."
  (setq command (buffer-file-name))
  (message command)
  (refresh-buffer)
  (setenv pytest-venv-key pytest-venv-value)
  (if (eq current-prefix-arg nil)
      (execute-test "ponytests" pytest-binary (append (list command) (split-string pytest-args)))
    ;; optional command
    )
)


(defun run-pytest-one ()
  "Run the test(s) given by `command'."
  (interactive)
  (run-pytest))

(defun run-pytest-on-file ()
  (interactive)
  (run-pytest-file)
  )


;; Create keymaps
(defvar ma-pytest-mode-map
  (let ((map (make-keymap)))
    map))

(define-key ma-pytest-mode-map "\C-cm"  'run-pytest-on-file)
(define-key ma-pytest-mode-map "\C-co"  'run-pytest-one)
(define-key ma-pytest-mode-map "\C-c\C-pm"  'copy-module-to-clipboard)

;; Create minor mode with keymaps
(define-minor-mode ma-pytest-minor-mode
  "Meilleurs Agents pytest mode"
  :initial nil
  :lighter "ma-pytest"
  :keymap ma-pytest-mode-map)


;;;;;;;;;;;;;; Replay tests

(defvar *last-test-args* nil)
(defvar *last-python-interpreter* nil)

(defun replay-last-test ()
  (interactive)
  (execute-test "ponytests" *last-python-interpreter*  *last-test-args*))

;;;;;;;;;;;;;;;;;;;;; Build import module.

(defun build_function_import(module class function)
  (concat "from " module "." class " import " function)
    )

(defun build_module_import(module class)
  (concat "from " module " import " class)
  )

(defun copy-module-to-clipboard()
  (interactive)
  (let* ((defuns (subseq (split-string (which-function) "\\.") 0 2))
 	(class (first defuns))
   	(function (second defuns))
   	(module (pony-get-module))
   	(import-string (if function
			   (build_function_import module class function)
			 (build_module_import module class))))
    (message "%s" import-string)
    (kill-new import-string)
    )
)
