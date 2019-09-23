
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  EMACS CONFIGURATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user (getenv "USER"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PACKAGE.EL                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Activate installed packages with elpa
(package-initialize)

;; Add other source of packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GENERAL                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utf-8 used ?
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;copy file name path

(setq nbr (string-match "[0-9]+" (what-line)))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (concat (buffer-file-name) ":" (substring (what-line)  (string-match "[0-9]+" (what-line)))) )))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key "\C-x9" 'my-put-file-name-on-clipboard)

;; Rename a buffer
(global-set-key "\C-cw" 'rename-buffer)

;; Show time in the mode line.
(setq display-time-24hr-format 1)

;; disable parenthesis display delay
(setq show-paren-delay 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 59)
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".pyc" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc")))
 '(cua-mode t nil (cua-base))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|pyc\\|__pycached__\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-omit-files "__pycache__\\|.ssh\\|__init__.py")
 '(display-time-mode t)
 '(flycheck-flake8-error-level-alist
   (quote
    (("^E9.*$" . error)
     ("^F82.*$" . error)
     ("^F83.*$" . error)
     ("^D.*$" . info)
     ("^N.*$" . info)
     ("^W503$"))))
 '(flycheck-flake8rc ".flake8")
 '(flycheck-idle-change-delay 0)
 '(flycheck-javascript-jshint-executable "")
 '(flycheck-python-flake8-executable
   "/home/jpaille/meilleursagents/apps/MediaAPI/.venv/bin/flake8")
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendors" "static" "node_modules" ".venv")))
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-ignore-buffers
   (quote
    ("^((?!fun-apps).)*$" "*Completions*" "*Shell Command Output*" "*Messages*")))
 '(ido-mode (quote both) nil (ido))
 '(ido-show-dot-for-dired nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(line-number-mode 1)
 '(magit-diff-options nil)
 '(magit-save-some-buffers nil)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(mark-even-if-inactive t)
 '(mmm-submode-decoration-level 0)
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode web-mode virtualenv tide terraform-mode sqlup-mode sql-indent rjsx-mode relative-line-numbers python-pep8 pytest pymacs py-isort projectile pony-mode plsql pinentry php-mode nodejs-repl neotree nav multiple-cursors markdown-mode magit linum-relative keychain-environment jedi hackernews groovy-mode grizzl git-link format-sql flymake-python-pyflakes emms dockerfile-mode docker-tramp company coffee-mode bongo bash-completion autopair ace-jump-mode)))
 '(pony-server-host "0")
 '(pony-settings-module "hive.tests_settings")
 '(safe-local-variable-values
   (quote
    ((pony-settings
      (make-pony-project :python "/home/julien/python_env/hive3/bin/python" :pythonpath "" :settings "tests_settings" :projectname "hived"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/scv/bin/python" :pythonpath "" :settings "" :projectname "scv"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive3/bin/python" :pythonpath "" :settings "tests_settings" :projectname "hive"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive3/bin/python" :pythonpath "" :settings "tests_settings"))
     (pytest-cmd-flags . "")
     (pytest-global-name . "/home/julien/python_env/scv/bin/pytest")
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/scv/bin/python" :pythonpath "" :settings "tests_settings"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/scv/bin/python" :pythonpath "" :settings ""))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive2/bin/python" :pythonpath "" :settings "tests_settings"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive/bin/python" :pythonpath "" :settings "tests_settings"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive/bin/python" :pythonpath "" :settings "hive.tests_settings" :appsdir "/home/julien/hive"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_env/hive/bin/python" :pythonpath "" :settings "tests_settings" :appsdir "/home/julien/hive/hive"))
     (pony-settings
      (make-pony-project :python "/home/julien/python_envs/hive/bin/python" :pythonpath "" :settings "tests_settings" :appsdir "/home/julien/hive/hive"))
     (pony-settings make-pony-project :python "~/python_envs/hive/bin/python" :pythonpath "~/hive/hive" :settings "hive.test_settings"))))
 '(show-paren-mode t)
 '(sql-postgres-login-params
   (quote
    ((user :default "julien")
     password server
     (database :default "julien"))))
 '(tab-width 11)
 '(unittest-last-executed-module "hive.salesstructures.tests.test_sales_structure"))
;; ? This option makes a difference in Transient Mark mode.
 ;; '(virtualenv-root "~/venvs/edxapp/")) ? not necessary with setq jedi:server-args

;;'(scroll-bar-mode (quote right)) ? scroll-bar-mode is not installed

;; smooth scrolling
(setq scroll-step 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:inherit magit-branch))))
 '(comint-highlight-prompt ((t (:inherit dired-flangged))))
 '(custom-link ((t (:inherit change-log-date))))
 '(custom-visibility ((t (:inherit web-mode-constant-face :height 0.8))))
 '(diff-added ((t (:inherit diff-changed))))
 '(diff-changed ((t (:foreground "green"))))
 '(diff-header ((t nil)))
 '(diff-removed ((t (:foreground "red"))))
 '(ediff-current-diff-A ((t (:background "white" :foreground "blue3"))))
 '(ediff-current-diff-B ((t (:underline "red"))))
 '(ediff-even-diff-A ((t (:foreground "red3" :weight bold))))
 '(ediff-even-diff-B ((t (:foreground "red3" :weight bold :underline "red"))))
 '(emms-playlist-track-face ((t (:foreground "green"))))
 '(epa-string ((t (:foreground "red"))))
 '(erc-prompt-face ((t (:foreground "Black" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "blue" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "brown"))))
 '(font-lock-function-name-face ((t (:foreground "magenta" :weight bold))))
 '(hi-blue ((t (:foreground "red" :underline t))))
 '(highlight ((t (:weight bold))))
 '(ido-first-match ((t (:foreground "green" :weight bold))))
 '(magit-branch ((t (:foreground "LightSkyBlue4"))))
 '(magit-item-highlight ((t (:inherit nil))))
 '(magit-log-head-label-head ((t (:foreground "green" :box 1))))
 '(magit-log-head-label-local ((t (:foreground "LightSkyBlue4" :box 1))))
 '(magit-log-head-label-remote ((t (:foreground "OliveDrab4" :box 1))))
 '(magit-log-head-label-tags ((t (:foreground "green" :box 1))))
 '(magit-tag ((t (:foreground "goldenrod4"))))
 '(match ((t (:foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "red"))))
 '(rst-level-1 ((t nil)))
 '(rst-level-2 ((t nil)))
 '(rst-level-3 ((t nil)))
 '(rst-level-4 ((t nil)))
 '(rst-level-5 ((t nil)))
 '(rst-level-6 ((t nil)))
 '(web-mode-html-tag-custom-face ((t (:inherit web-mode-warning-face))))
 '(web-mode-html-tag-face ((t (:foreground "red")))))


;; Hightlight current line.
(global-hl-line-mode t)

;; Highlight region.
(transient-mark-mode 1) ;; ?When enabled, the region is highlighted whenever the mark is active.

;; disable backup
(setq backup-inhibited t)

;;disable auto save
(setq auto-save-default nil)

;; disable lock file system
(setq create-lockfiles nil)

;; disable menu_bar
(menu-bar-mode -1)

;; edit in search bar

(global-set-key (kbd "M-s") 'isearch-edit-string)

;;;;;;;
;;;;;;; whitespace general handling
;;;;;;;

;; Show/delete trailing spaces disable for shell-mode
(setq-default show-trailing-whitespace nil)
(global-set-key [f6] 'delete-trailing-whitespace)

;; show white spaces
(global-set-key (kbd "C-x w") 'whitespace-mode)

;; undo
(global-set-key (kbd "C-w") 'undo)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;;;;;;;;;;;



;; delete backwards
;;(global-set-key "\C-h" 'backward-delete-char)

;; short answers
(fset 'yes-or-no-p 'y-or-n-p)

;; navigation shortcuts.
(ffap-bindings)
(global-set-key  (kbd "C-x C-d") 'ffap-other-window)


(defun move_fast_down()
(interactive)
(forward-line 4))

(defun move_fast_up()
(interactive)
(forward-line -4))

(global-set-key  (kbd "M-n") 'move_fast_down)
(global-set-key  (kbd "M-p") 'move_fast_up)
(global-set-key  (kbd "C-q") 'move-beginning-of-line)

(define-key ctl-x-map "\'" 'ctl-x-4-prefix)
(global-set-key (kbd "C-x \' b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-x \' f") 'find-file-other-window)

;;global-set-key (kbd "C-x \"") 'split-window-horizontally)
;;(global-set-key (kbd "C-x é") 'split-window-vertically)

;;(global-set-key (kbd "C-x à") 'delete-window)
(global-set-key (kbd "ESC <up>") 'enlarge-window)
(global-set-key (kbd "ESC <down>") 'shrink-window)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x -") 'beginning-of-buffer)
(global-set-key (kbd "C-x =") 'end-of-buffer)
;;(global-set-key (kbd "M-:") 'dabbrev-expand)
(global-set-key (kbd "C-x l") 'find-file-at-point) ;; shell need to be sync
(global-set-key (kbd "C-x C-f") 'ido-find-file) ;; shell need to be sync
(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x v") 'uncomment-region)

(defun go_to_conf()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-x <home>") 'go_to_conf)


;; revert buffer
(global-set-key (kbd "<C-f5>") 'revert-buffer)


;; time
(defun now ()
  "Insert string for the current time formatted like '2:34'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%-I:%M")))

(defun today ()
    "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%A, %B %e, %Y")))
(global-set-key (kbd "C-c t") 'now)



;; Go to file at line


(defun xah-open-file-at-cursor ()
    "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2018-02-21"
    (interactive)
    (let* (($inputStr (if (use-region-p)
			  (buffer-substring-no-properties (region-beginning) (region-end))
			(let ($p0 $p1 $p2
				  ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
				  ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
			  (setq $p0 (point))
			  (skip-chars-backward $pathStops)
			  (setq $p1 (point))
			  (goto-char $p0)
			  (skip-chars-forward $pathStops)
			  (setq $p2 (point))
			  (goto-char $p0)
			  (buffer-substring-no-properties $p1 $p2))))
	   ($path
	    (replace-regexp-in-string
	     "^file:///" "/"
	     (replace-regexp-in-string
	      ":\\'" "" $inputStr))))
      (if (string-match-p "\\`https?://" $path)
	  (if (fboundp 'xahsite-url-to-filepath)
	      (let (($x (xahsite-url-to-filepath $path)))
		(if (string-match "^http" $x )
		    (browse-url $x)
		  (find-file $x)))
	    (progn (browse-url $path)))
	(if ; not starting “http://”
	    (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
	    (let (
		  ($fpath (match-string 1 $path))
		  ($line-num (string-to-number (match-string 2 $path))))
	      (if (file-exists-p $fpath)
		  (progn
		    (find-file $fpath)
		    (goto-char 1)
		    (forward-line (1- $line-num)))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		  (find-file $fpath))))
	  (if (file-exists-p $path)
	      (progn ; open f.ts instead of f.js
		(let (($ext (file-name-extension $path))
		      ($fnamecore (file-name-sans-extension $path)))
		  (if (and (string-equal $ext "js")
			   (file-exists-p (concat $fnamecore ".ts")))
		      (find-file (concat $fnamecore ".ts"))
		    (find-file $path))))
	    (if (file-exists-p (concat $path ".el"))
		(find-file (concat $path ".el"))
	      (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
		              (find-file $path ))))))))


(global-set-key (kbd "C-c 9") 'xah-open-file-at-cursor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               multiple-cursor                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)
(global-set-key (kbd "M-3") 'mc/mark-next-like-this)
(global-set-key (kbd "M-4") 'mc/skip-to-next-like-this)
(global-set-key (kbd "M-5") 'mc/unmark-next-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               IDO                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(ido-everywhere t)

;;(setq ido-use-filename-at-point 'guess)

;; create buffer
(setq ido-create-new-buffer 'always)

;; make ido display choices vertically
;;(setq ido-separator "\n")

;; display any item that contains the chars you typed
;;(setq ido-enable-flex-matching t)

;;(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;; (define-key ido-completion-map (kbd "M-n") 'ido-next-match)
;; (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)
;; (define-key ido-completion-map (kbd "C-p") 'ido-prev-work-directory)
;; (define-key ido-completion-map (kbd "C-n") 'ido-next-work-directory))

;;qd-hook 'ido-setup-hook 'ido-define-keys)

;; remove useless buffer from buffer list
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
			   "*Messages*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               DIRED                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'dired-load-hook
           (function (lambda ()
                       (load "dired-x")
                       )))
(setq dired-omit-mode t)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(global-set-key (kbd "C-x <end>") 'dired-dotfiles-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PROJECTILE                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key (kbd "<f9>") 'projectile-find-file-in-known-projects)
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               BUFFER                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer listy direct
(global-set-key (kbd "C-x b") 'switch-to-buffer)

;; Only display files
;;(fset 'bufferlistfiles
;;      "\C-xbT")
(global-set-key (kbd "C-x n") 'buffer-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PYTHON                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set all edx sys-path for jedi mode
;; (if (string= user "edxapp")
;;     (setq jedi:server-args
;; 	  '("--virtual-env" "/edx/app/edxapp/venvs/edxapp"
;; 	    "--sys-path" "/edx/app/edxapp/edx-platform"
;; 	    "--sys-path" "/edx/app/edxapp/edx-platform/lms/djangoapps"
;; 	    "--sys-path" "/edx/app/edxapp/edx-platform/common/djangoapps"
;; 	    "--sys-path" "/edx/app/edxapp/edx-platform/cms/djangoapps"
;; 	    "--sys-path" "/edx/app/edxapp/fun-config"
;; 	    ))
;;   )
;; set all Hive auchan sys-path for jedi mode TODO create a Hive user ?

(setq jedi:server-args
      '("--virtual-env" "/home/jpaille/meilleursagents/apps/MediaAPI/.venv"
	)
      )


;;(setq jedi:server-command '("/home/jpaille/emacs-jedi/jediepcserver.py"))


(defun jedi-custom-keys ()
  (local-set-key (kbd "C-x p") 'jedi:goto-definition)
  (local-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)
)

(add-hook 'jedi-mode-hook 'jedi-custom-keys)

;;(global-set-key (kbd "C-x p") 'jedi:goto-definition)
;;(global-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)

;; import debug python
(fset 'include
   "import ipdb; ipdb.set_trace()")
(global-set-key (kbd "C-x i") 'include)


;; sort package

(global-set-key (kbd "C-c i") 'py-isort-buffer)

;; Easy import package


(fset 'doctrings
   "\"\"\"Submit 'generate_answers_distribution_report' task to celery.

        Args:
            course_id (str): The course id as string.
            problem_id (str): The problem id as string.

	Returns:
	    Redirect to Report Manager dashboard.
        \"\"\"")

(global-set-key (kbd "C-x -") 'doctrings)



;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; auto-complete
(load "auto-complete.el")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)



(defun flycheck-python-setup ()
  (flycheck-mode))

(add-hook 'python-mode-hook #'flycheck-python-setup)

(global-set-key (kbd "C-x j") 'flycheck-mode)


;; auto pairing

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

(require 'python)
(define-key python-mode-map (kbd "C-c C-u") 'python-nav-backward-up-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Pytest                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'pytest)

(add-to-list 'pytest-project-names "/home/julien/scv/scv/")
;; (add-to-list 'pytest-project-root-files ".pyt")
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ca" 'pytest-all)
;; 	    (local-set-key "\C-cm" 'pytest-module)
;; 	    (local-set-key "\C-c." 'pytest-one)
;; 	    (local-set-key "\C-cp." 'pytest-pdb-one)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Django                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'pony-mode)

;;;; PONY mode

;; fast movement in pony test
(define-key comint-mode-map (kbd "M-p") nil)
(define-key comint-mode-map (kbd "M-n") nil)



;; replay last test from everywhere in pony.
(global-set-key (kbd "<f6>") 'replay-last-test)

;; add autocomplete to pony test buffer.
(add-to-list 'ac-modes 'pony-test-minor-mode)
(add-to-list 'ac-modes 'pony-test-mode)
(add-to-list 'ac-modes 'pony-mode)


(message (if nil "ll" "oo"))
(if (eq nil nil)  "null" "oo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               SNIPPET                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SNIPPET YAS
;; activate yas-mode in python mode

(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'python-mode-hook 'yas-minor-mode)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)

(defun python-args-to-google-docstring (text &optional make-fields)
"Return a reST docstring format for the python arguments in yas-text."
(let* ((indent (concat "\n" (make-string (current-column) 32)))
       (args (python-split-args text))
       (nr 0)
       (formatted-args
	(mapconcat
	 (lambda (x)
	   (concat "   " (nth 0 x)
		   (if make-fields (format " ${%d:arg%d}" (incf nr) nr))
		   (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
	 args
	 indent)))
  (unless (string= formatted-args "")
    (concat
     (mapconcat 'identity
		(list "" "Args:" formatted-args)
		indent)
            "\n"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                HTML                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Never user tabs but spaces
(add-hook 'html-mode-hook
          (lambda()
	    (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)))


;; set better shortcuts to move from tag to tag
(defun html-mode-keys ()
  (local-set-key (kbd "C-M-n") 'sgml-skip-tag-forward)
  (local-set-key (kbd "C-M-p") 'sgml-skip-tag-backward)
  (local-set-key (kbd "C-c :") 'sgml-close-tag)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAKO                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.underscore?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'") ))
;;(setq web-mode-engines-alist '(("underscore" . "\\.underscore\\'")))

(defun web-mode-keys ()
 ;; (local-set-key (kbd "C-M-n") 'web-mode-element-end)
;;  (local-set-key (kbd "C-M-p") 'web-mode-element-beginning)
  (local-set-key (kbd "C-c o") 'web-mode-element-insert)
  (setq indent-tabs-mode nil)
)

(add-hook 'web-mode-hook 'web-mode-keys)

(add-hook 'html-mode-hook 'html-mode-keys)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)


(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "Pink3")


;; ;; import debug python
;; (fset 'include-css-border
;;    "border: 1px solid black; /*TO REMOVE*/")
;; (global-set-key (kbd "C-c i") 'include-css-border)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAGIT                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;To prevent this message from being shown each time you start
;;Emacs, you must add the following line to your init file:
(setq magit-last-seen-setup-instructions "1.4.0")

(defun magit-status-edx()
  (interactive)
  (magit-status "/edx/app/edxapp/edx-platform"))
(global-set-key (kbd "C-<f11>") 'magit-status-edx)

(defun magit-status-fun()
  (interactive)
  (magit-status "/edx/app/edxapp/fun-apps"))
(global-set-key (kbd "C-<f12>") 'magit-status-fun)

(defun magit-status-fun-theme()
  (interactive)
  (magit-status "/edx/app/edxapp/themes/fun"))
(global-set-key (kbd "C-<f10>") 'magit-status-fun-theme)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                C                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "M-c") 'compile)
;; (global-set-key (kbd "C-x n") 'next-error)

(setq-default gdb-many-windows t)

;;hearder guards
(define-auto-insert
(cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
'(nil
     ""
  (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
        (nopath (file-name-nondirectory noext))
     (ident (concat (upcase nopath) "_H")))
    (concat "#ifndef	" ident "\n"
      "#define	" ident  "\n\n\n"
       "\n\n#endif // " ident "\n"))
))
(global-set-key [f12] 'auto-insert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                SHELL                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'shell)

;; Git grep
(defun git_grep()
  "shorcut for grep with git"
(interactive)
(insert "git gr \"\"")
(backward-char 1))
(global-set-key "\C-cg" 'git_grep)

;; grep mode
(global-set-key "\C-xg" 'rgrep)

;; Git graph
(fset 'gitgg
   "git gg | head -35\C-m")
(global-set-key "\C-xy" 'gitgg)

;; vpn
(fset 'code
   "!9@T2cJv")
(global-set-key (kbd "C-c 3") 'code)

;; move fast like everywhere
(defun mp-add-shell-keys ()
  (setq-default show-trailing-whitespace nil)
  (local-set-key (kbd "C-x C-e") 'comint-show-maximum-output)
  (define-key (kbd "M-,") 'comint-previous-input))

;;(add-hook 'shell-mode-hook 'mp-add-shell-keys)

(define-key shell-mode-map (kbd "M-p") nil)
(define-key shell-mode-map (kbd "M-n") nil)
(define-key shell-mode-map (kbd "C-c RET") nil)
(global-set-key  (kbd "M-o") 'comint-previous-input)
(global-set-key  (kbd "M-m") 'comint-next-input)


;; shell-resync-dirs
(global-set-key "\M-\r" 'shell-resync-dirs)

;; shortcut shell
(defun spawn-shell (name)
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer)))
(global-set-key (kbd "M-.") 'spawn-shell)

(defun start-shells()
  (get-buffer-create "*shell*")
  (get-buffer-create "*oo*")
  (shell "*shell*")
  (shell "*oo*"))
(start-shells)

(defun start-fun-server()
  (interactive)
  (get-buffer-create "*lms*")
  (shell "*lms*")
  (shell "*cms*")
  (process-send-string "*lms*" "funlms\n")
  (process-send-string "*cms*" "funcms\n"))

;; start fun development server
(if (string= user "edxapp")
    (start-fun-server))

;; go to prompt
(global-set-key (kbd "C-c e") 'comint-goto-process-mark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                TODO                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun todo()
  (interactive)
  (switch-to-buffer-other-window "*tmp*")
  (find-file "~/todo/todo")
  (kill-buffer "*tmp*"))
(global-set-key (kbd "C-x [") 'todo_direct)

(defun todo_direct()
  (interactive)
  (find-file "~/todo/todo"))

(global-set-key (kbd "C-c [") 'todo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PHP                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAGIT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f5] 'magit-status)
;;(global-set-key  (kbd "C-c 9") 'magit-blame-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MP3                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; ;;(add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp")

;; (require 'emms-setup)
;; (require 'emms-player-mplayer)
;; (emms-standard)
;; (emms-default-players)
;; (define-emms-simple-player mplayer '(file url)
;;       (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
;;                     ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
;;                     ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
;;       "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
;; (setq emms-player-mpg321-parameters '("-o" "alsa"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                TRAMP                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq tramp-default-method "ssh")
;; (add-to-list 'tramp-default-proxies-alist
;;          '("cargolms02" nil "/ssh:master@infraansible.cines.fr:"))

;; (defun infra-shell ()
;;   (interactive)
;;   (let ((default-directory "/ssh:master@infraansible.cines.fr:/"))
;;     (shell)))
(add-hook 'comint-exec-hook
	        (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun anr-shell (buffer)
  "Opens a new shell buffer where the given buffer is located."
  (interactive "sBuffer: ")
  (pop-to-buffer (concat "*" buffer "*"))
  (unless (eq major-mode 'shell-mode)
    (dired buffer)
    (shell buffer)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer))
			(concat "export PS1=\"\033[33m" buffer "\033[0m:\033[35m\\W\033[0m>\"")))
  (setq inhibit-read-only 42)
)

(defun connect_cargo ()
  (interactive)
  (find-file "/ssh:cargo:/"))

(defun connect_infraansible ()
  (interactive)
  (find-file "/ssh:infraansible:/"))

;; (global-set-key (kbd "C-c 6") 'connect_infraansible)
;; (global-set-key (kbd "C-c 4") 'connect_cargo)
;; (global-set-key (kbd "C-c 5") 'anr-shell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MYSQL                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; capitalize keywords
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;; active autocomplete for sql-mode
(add-to-list 'ac-modes 'sql-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MARKDOWN                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-n") nil)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                POMODORO                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *pomodoro-directory* "/home/jpaille/pomodoro_analyzer/pomodoro_files/")

(defun get-pomodoro-filename()
  (concat (format-time-string "%Y-%m-%d") ".txt"))


(get-pomodoro-filename)


(defun get-pomodoro-filepath ()
    "The pomodoro filename will be the current date."
    (concat *pomodoro-directory* (get-pomodoro-filename)))


(defun get-time ()
  (format-time-string "%-H:%M"))

(defun start-pomodoro ()
  (interactive)
  (go-to-pomodoro-file)
  (end-of-buffer)
  (insert (concat "\n" "p: " (get-time) "\n"))
  (message "**START POMODORO")
  )


(defun end-pomodoro ()
  (interactive)
  (go-to-pomodoro-file)
  (end-of-buffer)
  (insert (concat "\n" "b: " (get-time) "\n"))
  (message "STOP POMODORO")
  )

(defun task-time ()
  (interactive)
  (end-of-line)
  (insert (concat " t=" (get-time)))
  )


(defun interrupt-pomodoro ()
  (interactive)
  (set-buffer  (get-pomodoro-filename))
  (end-of-buffer)
  (insert (concat "\n" "i: " (get-time) "\n"))
  (message "INTERRUPT POMODORO")
  )

(defun small-interrupt-pomodoro ()
  (interactive)
  (set-buffer  (get-pomodoro-filename))
  (end-of-buffer)
  (insert (concat "\n" "X: "))
  (message "SM INTERRUPT POMODORO")
  )
  

(defun go-to-pomodoro-file ()
  (interactive)
  (if (eq (get-buffer (get-pomodoro-filename)) nil)
      (find-file (get-pomodoro-filepath))
    )
  (switch-to-buffer (get-pomodoro-filename))
  (save-buffer)
)

(global-set-key (kbd "C-c =") 'start-pomodoro)
(global-set-key (kbd "C-c -") 'end-pomodoro)
(global-set-key (kbd "C-c DEL") 'interrupt-pomodoro)
(global-set-key (kbd "<f12>") 'interrupt-pomodoro)
(global-set-key (kbd "<f1>") 'small-interrupt-pomodoro)
(global-set-key (kbd "C-c \\") 'go-to-pomodoro-file)

(defun pomodoro-stats ()
  (interactive)
  (switch-to-buffer (get-pomodoro-filename))
  (save-buffer)
  (term "/bin/bash")
  (term-line-mode)
  (term-send-raw-string "po")
  (term-send-input))

(global-set-key (kbd "<f9>") 'task-time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                JAVASCRIPT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;; flycheck

(defun flycheck-js-setup ()
  (flycheck-mode))

(add-hook 'rjsx-mode-hook 'flycheck-js-setup)

(global-set-key (kbd "C-x j") 'flycheck-mode)

(require 'flycheck)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; use local eslint from node_modules before global
(defun my-use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node_modules"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js"
					root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook 'my-use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rjsx  / js2

(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

(defun rjsx-jump-definition ()
  (interactive)
  (js2-jump-to-definition 1)
;;  (call-interactively 'cua-set-mark)
;;  (call-interactively 'cua-cancel)

  )

(defun rjsx-custom-keys ()
  (local-set-key (kbd "C-x p") 'rjsx-jump-definition)
  (local-set-key (kbd "C-x ]") 'jump-back)
)

(add-hook 'rjsx-mode-hook 'rjsx-custom-keys)

;; auto fix eslint 
(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat flycheck-javascript-eslint-executable " --fix " (buffer-file-name))))


(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'rjsx-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'eslint-fix-file-and-revert nil 'make-it-local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                NODE.JS                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; repl interpreter
(add-to-list
 'comint-preoutput-filter-functions
 (lambda (output)
              (replace-regexp-in-string "\\[[0-9]+[GJ]" "" output)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                JAVASCRIPT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;; flycheck

(defun flycheck-js-setup ()
  (flycheck-mode))

(add-hook 'rjsx-mode-hook 'flycheck-js-setup)

(global-set-key (kbd "C-x j") 'flycheck-mode)

(require 'flycheck)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; use local eslint from node_modules before global
(defun my-use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node_modules"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js"
					root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook 'my-use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rjsx  / js2

(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

(defun rjsx-jump-definition ()
  (interactive)
  (js2-jump-to-definition 1)
;;  (call-interactively 'cua-set-mark)
;;  (call-interactively 'cua-cancel)

  )

(defun rjsx-custom-keys ()
  (local-set-key (kbd "C-x p") 'rjsx-jump-definition)
  (local-set-key (kbd "C-x ]") 'jump-back)
)

(add-hook 'rjsx-mode-hook 'rjsx-custom-keys)

;; auto fix eslint 
(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat flycheck-javascript-eslint-executable " --fix " (buffer-file-name))))


(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'rjsx-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'eslint-fix-file-and-revert nil 'make-it-local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                NODE.JS                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; repl interpreter
(add-to-list
 'comint-preoutput-filter-functions
 (lambda (output)
              (replace-regexp-in-string "\\[[0-9]+[GJ]" "" output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAN                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'man)
(define-key Man-mode-map (kbd "M-p") nil)
(define-key Man-mode-map (kbd "M-n") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                ASM                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq make-backup-files nil)
(autoload 'asm-mode-custom "/home/julien/.emacs.d/elpa/asm-mode-custom/asm-mode-custom.el")
(setq auto-mode-alist
       (append '(("\\.asm\\'" . asm-mode-custom) ("\\.inc\\'" . auto-mode-alist))
 	      auto-mode-alist))
;; (setq asm86-inst-func-offset 3)
(defun modify-tab-asm ()
  (setq tab-width 11)
    )
(add-hook 'asm-mode-custome-hook 'modify-tab-asm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAKEFILE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun disable-c-c ()
(local-set-key "\C-c" ctl-x-map)
  )

(add-hook 'makefile-gmake-mode-hook 'disable-c-c)

