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
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (concat (buffer-file-name)) )))
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(asm-comment-char 59)
 '(beacon-color "#d33682")
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".pyc" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc")))
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|pyc\\|__pycached__\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-omit-files "__pycache__\\|.ssh\\|__init__.py")
 '(display-time-mode t)
 '(fci-rule-color "#eee8d5")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
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
 '(frame-background-mode (quote light))
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
    (auto-complete highlight-quoted diredfl color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized tide terraform-mode rjsx-mode projectile plsql pinentry nodejs-repl magit keychain-environment jedi hackernews format-sql dockerfile-mode docker-tramp company bash-completion autopair pyasnippet web-mode  pkg-info multiple-cursors markdown-mode  flycheck epl proceed)))
 '(safe-local-variable-values
   (quote
    ((pytest-venv-value . "test")
     (pytest-venv-key . "ENVIRONMENT")
     (pytest-args . "-s -Wignore -vv")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/www/.venv/bin/pytest")
     )))
 '(show-paren-mode t)
 '(tab-width 11)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))

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

(add-hook 'dired-mode-hook 'diredfl-mode)


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


(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(setq jedi:server-args
      '("--virtual-env" "/home/jpaille/meilleursagents/apps/MediaAPI/.venv"
	)
      )

(defun jedi-custom-keys ()
  (local-set-key (kbd "C-x p") 'jedi:goto-definition)
  (local-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)
)

(add-hook 'jedi-mode-hook 'jedi-custom-keys)


;; import debug python
(fset 'include
   "import ipdb; ipdb.set_trace()")
(global-set-key (kbd "C-x i") 'include)


;; sort package

(global-set-key (kbd "C-c i") 'py-isort-buffer)

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
;;(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key [f5] 'magit-status)

;; for grep mode deactivate C-xg
(with-eval-after-load 'magit
  (define-key magit-file-mode-map "\C-xg" nil))
(global-set-key "\C-xg" 'rgrep)

;; Git graph
(fset 'gitgg
   "git gg | head -35\C-m")
(global-set-key "\C-xy" 'gitgg)


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

