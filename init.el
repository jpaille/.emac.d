;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  EMACS CONFIGURATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(setq load-path (cons "/edx/app/edxapp/.emacs.d" load-path))

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
                    (buffer-file-name))))
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
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".pyc" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc")))
 '(cua-mode t nil (cua-base))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|pyc\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-omit-files "^\\.?#\\$")
 '(display-time-mode t)
 '(flycheck-idle-change-delay 0)
 '(flycheck-javascript-jshint-executable "/edx/app/edxapp/node_modules/jshint/bin/jshint")
 '(ido-default-file-method (quote selected-window))
 '(line-number-mode 1)
 '(magit-save-some-buffers nil)
 '(mark-even-if-inactive t)
 '(mmm-submode-decoration-level 0)
 '(show-paren-mode t))
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
 '(comint-highlight-prompt ((t (:inherit dired-flagged))))
 '(custom-link ((t (:inherit change-log-date))))
 '(custom-visibility ((t (:inherit web-mode-constant-face :height 0.8))))
 '(ediff-current-diff-A ((t (:foreground "blue3"))))
 '(ediff-current-diff-B ((t (:foreground "magenta3" :weight bold))))
 '(ediff-even-diff-A ((t (:foreground "red3" :weight bold))))
 '(ediff-even-diff-B ((t (:foreground "blue3" :weight bold))))
 '(emms-playlist-track-face ((t (:foreground "green"))))
 '(erc-prompt-face ((t (:foreground "Black" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "brown"))))
 '(highlight ((t (:weight bold))))
 '(ido-first-match ((t (:foreground "green" :weight bold))))
 '(magit-branch ((t (:foreground "LightSkyBlue4"))))
 '(magit-item-highlight ((t (:inherit nil))))
 '(magit-log-head-label-local ((t (:foreground "LightSkyBlue4" :box 1))))
 '(magit-log-head-label-remote ((t (:foreground "OliveDrab4" :box 1))))
 '(magit-tag ((t (:foreground "goldenrod4"))))
 '(match ((t (:foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "red"))))
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
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PROJECTILE                               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f9>") 'projectile-find-file-in-known-projects)
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
(setq jedi:server-args
      '("--virtual-env" "/edx/app/edxapp/venvs/edxapp"
	"--sys-path" "/edx/app/edxapp/edx-platform"
	"--sys-path" "/edx/app/edxapp/edx-platform/lms/djangoapps"
	"--sys-path" "/edx/app/edxapp/edx-platform/common/djangoapps"
	"--sys-path" "/edx/app/edxapp/edx-platform/cms/djangoapps"
	"--sys-path" "/edx/app/edxapp/fun-config"
	))

(global-set-key (kbd "C-x p") 'jedi:goto-definition)
(global-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)
(global-set-key (kbd "C-x =") 'jedi:show-doc)

;; import debug python
(fset 'include
   "import ipdb; ipdb.set_trace()")
(global-set-key (kbd "C-x i") 'include)


(fset 'doctrings
   "\"\"\" Submit 'generate_answers_distribution_report' task to celery.
    Args:
         course_id (str): The course id as string.
         problem_id (str): The problem id as string.

    Returns:
         Redirect to Report Manager dashboard.
     \"\"\"")

(global-set-key (kbd "C-x _") 'doctrings)
;; settings 
(fset 'test_fun
   "fun lms.test test")
(global-set-key (kbd "C-x 7") 'test_fun)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                JS                                       ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun flycheck-js-setup ()
  (flycheck-mode))
(add-hook 'js-mode-hook #'flycheck-js-setup)


(global-set-key (kbd "C-x j") 'flycheck-mode)


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
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist '(("mako" . "\\.html\\'") ))

(defun web-mode-keys ()
  (local-set-key (kbd "C-M-n") 'web-mode-element-end)
  (local-set-key (kbd "C-M-p") 'web-mode-element-beginning)
  (local-set-key (kbd "C-c o") 'web-mode-element-insert)
  (setq indent-tabs-mode nil)
)
(add-hook 'web-mode-hook 'web-mode-keys)

(add-hook 'html-mode-hook 'html-mode-keys)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)


(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "Pink3")

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

(global-set-key  (kbd "M-o") 'comint-previous-input)
(global-set-key  (kbd "M-,") 'comint-next-input)



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
;; start fun development server
(defun start-fun-server()
  (interactive)
  (get-buffer-create "*lms*")
  (shell "*lms*")
  (shell "*cms*")
  (process-send-string "*lms*" "funlms\n")
  (process-send-string "*cms*" "funcms\n"))
(start-fun-server)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                TODO                                     ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun todo()
  (interactive)
  (switch-to-buffer-other-window "*tmp*")
  (find-file "~/todo")
  (kill-buffer "*tmp*"))
(global-set-key (kbd "C-x [") 'todo_direct)

(defun todo_direct()
  (interactive)
  (find-file "~/todo"))
(global-set-key (kbd "C-c [") 'todo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PHP                                      ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAGIT                                    ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f5] 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MP3                                      ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp")
(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-standard)
(emms-default-players)
(define-emms-simple-player mplayer '(file url)
      (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                    ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                    ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
      "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
(setq emms-player-mpg321-parameters '("-o" "alsa"))

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
