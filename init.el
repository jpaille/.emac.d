;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  EMACS CONFIGURATION   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PACKAGE.EL                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add other source of packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
;; Activate installed packages with elpa
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GENERAL                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user venv
(setq user (getenv "USER"))

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Kill buffer message
(kill-buffer "*Messages*")

;; Show time in the mode line.
(setq display-time-24hr-format 1)

;; Disable parenthesis display delay
(setq show-paren-delay 0)

;; Smooth scrolling
(setq scroll-step 1)

;; Hightlight current line.
;; (global-hl-line-mode t)

;; Disable menu_bar
(menu-bar-mode -1)

;; Short answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight selected region.
(transient-mark-mode 1)

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Disable lock file system
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               NAVIGATION                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ffap-bindings) ;; ?

;; Undo
(global-set-key (kbd "C-w") 'undo)

;; Move begin of line (more accessible better than C-a)
(global-set-key  (kbd "C-q") 'move-beginning-of-line)

;; Move fast up and down
(defun move_fast_down()
(interactive)
(forward-line 4))
(defun move_fast_up()
(interactive)
(forward-line -4))
(global-set-key  (kbd "M-n") 'move_fast_down)
(global-set-key  (kbd "M-p") 'move_fast_up)

;; Move windows
(global-set-key (kbd "ESC <up>") 'enlarge-window)
(global-set-key (kbd "ESC <down>") 'shrink-window)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)

;; Find file at point
(global-set-key (kbd "C-x l") 'find-file-at-point) ;; shell need to be sync

;; Comments quick
(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x v") 'uncomment-region)


(defun go_to_conf()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Copy file name path on clipboard 
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Custom variables                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(asm-comment-char 59)
 '(beacon-color "#cc6666")
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (doom-acario-dark)))
 '(custom-safe-themes
   (quote
    ("c2b24b9c9ac598bdf14e61ea1b7d1bfe1b67e173c705ab26b22f3c0763b11db4" "bc8935a8030cf10e634201399fa3dea50a4c1ef03eb4977c7f482b747548350c" "99ca2f7f413db6e5e97d4e4dee1738f9262cecd00218e04aad4affd48ab33b6c" "a79f89e2d547ba796f37582bd21f091cc1af81c3897140591224dc121a716791" "d8492c09fb5b3e40614b0df2c90b641d3a4a4be46b3f80457f75c580a6c18cd0" "b07553317b586539057a06c1b8179218d8baada061c95d17ef63710b6586263a" "30e82c88c9e44e3d6c27549e5b6d46a92e0159e03ee330730a8e0642aa241041" "6d64ea8cdcf31ca98219ea096f9a31d88160f313782b031ee06f745e141a94ca" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "28dde8e3bed35eace3b7c9c2e688040f3973c0afd0de40890ac705e5f970837f" "b1bfa5c40ae6fc4b4b99e3e8e4ebe5295343cc5cdac1011888f4e5dcb9f1a44c" "0d5615c64e5f29ac771a2b4f560ca807292085ef3ecdabe593f4f5d8d0cbfdb0" "59d1a1db38dc5527d78e64f6e244bdb7d528b974d23e914268556e3929f9f035" "2c300250a9ad134dd80ec6e0ef56eb3455ec1a42cf6b5c33b1ed95d69dc8cbdc" "bd607e3b1b64102e2b1918665631db4d4b866e3b765609c9556f52ecfd4ecad1" "50e8bc3372f7e171def01ba91f908799ed457a9dc0e68ff318c2b1a1e04f280f" "b1d4f9f1ce4b07750e40610f450b0a01bd5297fcc59541ca968831c62d6f69b2" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "0713580a6845e8075113a70275b3421333cfe7079e48228c52300606fa5ce73b" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|pyc\\|__pycached__\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-omit-files "__pycache__\\|__init__.py")
 '(display-time-mode t)
 '(fci-rule-color "#373b41")
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
 '(flycheck-python-flake8-executable "~/MeilleursAgents/apps/MediaAPI/.venv/bin/flake8")
 '(frame-background-mode (quote dark))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendors" "static" "node_modules" ".venv" "js")))
 '(grep-save-buffers nil)
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-ignore-buffers
   (quote
    ("^((?!fun-apps).)*$" "*Completions*" "*Shell Command Output*" "*Messages*")))
 '(ido-mode (quote both) nil (ido))
 '(ido-show-dot-for-dired nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#0F1019" "#D85F00"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0F1019" "#79D836"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0F1019" "#767676"))
 '(js-indent-level 4)
 '(line-number-mode 1)
 '(magit-save-repository-buffers nil)
 '(mark-even-if-inactive t)
 '(objed-cursor-color "#D83441")
 '(package-selected-packages
   (quote
    (lua-mode indent-tools indent-guide origami highlight-indent-guides k8s-mode docker crystal-mode company-native-complete native-complete vterm magit terraform-mode sudo-edit php-mode move-text auto-complete-exuberant-ctags go-mode python-black doom-themes js2-mode yaml-mode git-link groovy-mode auto-complete highlight-quoted diredfl jedi dockerfile-mode docker-tramp bash-completion autopair yasnippet web-mode pkg-info multiple-cursors markdown-mode flycheck epl proceed)))
 '(python-black-command "~/MeilleursAgents/apps/MediaAPI/.venv/bin/black")
 '(python-black-macchiato-command "~/opt/pyenv/shims/black-macchiato")
 '(safe-local-variable-values
   (quote
    ((app_name . "mailapi")
     (pytest-docker-args . "-p mailapi-dev -f /home/jpaille/MailAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/MailAPI/.venv")
     (isort-binary . "/home/jpaille/MailAPI/.venv/bin/isort")
     (pytest-docker-args . "-p estimaapi-dev -f /home/jpaille/MailAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/MyProAPI/.venv")
     (isort-binary . "/home/jpaille/MyProAPI/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/MyProAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/<MyProAPI/.venv")
     (isort-binary . "/home/jpaille/<MyProAPI/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/<MyProAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/ProspectionMapAPI/.venv")
     (isort-binary . "/home/jpaille/ProspectionMapAPI/.venv/bin/isort")
     (pytest-docker-args . "-p prospectionmapapi-dev -f /home/jpaille/ProspectionMapAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/BarometreAPI/.venv")
     (isort-binary . "/home/jpaille/BarometreAPI/.venv/bin/isort")
     (pytest-docker-args . "-p estimaapi-dev -f /home/jpaille/BarometreAPI/docker-compose-dev.yml run --rm app pytest")
     (pytest-docker-args . "-p estimaapi-dev -f /home/jpaille/LeadAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/EstimaAPI/.venv")
     (isort-binary . "/home/jpaille/EstimaAPI/.venv/bin/isort")
     (pytest-docker-args . "-p estimaapi-dev -f /home/jpaille/EstimaAPI/docker-compose-dev.yml run --rm app pytest")
     (isort-binary . "/home/jpaille/meilleursagents/apps/Tools/.venv/bin/isort")
     (jedi:server-args "--virtual-env" "/home/jpaille/UserAPI/.venv")
     (isort-binary . "/home/jpaille/UserAPI/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/UserAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/MarketAPI/.venv")
     (isort-binary . "/home/jpaille/MarketAPI/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/MarketAPI/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/MyAccountAPI/.venv")
     (isort-binary . "/home/jpaille/MyAccountAPI/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/MyAccountAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/FeedbackAPI/.venv")
     (app_name . "feedbackapi")
     (isort-binary . "/home/jpaille/FeedbackAPI/.venv/bin/isort")
     (pytest-docker-args . "-p feedbackapi-dev -f /home/jpaille/FeedbackAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/SoldPropertyAPI/.venv")
     (app_name . "soldpropertyapi")
     (isort-binary . "/home/jpaille/SoldPropertyAPI/.venv/bin/isort")
     (pytest-docker-args . "-p soldpropertyapi-dev -f /home/jpaille/SoldPropertyAPI/docker-compose-dev.yml run --rm app pytest")
     (app_name . "agencyapi")
     (app_name . "wa")
     (jedi:server-args "--virtual-env" "/home/jpaille/Webanalytics/.venv")
     (isort-binary . "/home/jpaille/Webanalytics/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/Webanalytics/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/WebAnalytics/.venv")
     (isort-binary . "/home/jpaille/WebAnalytics/.venv/bin/isort")
     (pytest-docker-args . "-p wa-dev -f /home/jpaille/WebAnalytics/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/AgencyAPI/.venv")
     (isort-binary . "/home/jpaille/AgencyAPI/.venv/bin/isort")
     (pytest-docker-args . "-p agencyapi-dev -f /home/jpaille/AgencyAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/julien/meilleursagents/apps/MarketAPI/.venv")
     (isort-binary . "~/meilleursagents/apps/MarketAPI/.venv/bin/isort")
     (pytest-binary . "~/meilleursagents/apps/MarketAPI/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "~/meilleursagents/apps/Thumbor/.venv")
     (pytest-binary . "~/meilleursagents/apps/Thumbor/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/julien/meilleursagents/apps/www/.venv")
     (isort-binary . "/home/julien/meilleursagents/apps/www/.venv/bin/isort")
     (pytest-binary . "/home/julien/meilleursagents/apps/www/.venv/bin/pytest")
     (pytest-docker-args . "-p topofthelistapi-dev -f /home/julien/Webanalytics/docker-compose-dev.yml run --rm app pytest")
     (pytest-docker-args . "-p topofthelistapi-dev -f /~/Webanalytics/docker-compose-dev.yml run --rm app pytest")
     (pytest-docker-args . "-p topofthelistapi-dev -f ~/Webanalytics/docker-compose-dev.yml run --rm app pytest")
     (pytest-binary . "~/python_env/Webanalytics-p9q6zuAU/bin/pytest")
     (jedi:server-args "--virtual-env" "~/python_env/Webanalytics-p9q6zuAU")
     (jedi:server-args "--virtual-env" "~/MeilleursAgents/apps/Tools/.venv")
     (pytest-binary . "~/MeilleursAgents/apps/Tools/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/TopOfTheListAPI/.venv")
     (isort-binary . "/home/jpaille/TopOfTheListAPI/.venv/bin/isort")
     (pytest-docker-args . "-p topofthelistapi-dev -f /home/jpaille/TopOfTheListAPI/docker-compose-dev.yml run --rm app pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/Thumbor/.venv")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/Thumbor/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/PdfAPI/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/apps/PdfAPI/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/PdfAPI/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/SalesforceAPI/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/apps/SalesforceAPI/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/SalesforceAPI/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/MA-Flask-Common/.tox/onetest")
     (pytest-binary . "/home/jpaille/MA-Flask-Common/.tox/onetest/bin/pytest")
     (pytest-args . "-e onetest")
     (pytest-binary . "/home/jpaille/.virtualenvs/tox/bin/tox")
     (pytest-binary . "/home/jpaille/.virtualenvs/tox/bin/tox -e onetest")
     (pytest-binary . "tox -e onetest")
     (pytest-docker-args . "-p leadapi-dev -f /home/jpaille/LeadAPI/docker-compose-dev.yml run --rm app pytest")
     (pytest-binary . "docker-compose")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/tools/passerelles/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/tools/passerelles/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/tools/passerelles/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/LeadAPI/.venv")
     (isort-binary . "/home/jpaille/LeadAPI/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/LeadAPI/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/MyProAPI/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/apps/MyProAPI/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/MyProAPI/.venv/bin/pytest")
     (pytest-binary . "python -Wignore /home/jpaille/meilleursagents/apps/www/.venv/bin/pytest")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/Barometre/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/apps/Barometre/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/Barometre/.venv/bin/pytest")
     (isort-binary . "/home/jpaille/meilleursagents/apps/MediaAPI/.venv/bin/isort")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/MyPro/.venv")
     (isort-binary . "/home/jpaille/meilleursagents/apps/MyPro/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/MyPro/.venv/bin/pytest")
     (isort-binary . "/home/jpaille/meilleursagents/apps/www/.venv/bin/isort")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/MarketAPI/.venv/bin/pytest")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/Tools/.venv/bin/pytest")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/MediaAPI/.venv/bin/pytest")
     (pytest-binary . "/home/jpaille/meilleursagents/apps/www/.venv/bin/pytest")
     (pytest-venv-value . "True")
     (pytest-venv-key . "TEST_ACTIVE")
     (pytest-args . "-s -Wignore -vv")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/MarketAPI/.venv")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/Tools/.venv")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/MediaAPI/.venv")
     (jedi:server-args "--virtual-env" "/home/jpaille/meilleursagents/apps/www/.venv"))))
 '(show-paren-mode t)
 '(tab-width 11)
 '(tags-table-list (quote ("/home/julien/minix2/minix-2.0/fs/usr/TAGS")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredfl-flag-mark-line ((t (:foreground "yellow"))))
 '(highlight-indent-guides-even-face ((t (:background "color-18"))))
 '(smerge-lower ((t nil)))
 '(smerge-refined-added ((t nil)))
 '(smerge-refined-removed ((t (:inverse-video t))))
 '(smerge-upper ((t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               White space mode                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Show/delete trailing spaces disable for shell-mode
(setq-default show-trailing-whitespace nil)

;; Show white spaces
(global-set-key (kbd "C-x w") 'whitespace-mode)

;; Make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))



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

(global-set-key (kbd "C-x C-f") 'ido-find-file) ;; shell need to be sync

;; create buffer
(setq ido-create-new-buffer 'always)

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
(global-set-key (kbd "C-x n") 'buffer-menu)

;; Force Kill a buffer
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; don't ask confirmation when I want to revert a buffer
(setq revert-without-query '(".*"))
(global-set-key [f6] 'revert-buffer)

;; hard-revert
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PYTEST                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load-file "~/.emacs.d/my_packages/ma-pytest.el")
(defun ma-pytest-python-setup ()
  (ma-pytest-minor-mode)
  )

(add-hook 'python-mode-hook 'ma-pytest-python-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                ISORT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/my_packages/my_isort.el")
;; Isort buffer.
(global-set-key (kbd "C-c i") 'py-isort-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                BLACK                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Black region
(require 'python)
(define-key python-mode-map (kbd "C-c C-b") 'python-black-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                PYTHON                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t) ?? what is for 


;; Default jedi server
(setq jedi:server-args
      '("--virtual-env" "~/MeilleursAgents/apps/MediaAPI/.venv")
      )

;; Restart a jedi server with local jedi:server-args (.dir-locals) arg to point to right venv.
(defun jk ()
  (interactive)
  (jedi:stop-all-servers)
  (jedi:start-server)
)

;; Jedi go to definition
(defun jedi-custom-keys ()
  (local-set-key (kbd "C-x p") 'jedi:goto-definition)
  (local-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)
)
(add-hook 'jedi-mode-hook 'jedi-custom-keys)


;; Import snippet
(fset 'include
   "import pdb; pdb.set_trace()")
(global-set-key (kbd "C-x i") 'include)

;; Move up to python context.
(define-key python-mode-map (kbd "C-c C-u") 'python-nav-backward-up-list)

;; Isort buffer.
(global-set-key (kbd "C-c i") 'py-isort-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               FLYCHECK                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun flycheck-python-setup ()
  (flycheck-mode))

(add-hook 'python-mode-hook #'flycheck-python-setup)

(global-set-key (kbd "C-x j") 'flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               AUTO PAIRING                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAGIT                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit status in current window
(global-set-key [f5] 'magit-status)

;; enable cua-mode for magit
(require 'magit)
(define-key magit-diff-mode-map (kbd "C-c") 'cua-copy-region)
(define-key magit-file-section-map (kbd "C-c") 'cua-copy-region)
(define-key magit-hunk-section-map (kbd "C-c") 'cua-copy-region)
(define-key magit-unstaged-section-map (kbd "C-c") 'cua-copy-region)
(define-key magit-staged-section-map (kbd "C-c") 'cua-copy-region)

;; Jump directly to file in log
(define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-worktree-file)


;; original

(defun pr ()
  "Visit the current branch's PR on Github."
  (interactive)
  (setq git-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch)))
  (message git-url)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GREP                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for grep mode deactivate C-xg
(with-eval-after-load 'magit
  (define-key magit-file-mode-map "\C-xg" nil))
(global-set-key "\C-xg" 'rgrep)

;; find-name-dired TODO bind a shortcut

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
;;                                TERM                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-key term-mode-map (kbd "M-p") 'move_fast_up)
;; (define-key term-mode-map (kbd "M-n") 'move_fast_down)

;; (defun jnm/term-toggle-mode ()
;;   "Toggles term between line mode and char mode"
;;   (interactive)
;;   (if (term-in-line-mode)
;;       (term-char-mode)
;;     (term-line-mode)))

;; (define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
;; (define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

;; (define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
;; (define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                SHELL                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'shell)

;; Move fast like everywhere in comint buffers
(define-key comint-mode-map (kbd "M-p") 'move_fast_up)
(define-key comint-mode-map (kbd "M-n") 'move_fast_down)

;; Previous next input
(global-set-key  (kbd "M-o") 'comint-previous-input)
(global-set-key  (kbd "M-m") 'comint-next-input)

;; Shell shell-resync-dirs
(global-set-key "\M-\r" 'shell-resync-dirs)

;; Spawn a new shell
(defun spawn-shell (name)
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer)))
(global-set-key (kbd "M-.") 'spawn-shell)

;; go to prompt
(global-set-key (kbd "C-c e") 'comint-goto-process-mark)

;; Do not shadow prompt native color
(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;; Start every emacs session with a main shell and a secondary shell named *oo*.
(defun start-shells()
  (get-buffer-create "*shell*")
  (get-buffer-create "*oo*")
  (shell "*shell*")
  (shell "*oo*")
  (shell "*rebond*")
)
(start-shells)

;; Start dotfiles in sh mode
(add-to-list 'auto-mode-alist '("\\.bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.localrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env$" . sh-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                SSH-AGENT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; restart ssh-agent
(defun rs ()
  (interactive)
  (setq ssh-auth-sock  (shell-command-to-string "source /home/jpaille/reboot_ssh.sh ; reboot_ssh | grep ssh"))
  (setenv "SSH_AUTH_SOCK" ssh-auth-sock)
  (my-term-send-string "*shell*" "exec bash")
  (my-term-send-string "*oo*" "exec bash")
  (my-term-send-string "*rebond*" "exec bash")
  )

;; helper for restart ssh
(defun my-term-send-string (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "*terminal*"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (cond ((derived-mode-p 'comint-mode)
                 (insert input)
                 (comint-send-input))
                ((derived-mode-p 'term-mode)
                 (term-send-string process input)
                 (term-send-input))))))))


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
;;                                GROOVY                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-c-mode-hook ()
  (setq indent-tabs-mode nil
        c-basic-offset 4))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                TRAMP                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;                                MARKDOWN                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-n") nil)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                POMODORO                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun now ()
  "Insert string for the current time formatted like '2:34'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%-I:%M")))

(defun today ()
    "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%A, %B %e, %Y")))

(defvar *pomodoro-directory* "~/pomodoro_analyzer/pomodoro_files/")

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAN                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'man)
(define-key Man-mode-map (kbd "M-p") nil)
(define-key Man-mode-map (kbd "M-n") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                MAKEFILE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun disable-c-c ()
(local-set-key "\C-c" ctl-x-map)
)

(add-hook 'makefile-gmake-mode-hook 'disable-c-c)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                THEME                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-acario-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                C KERNEL MINIX                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate ctags : find . -type f -iname "*.[chS]" | xargs etags -a

;; C go to definition
(defun cc-custom-keys ()
  (local-set-key (kbd "C-x p") 'xref-find-definitions)
  (local-set-key (kbd "C-x ]") 'xref-pop-marker-stack)
)
(add-hook 'c-mode-hook 'cc-custom-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                JS MODE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind typescript files to js-mode
(add-to-list 'auto-mode-alist '("\\.ts$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . js-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Move text                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO interfere with shell move up / down

;; (defun move-text-default-bindings ()
;;   "Use default bindings for move-text-up and move-text-down (M-up / M-down)."
;;   (interactive)
;;   "Bind `move-text-up' and `move-text-down' to M-up & M-down."
;;   )

;(require 'move-text)
;(global-set-key  (kbd "M-o") 'move-text-up)
;(global-set-key  (kbd "M-m") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GO tpl                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "/home/jpaille/.emacs.d/my_packages/go-template-mode.el")
(require 'go-template-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . go-template-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GO                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                YAML                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'indent-guide)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . indent-guide-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GOLANG                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun godef-custom-keys ()
  (local-set-key (kbd "C-x p") 'godef-jump)
  (local-set-key (kbd "C-x ]") 'jedi:goto-definition-pop-marker)
)
(add-hook 'go-mode-hook 'godef-custom-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                USEFULL COMMANDS/BINDINGS           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; describe-text-properties : GET FACES AT POINT
;; Close all other windows C-x 1
;; C-x k kill-buffer
;; find-name-dired
;; revert-buffer f6
;; replay last test f9
;; go inside a container fs  /docker:user@container:/path/to/file
;; debug elisp : M-x edebug-defun
;; json-reformat-region
;; term C-c C-j/C-c C-k
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq highlight-indent-guides-method 'character)

(add-hook 'yaml-mode-hook 'setup-guides-method)

(defun setup-guides-method ()
(indent-guide-mode))


