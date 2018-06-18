;; init.el --- AnChu's Emacs configuration
;; Refactor on 02/06/2018

;; -----------------------------------------------------------------------------
;; Starting up
;; -----------------------------------------------------------------------------

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)    ; to prevent accidentally loading packages twice

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "This config requires Emacs v%s or higher" minver)))

;; Add package sources
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; I prioritize stable package releases
(setq package-archive-priorities '(("melpa-stable" . 10)
                                   ("gnu-elpa"     . 5)
                                   ("melpa"        . 0)))
(unless package-archive-contents
  (package-refresh-contents))

;; Set up my package directory
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

;; Personal information
(setq user-full-name "An Chu"
      user-mail-address "chuvanan.cva@gmail.com")

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(use-package diminish)                  ; to enable :diminish
(use-package bind-key)                  ; to enable :bind
(setq load-prefer-newer t)              ; always load newest byte code

;; -----------------------------------------------------------------------------
;; General configuration
;; -----------------------------------------------------------------------------

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 50MB
(setq large-file-warning-threshold 50000000)

;; I prefer a central place for all backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Disabled confused commands
(unbind-key "C-x C-z")                  ; suspend-frame
(unbind-key "C-x m")			; compose-mail

;; Font for source code
(when (member "Hasklig" (font-family-list))
  (set-frame-font "Hasklig 10" nil t))

;; Space is expensive. So remove unnecessary GUI element
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Reverts any buffer associated with a file when the file changes on disk
(global-auto-revert-mode t)

;; UX
(global-hl-line-mode +1)
(global-prettify-symbols-mode +1)

(setq line-move-visual t)

(setq scroll-step 1
      scroll-margin 5
      hscroll-step 1
      hscroll-margin 3
      scroll-conservatively  10000)

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))
(set-fringe-mode '(4 . 0))
(put 'narrow-to-region 'disabled nil)

(setq-default fill-column 80)
(setq-default default-tab-width 4)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

;; Require a newline at the end of files
(setq require-final-newline t)

;; Insert new line if the point is at the end of the buffer
(setq next-line-add-newlines t)

;; The blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; Disable startup screen, message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message #'ignore)
(setq use-dialog-box nil)

;; Turn off bidirectional text
(setq-default bidi-paragraph-direction 'left-to-right)

;; Mode line settings
(setq line-number-mode t)
(column-number-mode t)

;; Mouse avoidance
(mouse-avoidance-mode 'banish)

;; Fullscreen
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;; Indenting
(electric-indent-mode 1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Truncate longline as default
(set-default 'truncate-lines t)

;; Disable the current buffer mark when changing buffer
(transient-mark-mode 1)

;; Set up coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Speed up echo commands
(setq echo-keystrokes 0)
(setq focus-follows-mouse t)

;; Highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-delay 0)

;; Turn off auto revert messages
(setq auto-revert-verbose nil)

;; Auto save abbreviation
(setq save-abbrevs 'silently)

;; Emacs 26.1
(setq confirm-kill-processes nil)

(electric-pair-mode 1)
(global-set-key (kbd "<f7>") 'ispell-word)
(setq frame-title-format "%b")

;; Rings and registers
(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      save-interprogram-paste-before-kill t)

;; Bound undo to C-z
(global-set-key (kbd "C-z") 'undo)

;; Other navigation bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Set join line instead of fill paragraph
(global-set-key (kbd "M-q") 'delete-indentation)

;; Fill paragraph
(global-set-key (kbd "M-p") 'fill-paragraph)

;; Rebind occur mode
(global-set-key (kbd "M-o") 'occur)

;; Comment line
(global-set-key (kbd "C-M-;") 'comment-line)

;; Use hippie-expand instead of dabbrev
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key (kbd "M-/") #'hippie-expand)

;; vc-diff shortcut
(global-set-key (kbd "<f10>") 'vc-diff)

;; Config for SQLi
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
(add-hook 'sql-mode-hook 'sqlup-mode)
;; (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(add-hook 'redis-mode-hook 'sqlup-mode)
(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

;; (add-hook 'sql-mode-hook **/
;;           (lambda () **/
;;             (setq comment-start "/* ") **/
;;             (setq comment-end " *\/"))) **/

;; https://www.emacswiki.org/emacs/SqlQueryBuffer
(defun my-sql-query-buffer (arg)
  "Open a `sql-mode' buffer which interacts with the current SQLi buffer.
 Switches to an existing buffer if possible, otherwise creates a new buffer.
 With C-u prefix arg, always creates a new buffer."
  (interactive "P")
  (let ((sqlibuf (current-buffer)))
    (if (null (sql-buffer-live-p sqlibuf))
        (error "Buffer %s is not a working SQLi buffer" sqlibuf)
      (let ((product sql-product)
            (querybuf
             (or (and (not (consp arg)) ;; prefix arg
                      (boundp 'my-sql-query-buffer)
                      (buffer-live-p (get-buffer my-sql-query-buffer))
                      (get-buffer my-sql-query-buffer))
                 (generate-new-buffer
                  (format "*SQL ctl: %s*" (buffer-name sqlibuf))))))
        (setq-local my-sql-query-buffer querybuf)
        (pop-to-buffer querybuf '(display-buffer-reuse-window
                                  . ((reusable-frames . visible))))
        (unless (eq major-mode 'sql-mode)
          (sql-mode)
          (setq sql-product product)
          (sql-highlight-product)
          (setq sql-buffer sqlibuf)
          (run-hooks 'sql-set-sqli-hook))))))

(eval-after-load "sql"
  '(define-key sql-interactive-mode-map (kbd "C-c q") 'my-sql-query-buffer))

;; -----------------------------------------------------------------------------
;; use-package packages
;; -----------------------------------------------------------------------------

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; https://github.com/magit/magit
(use-package magit
  :config
  (global-set-key (kbd "<f12>") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; https://github.com/camdez/goto-last-change.el
(use-package goto-last-change
  :config
  (global-set-key (kbd "C-x C-\\") 'goto-last-change))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :diminish multiple-cursors-mode
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  :config
  (yas-reload-all)
  (add-hook 'ess-mode-hook #'yas-minor-mode)
  (setq yas-snippet-dirs (format "%s%s" user-emacs-directory "snippets"))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))

(use-package saveplace
  :defer 1
  :ensure t
  :init (save-place-mode 1))

;; https://github.com/vspinu/polymode
(use-package polymode
  :diminish (poly-org-mode
	     poly-markdown-mode
	     poly-noweb+r-mode
	     poly-noweb+r-mode
	     poly-markdown+r-mode
	     poly-rapport-mode
	     poly-html+r-mode
	     poly-brew+r-mode
	     poly-r+c++-mode
	     poly-c++r-mode)
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
  (define-key polymode-mode-map "\M-p" 'fill-paragraph))

;; built-in package
(use-package savehist
  :init (savehist-mode t)
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval 180
        history-delete-duplicates t)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring)))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters)

;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (define-key markdown-mode-map "\M-p" 'fill-paragraph))

;; https://www.emacswiki.org/emacs/uniquify
;; (use-package uniquify
;;   :defer 1
;;   :config
;;   (setq uniquify-buffer-name-style 'forward)
;;   (setq uniquify-separator "/")
;;   ;; rename after killing uniquified
;;   (setq uniquify-after-kill-buffer-p t)
;;   ;; don't muck with special buffers
;;   (setq uniquify-ignore-buffers-re "^\\*"))

;; Don't know why use-package failed to load uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; https://github.com/abo-abo/avy
(use-package avy
  :bind (("M-s M-s" . avy-goto-word-or-subword-1)))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package recentf
  :defer 1
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; (use-package dired+
;;   :load-path "~/.emacs.d/elpa/dired+"
;;   :config
;;   (setq dired-listing-switches "-alh")
;;   (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired-subtree
  :defer 1
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :config
  (window-numbering-mode 1))

;; https://github.com/purcell/color-theme-sanityinc-tomorrow
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :defer 1
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'ess-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; https://github.com/bbatsov/crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c I" . crux-find-user-init-file)))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :bind (("<f11>" . projectile-vc)))

;; https://github.com/aspiers/smooth-scrolling/
(use-package smooth-scrolling
  :diminish smooth-scrolling-mode
  :config
  (smooth-scrolling-mode 1))

;; https://github.com/rakanalh/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))))

;; https://github.com/davidshepherd7/electric-operator
(use-package electric-operator
  :config
  (setq electric-operator-R-named-argument-style 'spaced)
  (add-hook 'ess-mode-hook #'electric-operator-mode)
  (add-hook 'inferior-ess-mode-hook #'electric-operator-mode)
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (electric-operator-add-rules-for-mode 'ess-mode
                                        (cons ":=" " := ")
                                        (cons "%" nil)
                                        (cons "%in%" " %in% ")
                                        (cons "%%" " %% ")
                                        (cons "!=" " != ")
                                        (cons "<=" " <= ")
                                        (cons ">=" " >= ")
                                        (cons ";" "; "))
  (electric-operator-add-rules-for-mode 'inferior-ess-mode
                                        (cons ":=" " := ")
                                        (cons "==" " == ")
                                        (cons "=" " = ")
                                        (cons "%" nil)
                                        (cons "%in%" " %in% ")
                                        (cons "%%" " %% ")
                                        (cons "!=" " != ")
                                        (cons "<=" " <= ")
                                        (cons ">=" " >= ")
                                        (cons ";" "; ")
                                        (cons "," ", ")))

;; https://github.com/Wilfred/ag.el
(use-package ag
  :defer 1
  :init
  ;; Truncate long results
  (add-hook 'ag-mode-hook (lambda () (setq truncate-lines t)))
  :config
  ;; Add highlighting
  (setq ag-highlight-search t)
  ;; Set ag to reuse the same buffer
  (setq ag-reuse-buffers nil))

;; https://github.com/company-mode/company-mode
(use-package company
  :diminish company-mode
  :init
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-ignore-case nil)
  (setq-local company-backends
              (append '((company-dabbrev-code
                         company-R-args
                         company-R-objects
                         company-jedi))
                      company-backends))
  (add-to-list 'company-dabbrev-code-modes 'ess-mode)
  :bind (:map company-active-map
              ([tab] . company-complete-common-or-cycle)
              ("TAB" . company-complete-common-or-cycle)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; https://github.com/milkypostman/powerline
(use-package powerline
  :config
  (powerline-center-theme)
  (setq powerline-arrow-shape 'arrow14))

;; built-in package
(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;; -----------------------------------------------------------------------------
;; ESS
;; -----------------------------------------------------------------------------

(use-package ess
  :defer t
  :init
  (require 'ess-site)
  (require 'ess-rutils)
  ;; Auto set width and length options when initiate new Ess processes
  :config
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)
  (add-hook 'ess-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (add-hook 'ess-mode-hook
            (lambda () (ess-set-style 'RRR 'quiet)
              (add-hook 'local-write-file-hooks
                        (lambda () (ess-nuke-trailing-whitespace)))))
  (add-hook 'inferior-ess-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-eval-visibly nil)
  (setq ess-use-flymake nil)
  (setq ess-roxy-fold-examples t)
  (setq ess-roxy-fontify-examples t)
  (setq ess-use-company nil)
  (setq inferior-R-args "--no-restore-history --no-save")
  (setq ess-offset-arguments 'prev-line)

  ;; fix assignment key
  (setq ess-S-assign-key (kbd "M--"))
  (ess-toggle-S-assign-key t) ; enable above key definition
  ;; leave my underscore key alone!
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)
  (ess-toggle-underscore nil)
  (ess-disable-smart-S-assign nil)

  (setq ess-use-eldoc 'script-only)
  (setq ess-eldoc-show-on-symbol nil)
  (setq ess-eldoc-abbreviation-style t)
  (setq comint-scroll-to-bottom-on-output t)
  :bind (:map ess-mode-map
              ("C-c C-w w" . ess-r-package-use-dir)
              ("C-c C-w C-w" . ess-r-package-use-dir)
              ("<C-return>" . ess-eval-region-or-function-or-paragraph-and-step)
              ("<C-S-return>" . ess-eval-buffer)
              ("C-M-;" . comment-line)
              ("C-S-<f10>" . inferior-ess-reload))
  :bind (:map inferior-ess-mode-map
              ("C-S-<f10>" . inferior-ess-reload)))

;; syntax highlight
(setq ess-R-font-lock-keywords
      (quote
       ((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators)
        (ess-fl-keyword:delimiters)
        (ess-fl-keyword:=)
        (ess-R-fl-keyword:F&T)
        (ess-R-fl-keyword:%op%))))

(setq inferior-ess-r-font-lock-keywords
      (quote
       ((ess-S-fl-keyword:prompt . t)
        (ess-R-fl-keyword:messages . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:matrix-labels)
        (ess-fl-keyword:fun-calls)
        (ess-fl-keyword:numbers)
        (ess-fl-keyword:operators)
        (ess-fl-keyword:delimiters)
        (ess-fl-keyword:=)
        (ess-R-fl-keyword:F&T))))

;; %>% operator
(defun anchu/isnet_then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

(define-key ess-mode-map (kbd "C-S-m") 'anchu/isnet_then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-S-m") 'anchu/isnet_then_R_operator)

;; %in% operator
(defun anchu/insert_in_operator ()
  "R - %in% operator"
  (interactive)
  (just-one-space 1)
  (insert "%in%")
  (just-one-space 1))

(define-key ess-mode-map (kbd "C-S-i") 'anchu/insert_in_operator)
(define-key inferior-ess-mode-map (kbd "C-S-i") 'anchu/insert_in_operator)

;; <<- operator
(defun anchu/insert_double_assign_operator ()
  "R - <<- operator"
  (interactive)
  (just-one-space 1)
  (insert "<<-")
  (just-one-space 1))

(define-key ess-mode-map (kbd "C-M-=") 'anchu/insert_double_assign_operator)
(define-key inferior-ess-mode-map (kbd "C-M-=") 'anchu/insert_double_assign_operator)

;; -> operator
(defun anchu/insert_right_assign_operator ()
  "R - %in% operator"
  (interactive)
  (just-one-space 1)
  (insert "->")
  (just-one-space 1))

(define-key ess-mode-map (kbd "C-M--") 'anchu/insert_right_assign_operator)
(define-key inferior-ess-mode-map (kbd "C-M--") 'anchu/insert_right_assign_operator)

(defun anchu/ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "library(rmarkdown); rmarkdown::render(\"%s\")"
                      buffer-file-name)))
        (message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-ns" 'anchu/ess-rmarkdown)

(defun anchu/ess-rshiny ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "library(rmarkdown); rmarkdown::run(\"%s\")"
                      buffer-file-name)))
        (message "Running shiny on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-nr" 'anchu/ess-rshiny)

(defun anchu/ess-publish-rmd ()
  "Publish R Markdown (.Rmd) to remote server"
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      ;; assignment
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "workflow::wf_publish_rmd(\"%s\")"
                      buffer-file-name)))
        ;; execute
        (message "Publishing rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-np" 'anchu/ess-publish-rmd)

(defun anchu/insert-minor-section ()
  "Insert minor section heading for a snippet of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## "))

(define-key ess-mode-map (kbd "C-c C-a n") 'anchu/insert-minor-section)

(defun anchu/insert-r-code-chunk ()
  "Insert R Markdown code chunk."
  (interactive)
  (insert "```{r, include=FALSE}\n")
  (insert "\n")
  (save-excursion
    (insert "\n")
    (insert "\n")
    (insert "```\n")))

(define-key polymode-mode-map (kbd "C-c C-a c") 'anchu/insert-r-code-chunk)

(defun anchu/insert-major-section ()
  "Insert major section heading for a block of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## ")
  (save-excursion
    (insert "\n")
    (insert "## -----------------------------------------------------------------------------\n")))

(define-key ess-mode-map (kbd "C-c C-a m") 'anchu/insert-major-section)

(defun anchu/insert-resource-header ()
  "Insert yaml-like header for R script resources."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## code: ")
  (save-excursion
    (insert "\n")
    (insert "## description: \n")
    (insert "## author: \n")
    (insert (concat "## date: " (current-time-string) "\n"))
    (insert "## -----------------------------------------------------------------------------\n")))

(define-key ess-mode-map (kbd "C-c C-a r") 'anchu/insert-resource-header)

(defun anchu/insert-yalm-header ()
  "Insert Rmd header."
  (interactive)
  (insert "---\n")
  (insert "title: ")
  (save-excursion
    (newline)
    (insert "author: \n")
    (insert "date: \"`r format(Sys.time(), '%d-%m-%Y %H:%M:%S')`\"\n")
    (insert "runtime: shiny\n")
    (insert "output:\n")
    (indent-to-column 4)
    (insert "html_document:\n")
    (indent-to-column 8)
    (insert "theme: flatly\n")
    (insert "---")
    (newline)))

(define-key polymode-mode-map (kbd "C-c C-a y") 'anchu/insert-yalm-header)

(defun anchu/insert-named-comment (cmt)
  "Make comment header"
  (interactive "sEnter your comment: ")
  (let* ((user-cmt (concat "## " cmt " "))
         (len-user-cmt (length user-cmt))
         (len-hyphen (- 80 len-user-cmt)))
    (insert user-cmt (apply 'concat (make-list len-hyphen "-")))
    (newline)
    (newline)
    )
  )

(define-key ess-mode-map (kbd "C-c C-a d") 'anchu/insert-named-comment)

;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :after python
  :init
  ;; Truncate long line in inferior mode
  (add-hook 'inferior-python-mode-hook (lambda () (setq truncate-lines t)))
  ;; Enable company
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'inferior-python-mode-hook 'company-mode)
  ;; Enable highlight indentation
  (add-hook 'highlight-indentation-mode-hook
            'highlight-indentation-current-column-mode)
  ;; Enable elpy
  (elpy-enable)
  :config
  ;; Do not enable elpy flymake for now
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

  ;; The old `elpy-use-ipython' is obseleted, see:
  ;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
  ;; (setq python-shell-interpreter "ipython3"
  ;; python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        python-shell-completion-native-disabled-interpreters '("python"))
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (setq elpy-rpc-python-command "python3")

  ;; Completion backend
  (setq elpy-rpc-backend "jedi")

  ;; Function: send block to elpy: bound to C-c C-c
  (defun forward-block (&optional n)
    (interactive "p")
    (let ((n (if (null n) 1 n)))
      (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

  (defun elpy-shell-send-current-block ()
    (interactive)
    (beginning-of-line)
    "Send current block to Python shell."
    (push-mark)
    (forward-block)
    (elpy-shell-send-region-or-buffer)
    (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                    nil
                    'visible))

  ;; Font-lock
  (add-hook 'python-mode-hook
            '(lambda()
               (font-lock-add-keywords
                nil
                '(("\\<\\([_A-Za-z0-9]*\\)(" 1
                   font-lock-function-name-face) ; highlight function names
                  ))))

  :bind
  (:map python-mode-map
        ("C-c <RET>" . elpy-shell-send-region-or-buffer)
        ("C-c C-c" . elpy-send-current-block))
  )


;; -----------------------------------------------------------------------------
;; Ivy
;; -----------------------------------------------------------------------------

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  :bind* (("C-s" . swiper)
          ("<f6>" . ivy-resume)
          ("C-x C-b" . ivy-switch-buffer)
          ("C-c C-w" . ivy-wgrep-change-to-wgrep-mode)
          ("C-c v" . ivy-push-view)
          ("C-c V" . ivy-pop-view)))

(use-package counsel
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind*
  (("C-x C-f" . counsel-find-file)
   ("C-c C-r" . counsel-recentf)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c k" . counsel-ag)
   ("C-c g" . counsel-grep)
   ("C-c j" . counsel-git-grep)
   ("M-y" . counsel-yank-pop)
   ("C-h a" . counsel-apropos)
   ("C-h b" . counsel-descbinds)
   ("M-x" . counsel-M-x)
   ("C-x r b" . counsel-bookmark)
   ))

(defun counsel-goto-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection :action 'dired)))

(global-set-key (kbd "C-x C-d") 'counsel-goto-recent-directory)

;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------

(use-package org
  :config
  (setq org-log-done t)
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "CANCELED" "DONE")))
  (setq org-agenda-files '("~/Dropbox/org-mode/"))
  (setq org-return-follows-link t)
  (setq org-startup-with-inline-images t)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (plantuml . t)))
  (setq org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("\C-cb" . org-iswitchb)))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; -----------------------------------------------------------------------------
;; Custom functions
;; -----------------------------------------------------------------------------

;; redefinde kill line and kill region
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "C-j") 'jump-to-mark)

;; faster pop-to-mark command
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)
(setq set-mark-command-repeat-pop t)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") 'move-line-down)
(global-set-key (kbd "C-S-k") 'move-line-up)

(defun anchu/set-cursor ()
  (cond
   (buffer-read-only
    (setq cursor-type 'box)
    (set-cursor-color "gold"))
   (t
    (setq cursor-type 'box)
    (set-cursor-color "gray")))
  ;; red cursor for overwrite mode
  (when overwrite-mode
    (set-cursor-color "red")))

(add-hook 'post-command-hook 'anchu/set-cursor)


(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-maybe-expand 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode) ;; xxx change this to point to right var
            (null (when (looking-at "\\_>") (do-yas-expand))))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(define-key prog-mode-map [tab] 'tab-indent-or-complete)
(define-key prog-mode-map (kbd "TAB") 'tab-indent-or-complete)

(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; https://gist.github.com/reiver-dev/82da77ba3f0008c56624661a7375e0e8#file-ligatures-el
(defconst ligatures-hasklig-code-start #Xe100)

(defconst ligatures-hasklig-code-list
  '("&&" "***" "*>" "\\\\" "||" "|>" "::"
    "==" "===" "==>" "=>" "=<<" "!!" ">>"
    ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
    "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
    "<<" "<<<" "<+>" ".." "..." "++" "+++"
    "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")
  "Ordered ligatures for Hasklig Code font")

(defun ligatures-correct-symbol-bounds (len char)
  "Prepend up to LEN non-breaking spaces with reference points to CHAR.
This way `compose-region' called by function `prettify-symbols-mode'
will use the correct width of the symbols instead of the width
measured by `char-width'."
  (let ((acc (list char)))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc)))
      (setq len (1- len)))
    acc))


(defun ligatures-make-alist (ligatures starting-code)
  "Construct text to ligature character.
For each string in LIGATURES list add replacement from STARTING-CODE
sequentially."
  (mapcar (lambda (l)
            (let ((n starting-code))
              (setq starting-code (1+ starting-code))
              (when l
                (cons l (ligatures-correct-symbol-bounds
                         (length l) n)))))
          ligatures))

(defun ligatures-hasklig-code-setup ()
  "Add Hasklig Code ligatures to `prettify-symbols-alist'."
  (setq prettify-symbols-alist (append (ligatures-make-alist
                                        ligatures-hasklig-code-list
                                        ligatures-hasklig-code-start)
                                       prettify-symbols-alist)))

(add-hook 'ess-mode-hook 'ligatures-hasklig-code-setup)
(add-hook 'inferior-ess-mode-hook 'ligatures-hasklig-code-setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dired+ ob crux dired uniquify zenburn-theme yaml-mode window-numbering which-key wgrep-ag use-package undo-tree solarized-theme smooth-scrolling smartparens rainbow-delimiters python-mode python powerline polymode paradox org-bullets nord-theme neotree multiple-cursors markdown-mode magit ledger-mode jedi ivy-historian inlineR imenu-anywhere iedit ibuffer-vc ibuffer-projectile hungry-delete helm-projectile gruvbox-theme goto-last-change fullframe expand-region eval-in-repl ess elpy electric-operator e2wm-R dumb-jump dired-subtree diminish dashboard counsel-projectile company-ycmd company-jedi color-theme-sanityinc-tomorrow anzu all-the-icons-ivy all-the-icons-dired aggressive-indent ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
