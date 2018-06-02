;; init.el --- AnChu's Emacs configuration
;; Refactor on 02/06/2018

;; -----------------------------------------------------------------------------
;; Starting up
;; -----------------------------------------------------------------------------

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)    ; to prevent accidentally loading packages twice

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

;; Set out my package directory
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

;; Personal information
(setq user-full-name "An Chu"
      user-mail-address "chuvanan.cva@gmail.com")

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)              ; always load newest byte code

;; -----------------------------------------------------------------------------
;; General configuration
;; -----------------------------------------------------------------------------


;; ------------------------------
;; Backups

;; I prefer a central place for all backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ------------------------------
;; Windows configuration

;; Space is expensive
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

;; Turn off auto revert messages
(setq auto-revert-verbose nil)

;; Emacs 26.1
(setq confirm-kill-processes nil)

(electric-pair-mode 1)
(global-set-key (kbd "<f7>") 'ispell-word)
(setq frame-title-format "%b")

;; -----------------------------------------------------------------------------
;;
;; -----------------------------------------------------------------------------

;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "<f12>") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; https://github.com/camdez/goto-last-change.el
(use-package goto-last-change
  :ensure t
  :config
  (global-set-key (kbd "C-x C-\\") 'goto-last-change))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :diminish multiple-cursors-mode
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'ess-mode-hook #'yas-minor-mode))

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
(use-package rainbow-delimiters
  :ensure t)

;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :diminish anzu-mode
  :ensure t
  :config
  (global-anzu-mode t)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :ensure t
  :config
  (global-hungry-delete-mode))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; https://www.emacswiki.org/emacs/uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind (("M-s M-s" . avy-goto-word-or-subword-1)))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode +1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package dired
  :config
  (setq dired-listing-switches "-alh")
  (require 'dired-x))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

;; https://github.com/purcell/color-theme-sanityinc-tomorrow
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
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
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c I" . crux-find-user-init-file)))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t))

;; https://github.com/aspiers/smooth-scrolling/
(use-package smooth-scrolling
  :ensure t
  :diminish smooth-scrolling-mode
  :config
  (smooth-scrolling-mode 1))

;; https://github.com/rakanalh/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

;; -----------------------------------------------------------------------------
;; ESS
;; -----------------------------------------------------------------------------

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


;; -----------------------------------------------------------------------------
;; Ivy
;; -----------------------------------------------------------------------------

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

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob
  :ensure org
  :after org
  :config
  ;; Active Babel languages:
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (plantuml . t)))

  (setq org-src-tab-acts-natively t     ;Indent normally in source code
        org-src-fontify-natively t      ;Fontification in org source block
        org-confirm-babel-evaluate nil  ;Inhibit execute messages
        ))



;; -----------------------------------------------------------------------------
;; Custom functions
;; -----------------------------------------------------------------------------

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


;; Allow access from emacsclient

(require 'server)
(unless (server-running-p)
  (server-start))

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
