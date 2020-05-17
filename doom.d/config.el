;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "An Chu"
      user-mail-address "chuvanan.cva@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Hasklig" :size 12))
(when (member "Hasklig" (font-family-list))
  (set-frame-font "Hasklig-9:weight=semibold" nil t))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; -----------------------------------------------------------------------------
;; Custom config
;; -----------------------------------------------------------------------------

;; full-screen by default
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;; I prefer 4-width tab
(setq-default default-tab-width 4)

;; Save last editied position
(save-place-mode 1)

;; No need to confirm killing process
(setq confirm-kill-processes nil)

;; Mouse avoidance
(mouse-avoidance-mode 'banish)

;; make company snapier
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-minimum-prefix-length 1)


;; Re-map key bindings ---------------------------------------------------------

;; Disabled confused commands
(unbind-key "C-x C-z")
(unbind-key "C-x m")

;; Kill this buffer
(map! "C-x k" #'kill-this-buffer)

;; Set join line instead of fill paragraph
(map! "M-q" #'delete-indentation)

;; Fill paragraph
(map! "M-p" #'fill-paragraph)

;; Undo
(map! "C-z" #'undo-fu-only-undo)

;; Rebind occur mode
(map! "M-o" #'occur)

(setq org-return-follows-link t)
(setq fill-column 80)

;; -----------------------------------------------------------------------------
;; ESS
;; -----------------------------------------------------------------------------


(after! ess
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)
  (add-hook 'ess-mode-hook (lambda () (ess-set-style 'RRR 'quite)))
  (add-hook 'inferior-ess-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'inferior-ess-mode-hook #'(lambda ()
                                        (setq-local comint-use-prompt-regexp nil)
                                        (setq-local inhibit-field-text-motion nil)))
  (add-hook 'ess-r-mode-hook
            (lambda()
              'eglot-ensure
              (make-local-variable 'company-backends)
              (delete-dups (push 'company-capf company-backends))
              (delete-dups (push 'company-files company-backends))))
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (setq ess-eval-empty t)               ; don't skip non-code line
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-move-point-for-output 'others)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-eval-visibly 'nowait)
  (setq ess-use-flymake nil)
  (setq ess-roxy-fold-examples nil)
  (setq ess-roxy-fontify-examples t)
  (setq ess-use-company 'script-only)
  (setq ess-company-arg-prefix-length 1)
  (setq ess-blink-region nil)
  (setq ess-r-flymake-lintr-cache nil)
  (setq inferior-R-args "--no-restore-history --no-save")
  (setq ess-offset-arguments 'prev-line)
  (setq ess-assign-list '(" = "))
  (setq ess-eldoc-show-on-symbol nil)
  (setq ess-eldoc-abbreviation-style 'mild)
  (setq ess-use-eldoc nil)
  (map! :map ess-r-mode-map
        "<C-return>" #'ess-eval-region-or-function-or-paragraph
        "<f5>" #'ess-display-help-on-object
        "<M-return>" #'ess-eval-line
        "<S-return>" #'anchu/ess-eval-word
        "C-S-m" #'anchu/insert_then_R_operator
        "C-S-i" #'anchu/insert_in_operator
        "C-c C-a n" #'anchu/insert-minor-section
        "C-c C-a m" #'anchu/insert-major-section
        "C-c C-a d" #'anchu/insert-named-comment
        "M--" #'ess-insert-assign)
  (setq ess-R-font-lock-keywords
      (quote
       ((ess-R-fl-keyword:modifiers . t)
        ;; (ess-R-fl-keyword:fun-defs . t)
        ;; (ess-R-fl-keyword:fun-defs2 . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators)
        (ess-fl-keyword:delimiters)
        (ess-fl-keyword:=)
        ;; (ess-fl-keyword::= . t)
        (ess-R-fl-keyword:F&T)
        (ess-R-fl-keyword:%op%)))))


;; evaluate object at point
;; http://www.emacswiki.org/emacs/ess-edit.el
(defun anchu/ess-edit-word-at-point ()
  "Find word at point."
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))
;; eval any word where the cursor is (objects, functions, etc)
(defun anchu/ess-eval-word ()
  (interactive)
  (let ((x (anchu/ess-edit-word-at-point)))
    (ess-eval-linewise (concat x))))


;; %>% operator
(defun anchu/insert_then_R_operator ()
  "R - %>% operator or 'then' pipe operator."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

;; %in% operator
(defun anchu/insert_in_operator ()
  "R - %in% operator"
  (interactive)
  (just-one-space 1)
  (insert "%in%")
  (just-one-space 1))

;; minor comment section
(defun anchu/insert-minor-section ()
  "Insert minor section heading for a snippet of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## "))

;; named commnet section
(defun anchu/insert-named-comment (cmt)
  "Make comment header."
  (interactive "sEnter your comment: ")
  (let* ((user-cmt (concat "## " cmt " "))
         (len-user-cmt (length user-cmt))
         (len-hyphen (- 80 len-user-cmt)))
    (insert user-cmt (apply 'concat (make-list len-hyphen "-")))
    (newline)
    (newline)
    )
  )

;; major comment section
(defun anchu/insert-major-section ()
  "Insert major section heading for a block of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## ")
  (save-excursion
    (insert "\n")
    (insert "## -----------------------------------------------------------------------------\n")))

;; -----------------------------------------------------------------------------
;; Window numbering
;; -----------------------------------------------------------------------------


(use-package! window-numbering
  :config
  (window-numbering-mode 1))


;; -----------------------------------------------------------------------------
;; Electric Operator
;; -----------------------------------------------------------------------------

(use-package! electric-operator
  :after ess
  :config
  (add-hook 'ess-mode-hook #'electric-operator-mode)
  (add-hook 'inferior-ess-mode-hook #'electric-operator-mode)
  (setq electric-operator-R-named-argument-style 'spaced)
  (electric-operator-add-rules-for-mode 'ess-r-mode
                                        (cons ":=" " := ")
                                        (cons "%" nil)
                                        (cons "%in%" " %in% ")
                                        (cons "%%" " %% ")
                                        (cons "!=" " != ")
                                        (cons "<=" " <= ")
                                        (cons ">=" " >= ")
                                        (cons "~" " ~ ")
                                        (cons ";" "; "))
  (electric-operator-add-rules-for-mode 'inferior-ess-r-mode
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

;; -----------------------------------------------------------------------------
;; Ivy
;; -----------------------------------------------------------------------------

(after! ivy
  (setq enable-recursive-minibuffers nil)
  (map! "C-x C-b" #'ivy-switch-buffer))


;; -----------------------------------------------------------------------------
;; Magit
;; -----------------------------------------------------------------------------

(map! "<f12>" #'magit-status)
