;; -*- lexical-binding: t -*-

(setopt custom-file "~/.emacs.custom")
(load custom-file)

;; Initialize package
(setopt package-native-compile t)
(setopt package-archives
    '(("gnu" . "http://elpa.gnu.org/packages/")
      ("gnu-devel" . "https://elpa.gnu.org/devel/")
      ("nongnu" . "http://elpa.nongnu.org/nongnu/")
      ("melpa" . "http://melpa.org/packages/")))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(x pgtk))
    (exec-path-from-shell-initialize)))

(add-to-list 'exec-path "/home/linuxbrew/.linuxbrew/bin")
(add-to-list 'exec-path "/usr/local/texlive/2025/bin/")
(add-to-list 'exec-path "/home/arthurmosley/.bun/bin")

(setq use-package-always-ensure t)

;; Disable all themes before loading a new one
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'doom-one)

;; early in init.el
(use-package emacs
  :ensure nil
  :init
  (require-theme 'modus-themes)
  :config
  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  :custom
  (tab-always-indent t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(set-face-attribute 'default nil
                    :font "Monaspace Neon"   ; paid, beautiful
                    ;; alternatives:
                    ;; "Iosevka"            ; free, very customizable
                    ;; "Monaspace Neon"     ; free, Microsoft, ligature support
                    ;; "Maple Mono"         ; free, great ligatures
                    :height 120            ; 130 = 13pt, tweak to taste
                    :weight 'regular)

(setq-default line-spacing 4)
(fringe-mode '(8 . 8))
(show-paren-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ----------------------- TREESITTER SETUP ----------------------- ;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 't)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                       "master" "typescript/src"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript"
                       "master" "tsx/src"))))

;; Remap major modes to treesitter versions
(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (clojure-mode    . clojure-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tsx-mode        . tsx-ts-mode)
        (c++-mode        . c++-ts-mode)
        (csharp-mode     . csharp-ts-mode)
        (css-mode        . css-ts-mode)
        (html-mode       . html-ts-mode)))

(setq auto-mode-alist
      (append '(("\\.ts\\'"  . typescript-ts-mode)
                ("\\.tsx\\'" . tsx-ts-mode)
                ("\\.h\\'"   . c++-ts-mode)
                ("\\.py\\'"  . python-ts-mode))
              auto-mode-alist))

(setopt sentence-end-double-space nil)

;; ----------------------- BASIC SETTINGS ----------------------- ;;
(global-unset-key (kbd "C-z"))
(setq make-backup-files nil)

(setopt user-full-name "Arthur Mosley")
(setopt user-mail-address "arthurcharlesmosley@gmail.com")

(setopt debug-on-error t)
(setopt byte-compile-debug t)
(setopt auto-save-default nil)
(setopt make-backup-files nil)

(setopt kill-whole-line t)
(setopt kill-read-only-ok t)
(setopt require-final-newline 'visit)

(setopt scroll-error-top-bottom t)
(setopt focus-follows-mouse t)
(setopt recenter-positions '(top bottom middle))
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(auto-insert-mode 1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-mode 1)
(tool-bar-mode -1)
(delete-selection-mode 1)

(setopt default-frame-alist
    '((menu-bar-lines . 0)
      (tool-bar-lines . 0)
      (internal-border-width . 16)
      (undecorated . t)))

(setopt initial-scratch-message "")
(setopt initial-major-mode 'org-mode)
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message "Arthur")
(setopt use-dialog-box nil)
(setopt line-move-visual nil)
(setopt visible-bell t)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

(save-place-mode 1)

;; ----------------------- IBUFFER ----------------------- ;;
(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)  ;; Fixed typo here
  (setq ibuffer-user-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-lock-doc-face)
  (setq ibuffer-user-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups
    '(("Main"
       ("Directories" (mode . dired-mode))
       ("Clojure" (or
               (mode . clojure-ts-mode)
               (mode . clojure-mode)
               (mode . cider-mode)))
       ("Python"
        (mode . python-ts-mode))
       ("Fundamental" (or
               (mode . fundamental-mode)
               (mode . text-mode)))
       ("Emacs" (or
                      (mode . emacs-lisp-mode)
                      (name . "^\\*Help\\*$")
                      (name . "^\\*Custom.*")
                      (name . "^\\*Org Agenda\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . (lambda ()
            (ibuffer-switch-to-saved-filter-groups "Main"))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ----------------------- QOL PACKAGES ----------------------- ;;
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package winner
  :config
  (winner-mode 1))

(setopt line-number-mode t)
(setopt column-number-mode t)

(use-package nerd-icons :ensure t)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-env-version t)
  (doom-modeline-lsp t))

;; ----------------------- COMPLETION PACKAGES ----------------------- ;;
(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

(use-package consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
     :map project-prefix-map
     ("s" . consult-ripgrep)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(with-eval-after-load 'consult
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;; macOS key mappings
(when (eq system-type 'darwin)
  (setq x-meta-keysym 'super
        x-super-keysym 'meta))

;; ----------------------- PROGRAMMING SETUP ----------------------- ;;

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(add-to-list 'display-buffer-alist
             '("^\\*eldoc\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 1)
               (window-height . 0.2)))

(keymap-global-set "C-c C" #'compile)

(use-package hungry-delete
  :ensure t
  :hook (prog-mode . hungry-delete-mode)
  :config
  (setq hungry-delete-chars-to-skip " \t\r\f\v"))

(use-package smartparens
  :init (require 'smartparens-config)
  :hook ((prog-mode . smartparens-mode)
     (clojure-ts-mode . smartparens-strict-mode)
     (cider-repl-mode . smartparens-strict-mode)
     (emacs-lisp-mode . smartparens-strict-mode))
  :bind (:map smartparens-mode-map
              ;; Movement
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("M-F"   . sp-forward-symbol)
              ("M-B"   . sp-backward-symbol)
              ;; Slurp/Barf
              ("C-)"   . sp-forward-slurp-sexp)
              ("C-}"   . sp-forward-barf-sexp)
              ("C-("   . sp-backward-slurp-sexp)
              ("C-{"   . sp-backward-barf-sexp)
              ;; Wrap
              ("M-("   . sp-wrap-round)
              ("M-["   . sp-wrap-square)
              ("M-{"   . sp-wrap-curly)
              ;; Kill/Copy
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
	      ;; transpose lines
	      ("M-<up>" . sp-transpose-sexp))
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'clojure-mode "(" ")" :when '(sp-in-code-p)))
  (setq sp-autodelete-pair t
        sp-autodelete-wrap t
        sp-autoskip-closing-pair 'always
        sp-cancel-autoskip-on-backward-movement nil
        sp-navigate-consider-symbols t)
  (sp-local-pair '(clojure-mode clojure-ts-mode) "`" nil :actions nil)
  (sp-pair "'" nil :actions nil))

(use-package aggressive-indent
  :hook (clojure-ts-mode . aggressive-indent-mode))

(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :custom (project-list-file "~/.emacs.d/projects"))

(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-d") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "M-P") 'flymake-show-project-diagnostics))

;; ----------------------- PYTHON SETUP (with IPython) ----------------------- ;;
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook
  ((python-ts-mode . (lambda ()
                       ;; Disable python-flymake, use your LSP for linting
                       (remove-hook 'flymake-diagnostic-functions 'python-flymake t)))
   ;; Enable Corfu in IPython REPL, disable auto-popup for manual control
   (inferior-python-mode . (lambda ()
                             (corfu-mode 1)
                             (setq-local corfu-auto nil))))
  :custom
  ;; Use IPython as the REPL
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i")
  ;; Keep native completion enabled
  (python-shell-completion-native-enable t))

(use-package python-black
  :demand t
  :after python
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

(use-package pyvenv
  :ensure t
  :hook (python-ts-mode . my/pyvenv-auto-activate)
  :config
  (require 'seq)
  (defun my/pyvenv-auto-activate ()
    "Activate ./.venv or ./venv if present at project root."
    (when-let* ((root (ignore-errors (project-root (project-current))))
                (cand (mapcar (lambda (d) (expand-file-name d root)) '(".venv" "venv")))
                (venv (seq-find #'file-directory-p cand)))
      (pyvenv-activate venv))))

;; ----------------------- EGLOT (LSP) ----------------------- ;;
(use-package eglot
  :ensure nil
  :defer t
  :bind (("M-TAB" . completion-at-point)
         ("M-g i" . imenu)
         ("C-h ." . display-local-help)
         ("M-."   . xref-find-definitions)
         ("M-,"   . xref-go-back)
         :map eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format))
  ;; Note: Removed clojure-ts-mode - CIDER handles that
  :hook (((python-ts-mode c++-ts-mode c-mode
              typescript-ts-mode tsx-ts-mode css-mode html-mode haskell-mode) . eglot-ensure))
  :config
  (setq eglot-server-programs
        (append '(((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))
                  (html-ts-mode . ("vscode-html-language-server" "--stdio"))
                  (css-ts-mode  . ("vscode-css-language-server" "--stdio"))
                  (haskell-mode . ("haskell-language-server-wrapper" "--stdio"))
                  (python-ts-mode . ("basedpyright-langserver" "--stdio")))
                eglot-server-programs))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil))

;; ----------------------- CLOJURE SETUP ----------------------- ;;
(use-package clojure-ts-mode
  :mode (("\\.clj\\'"  . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode))
  :hook ((clojure-ts-mode . cider-mode)
         (clojure-ts-mode . eldoc-mode)
         (clojure-ts-mode . my/clojure-local-setup))
  :init
  (defun my/clojure-local-setup ()
    (electric-pair-local-mode -1)))

;; Configure eldoc to show both CIDER and other documentation
(setq eldoc-documentation-strategy 'eldoc-documentation-compose)
(setq cider-eldoc-display-context-dependent-info nil)

(defun cider-integrant-reset ()
  "Run integrant.repl/reset in the current REPL."
  (interactive)
  (cider-interactive-eval "(integrant.repl/reset)"))

(use-package cider
  :after clojure-ts-mode
  :init
  (setq cider-repl-display-help-banner nil
        cider-repl-use-clojure-font-lock t
        cider-save-file-on-load t
        cider-use-completion-at-point t
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-clojure-cli-aliases ":dev"
        cider-repl-display-result t
        cider-eldoc-display-for-symbol-at-point nil
        cider-eldoc-display-context-dependent-info nil)
  :hook ((cider-repl-mode . eldoc-mode))
  :config
  (with-eval-after-load 'clojure-ts-mode
    (define-key clojure-ts-mode-map (kbd "C-c r") #'cider-integrant-reset)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

;; ----------------------- ORG MODE ----------------------- ;;
(setq inhibit-splash-screen t)
(transient-mark-mode 1)

(require 'org)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "refile.org"))
(global-set-key (kbd "C-c c") 'org-capture)

(use-package olivetti
  :custom (olivetti-body-width 90)
  :hook (org-mode . olivetti-mode))

(setq org-hide-emphasis-markers t)    ; hide *bold* markers, show just bold
(setq org-pretty-entities t)          ; \alpha → α, \to → →
(setq org-ellipsis " ▾")             ; nicer fold indicator

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))
  (org-modern-table t)
  (org-modern-block-fringe t))

(setq org-capture-templates
      '(("w" "Workout Log" entry (file+datetree "~/org/workouts.org")
         "* %^{Workout Type|Push|Pull|Legs A (Quads)|Legs B (Hams)}\n:PROPERTIES:\n:TIME: %U\n:END:\n%?" 
         :empty-lines 1)))

(add-to-list 'org-capture-templates
             '("n" "Daily Nutrition" table-line 
               (file+olp "~/org/nutrition.org" "Logs" "Current Week")
               "| %U | %^{Calories} | %^{Protein} | %^{Carbs} | %^{Fats} | %^{Notes} |"
               :immediate-finish t))

;; ----------------------- GIT (MAGIT) ----------------------- ;;
(use-package magit
  :bind (("C-x g" . magit-status)))

;; ----------------------- TERMINAL (VTERM) ----------------------- ;;
(use-package vterm
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(defun vterm-new-window ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (let ((vterm-buffer-name-string "%s"))
    (vterm)))

(global-set-key (kbd "C-c t w") 'vterm-new-window)

;; ----------------------- FILE BROWSER (TREEMACS) ----------------------- ;;
(use-package treemacs
  :config
  :bind
  (("C-x t t" . treemacs)
   ("C-x t a" . treemacs-select-window)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
