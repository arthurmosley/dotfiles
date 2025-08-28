;; -*- lexical-binding: t -*-

(setq custom-file "~/.emacs-custom")
(if (file-exists-p custom-file)
    (load custom-file))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; ensure that Emacs gets my shell's PATH:
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("PATH" "JAVA_HOME"))
  :config
  (exec-path-from-shell-initialize))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
 
(delete-selection-mode t)

(global-set-key (kbd "C-x m") 'eshell)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Minimize the UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; General QOL UI Mods
(global-display-line-numbers-mode t)
(setq inhibit-startup-message t
      require-final-newline t
      visible-bell t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Remembers where my cursor was on a file.
(save-place-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Highlights matching parentheses when cursor is on them.
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(savehist-mode 1)

;; Scrolling done right
(setopt scroll-error-top-bottom t)
(setopt focus-follows-mouse t)
(setopt recenter-positions '(top bottom middle))

(use-package which-key
  :ensure nil
  :config (which-key-mode))

;; pick the specific window to go to.
(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package winner
  :config
  (winner-mode 1))

;; Send backup files to a specific directory. Going to ignore this in the commits.
(unless backup-directory-alist
  (setq backup-directory-alist
    `(("." . ,(concat user-emacs-directory "backups")))))

;; ensure that universally, meta is on command and super is on alt across any OS.
(when (eq system-type 'darwin)
  (setq x-meta-keysym 'super
        x-super-keysym 'meta))

;; Handles the core packages for buffer/file/command running QOL

(use-package origami
  :hook (clojure-ts-mode . origami-mode))

;; Enable Vertico - vertical search options 
(use-package vertico
  :init
  (vertico-mode))

;; Enable Orderless - completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - annotations for completion candidates.
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Corfu - better in buffer completion.
(use-package corfu
  :init
  (global-corfu-mode))

(use-package consult
  ;; Optional: set a few convenient keybindings up front
  :bind (("C-s" . consult-line)                ;; better search
         ("M-y" . consult-yank-pop)            ;; browse kill-ring
         ("C-x b" . consult-buffer))
  :init
  ;; make sure project.el knows how to find files before consult hooks in
  (setq consult-project-function #'consult--default-project-function))

(use-package consult-project-extra)
(define-key project-prefix-map (kbd "f") #'consult-project-extra-find)

(use-package embark
  :bind (("C-." . embark-act)         ;; main action menu
         ("C-;" . embark-dwim)        ;; smart action
         ("C-h B" . embark-bindings)) ;; what’s bound here?
  :init
  ;; show more context in the minibuffer when using embark
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Ensure C-x p f *always* runs consult-project-file
(with-eval-after-load 'consult
  (keymap-set project-prefix-map "f" #'consult-project-file))

;; dired file manager
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

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Persist history over Emacs restarts. 
(use-package savehist
  :init
  (savehist-mode))

(use-package org
  :bind (("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda))
  :hook ((org-mode . visual-line-mode)
         (org-mode . visual-fill-column-mode)
         (org-mode . turn-off-auto-fill))
  :config
  ;; need to configure this at some point, useful for creating structure templates.
  (require 'org-tempo)
  :custom
  (setopt org-log-done t)
  (setopt org-default-notes-file "~/org/notes.org")
  (setopt org-archive-location "~/org/archives/%s::")
  (setopt org-todo-keywords '((sequence "ONGO(o)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)")))
  (setopt org-todo-keyword-faces
    '(("ONGO" . (:inverse-video t))
      ("NEXT" . (:weight bold :background "#eeeeee"))
      ("WAIT" . (:box t))
      ("SKIP" . (:strike-through t)))))

;; Ensure the directory exists
(make-directory "~/org/journal" t)

(with-eval-after-load 'org
  (defun my/org-monthly-journal-file ()
    (expand-file-name (format-time-string "%Y-%m.org") "~/org/journal/"))

  (setq org-capture-templates
        '(("J" "Journal (monthly files + datetree)" entry
                   (file+olp+datetree my/org-monthly-journal-file)
                   "* %U %?\n"))))

(use-package visual-fill-column
  :defer t
  :custom
  (visual-fill-column-width 88))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "▶" "▷")))

(setq org-log-done t)

(use-package project
  :ensure nil
  :bind ("C-c p" . project-prefix-map))

;; Show line and column on the modeline.
(setopt line-number-mode t)
(setopt column-number-mode t)

;;; GENERAL PROGRAMMING CONFIG used in every language.

(setq treesit-language-source-alist
      '((clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (edn     "https://github.com/tonsky/tree-sitter-edn")))

;; Rainbow delimeters
(use-package rainbow-delimiters
  :hook ((clojure-ts-mode python-ts-mode) . rainbow-delimiters-mode))

;; Paredit initialization
(use-package paredit
  :config
  (define-key paredit-mode-map (kbd "C-M-w") 'sp-copy-sexp))

;; Clojure initialization
(setopt inf-clojure-generic-cmd "clojure")

;; Use LSP
(use-package lsp-mode
  :commands lsp
  :hook ((clojure-ts-mode . lsp)
         (emacs-lisp-mode . lsp))
  :config
  (setopt lsp-warn-no-matched-clients nil)
  (setopt lsp-prefer-flymake nil)
  (setq lsp-completion-provider :capf))

(use-package clojure-ts-mode
  :config
  (require 'flycheck-clj-kondo)
  (setopt clojure-align-forms-automatically t)
  (add-hook 'clojure-ts-mode-hook 'origami-mode)
  (add-hook 'clojure-ts-mode-hook 'paredit-mode)
  ;; (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-ts-mode-hook 'aggressive-indent-mode))

;; (use-package clj-refactor
;;   :config
;;   ;; (setopt clojure-thread-all-but-last t)
;;   (define-key clj-refactor-map "\C-ctf" #'clojure-thread-first-all)
;;   (define-key clj-refactor-map "\C-ctl" #'clojure-thread-last-all)
;;   (define-key clj-refactor-map "\C-cu" #'clojure-unwind)
;;   (define-key clj-refactor-map "\C-cU" #'clojure-unwind-all))

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (setopt cider-use-fringe-indicators nil)
  (setopt cider-repl-pop-to-buffer-on-connect nil)
  (setopt nrepl-hide-special-buffers t))

;; Emacs Lisp initialization
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode 'append)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'origami-mode)

;; PYTHON CONFIG

(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)))

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(add-hook 'python-ts-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package blacken
  :hook (python-ts-mode . blacken-mode))

;; Magit Configuration
(use-package magit)

;;; init.el ends here.

