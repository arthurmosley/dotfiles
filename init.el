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
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(delete-selection-mode t)

(global-set-key (kbd "C-x m") 'eshell)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

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
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

(use-package consult
  ;; Optional: set a few convenient keybindings up front
  :bind (("C-s" . consult-line)                ;; better search
         ("M-y" . consult-yank-pop)            ;; browse kill-ring
         ("C-x b" . consult-buffer)            ;; better buffer switch
         ("C-x p f" . consult-project-file))   ;; project files
  :init
  ;; make sure project.el knows how to find files before consult hooks in
  (setq consult-project-function #'consult--default-project-function))

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
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Persist history over Emacs restarts. Vertico sorts by 
(use-package savehist
  :init
  (savehist-mode))

;; Documentation packages
(use-package devdocs
  :init
  :commands (devdocs-install devdocs-remove devdocs-lookup)
  :bind (("C-c d i" . devdocs-install)
         ("C-c d r" . devdocs-remove)
         ("C-c d d" . devdocs-lookup)))

(use-package org
  :bind (("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda))
  :config
  ;; need to configure this at some point, useful for creating structure templates.
  (require 'org-tempo)
  :custom
  (setopt org-log-done t)
  (setopt org-default-notes-file "~/org/notes.org")
  (setopt org-archive-location "~/org/archives/%s::")
  (setopt org-todo-keywords '((sequence "ONGO(o)"
                                        "NEXT(n)"
                                        "TODO(t)"
                                        "WAIT(w)"
                                        "|"
                                        "DONE(d)"
                                        "SKIP(s)")))
  (setopt org-todo-keyword-faces
	'(("ONGO" . (:inverse-video t))
	  ("NEXT" . (:weight bold :background "#eeeeee"))
	  ("WAIT" . (:box t))
	  ("SKIP" . (:strike-through t)))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "▶" "▷")))

(setq org-log-done t)

;; Show line and column on the modeline.
(setopt line-number-mode t)
(setopt column-number-mode t)

;;; GENERAL PROGRAMMING CONFIG used in every language.

;; Rainbow delimeters
(use-package rainbow-delimiters
  :hook ((clojure-mode python-ts-mode) . rainbow-delimiters-mode))

;; Project.el for project management
(use-package project
  :ensure nil
  :bind ("C-c p" . project-prefix-map))

;; Load smartparens and its default config
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)

  ;; Strict mode for Lisps only
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojurescript-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojurec-mode-hook #'smartparens-strict-mode)

  (bind-keys
   :map smartparens-mode-map
   ("C-(" . sp-backward-slurp-sexp)
   ("C-)" . sp-forward-slurp-sexp)
   ("M-(" . sp-backward-barf-sexp)
   ("M-)" . sp-forward-barf-sexp)
   ("M-k" . sp-kill-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-raise-sexp)
   ("M-DEL" . sp-unwrap-sexp)))

(global-set-key (kbd "C-c M-c") 'comment-region)
(global-set-key (kbd "C-c M-u") 'uncomment-region)

;; CLOJURE CONFIG
(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.edn\\'")
  :config
  (setopt clojure-align-forms-automatically t))
 
(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package eglot
  
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(clojure-mode . ("clojure-lsp"))))

;; PYTHON CONFIG

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

