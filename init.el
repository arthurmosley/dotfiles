;; *-* lexical-binding: t -*-

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

(setenv "PYTHON_BASIC_REPL" "1")
(add-to-list 'process-environment "PYTHON_BASIC_REPL=1")
(add-to-list 'exec-path "/home/linuxbrew/.linuxbrew/bin")
(add-to-list 'exec-path "/usr/local/texlive/2025/bin/")

;; early in init.el
(use-package emacs
  :ensure nil
  :custom
  ;; not sure if i want this yet
  (tab-always-indent t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (treesit-language-source-alist
   '((clojure "https://github.com/sogaiu/tree-sitter-clojure")
     (python "https://github.com/tree-sitter/py-tree-sitter")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")
        (json    "https://github.com/tree-sitter/tree-sitter-json")
        (yaml    "https://github.com/ikatyang/tree-sitter-yaml")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))))

(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(tsx-mode . tsx-ts-mode))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))


(setopt sentence-end-double-space nil)

(setq use-package-always-ensure t)

;; Misc minimizing emacs
(global-unset-key (kbd "C-z"))

;; Who I am
(setopt user-full-name "Arthur Mosley")
(setopt user-mail-address "arthurcharlesmosley@gmail.com")

;; Let's get a backtrace when errors are
(setopt debug-on-error t)

;; Display byte-compiler warnings on error
(setopt byte-compile-debug t)

;; Stop polluting the directory with auto-saved files and backup
(setopt auto-save-default nil)
(setopt make-backup-files nil)

;; Well, it's more so that you know this option
(setopt kill-whole-line t)
(setopt kill-read-only-ok t)
(setopt require-final-newline 'visit)

;; Scrolling done right
(setopt scroll-error-top-bottom t)
(setopt focus-follows-mouse t)
(setopt recenter-positions '(top bottom middle))

;; Always use "y" for "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Enabling and disabling some modes
;; Less is more - see https://bzg.fr/en/emacs-strip-tease/
(auto-insert-mode 1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(tool-bar-mode -1)

(delete-selection-mode 1)

;; better scrolling
(use-package good-scroll
  :config
  (good-scroll-mode 1))

;; Default Frame
(setopt default-frame-alist
	'((menu-bar-lines . 0)
	  (tool-bar-lines . 0)))

;; Don't display initial messages
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

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq -ibuffer-display-summary nil)
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


;; ----------------------- QOL Packages  ----------------------- ;;

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; pick the specific window to go to.
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Remember the packages.
(use-package winner
  :config
  (winner-mode 1))

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-operandi-tinted)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Show line and column on the modeline.
(setopt line-number-mode t)
(setopt column-number-mode t)

;; ----------------------- Completion  Packages  ----------------------- ;;
;; come back to this for better completion.
(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  ;; Enable auto completion and configure quitting
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

(use-package consult
  ;; Optional: set a few convenient keybindings up front
  :bind (("C-s" . consult-line)                ;; better search
         ("M-y" . consult-yank-pop)            ;; browse kill-ring
         ("C-x b" . consult-buffer)))

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

;; Make xref results nicer
(with-eval-after-load 'consult
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :init
  (marginalia-mode)                       ; enable everywhere
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)) ; toggle annotations
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)))

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

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;; ensure that universally, meta is on command and super is on alt across any OS.
(when (eq system-type 'darwin)
  (setq x-meta-keysym 'super
        x-super-keysym 'meta))

;; PROGRAMMING SETUP ;;
(use-package dash)

(use-package smartparens
  :init (require 'smartparens-config)
  :hook ((clojure-ts-mode . smartparens-strict-mode)
	 (cider-repl-mode . smartparens-strict-mode)
	 (emacs-lisp-mode . smartparens-strict-mode)
         (text-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
	 (python-mode . smartparens-mode))
  :config
  (with-eval-after-load 'smartparens
  (sp-local-pair 'clojure-mode "(" ")" :when '(sp-in-code-p)))
  ;; Movement
  (define-key smartparens-mode-map (kbd "C-M-f") #'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") #'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") #'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") #'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") #'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "M-F") #'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") #'sp-backward-symbol)
  ;; Slurp/barf
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-(") #'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-{") #'sp-backward-barf-sexp)
  ;; Wrap
  (define-key smartparens-mode-map (kbd "M-(") #'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-[") #'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") #'sp-wrap-curly)
  ;; Kill
  (define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") #'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "C-k")   #'sp-kill-hybrid-sexp)
  (setq sp-autodelete-pair t
        sp-autodelete-wrap t
        sp-autoskip-closing-pair 'always
        sp-cancel-autoskip-on-backward-movement nil
        sp-navigate-consider-symbols t)
  (sp-pair "'" nil :actions nil))

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

(add-to-list 'major-mode-remap-alist '((python-mode . python-ts-mode)
				       (clojure-mode . clojure-ts-mode)
				       (c++-mode . c++-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.h$ . c++-mode"))
(setq c-default-style "stroustrup"
      c-basic-indent 4
      c-basic-offset 4)
(c-set-offset 'innamespace 0)

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . my/python-format-on-save))
  :config
  (defun my/python-format-on-save ()
    (add-hook 'before-save-hook #'python-black-buffer nil t)))

(use-package python-black
  :demand t
  :after python
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

(use-package pyvenv
  :hook (python-ts-mode . my/pyvenv-auto-activate)
  :config
  (require 'seq)
  (defun my/pyvenv-auto-activate ()
    "Activate ./.venv or ./venv if present at project root."
    (when-let* ((root (ignore-errors (project-root (project-current))))
                (cand (mapcar (lambda (d) (expand-file-name d root)) '(".venv" "venv")))
                (venv (seq-find #'file-directory-p cand)))
      (pyvenv-activate venv))))

(use-package eglot
  :ensure nil
  :defer t
  :bind (("M-TAB" . completion-at-point)
           ("M-g i" . imenu)
           ("C-h ." . display-local-help)
           ("M-." . xref-find-definitions)
           ("M-," . xref-go-back)
           :map eglot-mode-map
           ("C-c c a" . eglot-code-actions)
           ("C-c c o" . eglot-code-actions-organize-imports)
           ("C-c c r" . eglot-rename)
           ("C-c c f" . eglot-format))
  :hook ((clojure-ts-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode        . eglot-ensure)
         (css-ts-mode        . eglot-ensure)
         (html-ts-mode       . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((html-ts-mode) . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((css-ts-mode)  . ("vscode-css-language-server" "--stdio"))))

(use-package apheleia
  :config
  (apheleia-global-mode 1))


(use-package clojure-ts-mode
  :mode (("\\.clj\\'"  . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode))
  :hook ((clojure-ts-mode . smartparens-strict-mode)
         (clojure-ts-mode . cider-mode)
         (clojure-ts-mode . eldoc-mode)
         (clojure-ts-mode . my/clojure-local-setup))
  :init
  (defun my/clojure-local-setup ()
    (electric-pair-local-mode -1)))

(setq eldoc-documentation-strategy 'eldoc-documentation-compose)
(setq cider-eldoc-display-context-dependent-info nil)

(defun my/clj-format-on-save ()
  (when (eglot-managed-p) (eglot-format-buffer)))

(add-hook 'clojure-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'my/clj-format-on-save nil t)))

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
        cider-repl-display-result t)
  :hook ((cider-repl-mode . eldoc-mode))
  :config
  
  (with-eval-after-load 'clojure-ts-mode
    (define-key clojure-ts-mode-map (kbd "C-c r") #'cider-integrant-reset)))

;; BASIC ORG MODE SETUP JUST FOR NOTE TAKING THAT IS ALL

(setq inhibit-splash-screen t)

(transient-mark-mode 1)

(require 'org)

;; magit setup

(use-package magit
  :bind (("C-x g" . magit-status)))

;; better terminal (vterm) setup
(use-package vterm
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(defun vterm-new-window ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (let ((vterm-buffer-name-string "%s"))
    (vterm)))

(global-set-key (kbd "C-c t w") 'vterm-new-window)

(use-package treemacs
  :bind
  (("C-x t t" . treemacs)
   ("C-x t a" . treemacs-select-window)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))


