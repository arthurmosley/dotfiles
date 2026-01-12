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
  :config
  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  :custom
  ;; not sure if i want this yet
  (tab-always-indent t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 't) ;; Auto install without asking
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(setq use-package-always-ensure t)

;; Misc minimizing emacs
(global-unset-key (kbd "C-z"))

(setq make-backup-files nil)

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

(set-frame-font "Menlo 15" nil t)

;; Well, it's more so that you know this option
(setopt kill-whole-line t)
(setopt kill-read-only-ok t)
(setopt require-final-newline 'visit)

;; Scrolling done right
(setopt scroll-error-top-bottom t)
(setopt focus-follows-mouse t)
(setopt recenter-positions '(top bottom middle))
(scroll-bar-mode -1)

;; Always use "y" for "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Enabling and disabling some modes
;; Less is more - see https://bzg.fr/en/emacs-strip-tease/
(auto-insert-mode 1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-mode 1)
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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Show line and column on the modeline.
(setopt line-number-mode t)
(setopt column-number-mode t)

(use-package all-the-icons :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ; Sets the bar height
  (doom-modeline-bar-width 4)   ; Sets the vertical bar thickness
  (doom-modeline-env-version t) ; Shows Python/Clojure versions
  (doom-modeline-lsp t))        ; Shows Eglot status

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
         ("C-x b" . consult-buffer)
	 :map project-prefix-map
	 ("s" . consult-ripgrep)))

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

;; python environment stuff
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(keymap-global-set "C-c C" #'compile)

(use-package smartparens
  :init (require 'smartparens-config)
  :hook ((prog-mode . smartparens-mode)
	 (prog-mode . show-smartparens-mode)
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
              ("C-k"   . sp-kill-hybrid-sexp))
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'clojure-mode "(" ")" :when '(sp-in-code-p)))
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

(setq c-default-style "stroustrup"
      c-basic-indent 4
      c-basic-offset 4)
(c-set-offset 'innamespace 0)

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook
  ;; Remove the legacy backend. Eglot will handle the linting.
  (python-ts-mode . (lambda ()
                      (remove-hook 'flymake-diagnostic-functions 'python-flymake t))))

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
      (pyvenv-activate venv)))) ;; Fixed: was 'v', changed to 'venv'

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 
            (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

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
  ;; Consolidation of all your programming hooks
  :hook (((clojure-ts-mode python-ts-mode c++-ts-mode c-mode 
			   typescript-ts-mode tsx-ts-mode css-mode html-mode haskell-mode) . eglot-ensure))
  :config
  ;; Refined server list
  (setq eglot-server-programs
        (append '(((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))
                  (html-ts-mode . ("vscode-html-language-server" "--stdio"))
                  (css-ts-mode  . ("vscode-css-language-server" "--stdio"))
                  (haskell-mode . ("haskell-language-server-wrapper" "--stdio"))
                  (python-ts-mode . ("basedpyright-langserver" "--stdio" "pyright-langserver" "pylsp"))
                  ((clojure-mode clojure-ts-mode clojurescript-mode clojurec-mode) . ("clojure-lsp")))
                eglot-server-programs))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  ;; Performance: Silence heavy logging to prevent UI lag
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil))

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
  :config
  :bind
  (("C-x t t" . treemacs)
   ("C-x t a" . treemacs-select-window)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
