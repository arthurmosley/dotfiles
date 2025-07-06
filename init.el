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

;; Modules
(load (expand-file-name "defaults.el" user-emacs-directory))
(load (expand-file-name "enhancements.el" user-emacs-directory))

;; folder way??? -- think about this and doing this for everything.
(add-to-list 'load-path (expand-file-name "programming" user-emacs-directory))`q

;; Show line and column on thpe modeline.
(setopt line-number-mode t)
(setopt column-number-mode t)

;; Loading Modules
(require 'defaults)
(require 'enhancements)
(require 'prog)
(require 'my-clojure)
(require 'my-python)

;;; init.el ends here.

