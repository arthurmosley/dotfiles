;; -*- lexical-binding: t -*-
;; Start of defaults.el
;; Handles the first things done in configuring my emacs. Maybe I should separate concerns more?

;; Setting the theme
(load-theme 'base16-default-dark t)

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

;; Highlights matching parentheses when curson is on them.
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(savehist-mode 1)

(use-package which-key
  :ensure nil
  :config (which-key-mode))

;; pick the specific window to go to.
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Send backup files to a specific directory. Going to ignore this in the commits.
(unless backup-directory-alist
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups")))))

(provide 'defaults)

;;; defaults.el ends here.
