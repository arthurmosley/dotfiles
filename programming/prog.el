;; -*- lexical-binding: t -*-
;;; general programming configuration, used in every language.

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


(provide 'prog)

;;; prog.el ends here.
