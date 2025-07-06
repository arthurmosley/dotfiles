;;; python.el --- Python config -*- lexical-binding: t; -*-

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(add-hook 'python-ts-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package eglot
  :ensure nil
  :hook (python-ts-mode . eglot-ensure))

(use-package blacken
  :hook (python-ts-mode . blacken-mode))

;; MacOS specific config for python REPL
;; Use IPython on macOS for cleaner REPL interaction(when (eq system-type 'darwin)


(provide 'my-python)
;;; python.el ends here
