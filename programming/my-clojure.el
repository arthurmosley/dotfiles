;; -*- lexical-binding: t -*-
;;; clojure.el

;; clojure major mode
(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.edn\\'"))

(use-package cider
  :hook (clojure-mode . cider-mode))

(use-package eglot
  :ensure nil
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(clojure-mode . ("clojure-lsp"))))

(use-package paredit
  :hook (clojure-mode . paredit-mode))

(provide 'my-clojure)

;; clojure.el ends here
