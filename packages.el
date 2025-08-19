;;; packages.el -*- lexical-binding: t; -*-

;; Python goodies
(package! blacken)           ; Black formatter
(package! python-pytest)     ; PyTest UI
(package! python-docstring)  ; Docstring helpers
(package! sphinx-doc)        ; Insert Google/Numpy docstrings
(package! coverage)          ; Coverage overlays
(package! jupyter)           ; Run notebooks / org-babel-jupyter


;; Add Org-Roam
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! org-roam-ui 
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

;; Docs quick-lookup (C-h D)
(package! devdocs)


