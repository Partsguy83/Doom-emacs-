
;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion
 (vertico +icons)

 :ui
 doom
 doom-dashboard
 treemacs
 hl-todo
 indent-guides
 modeline
 nav-flash
 (popup +defaults)
 vc-gutter
 vi-tilde-fringe
 workspaces

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format +onsave)       ; run formatters on save
 snippets

 :emacs
 dired
 electric
 ibuffer
 undo
 vc

 :term
 vterm

 :checkers
 (syntax +flymake)

 :tools
 direnv
 (debugger +lsp)
 eval
 lookup
 lsp
 magit
 pdf
 tree-sitter

 :lang
 data
 emacs-lisp
 json
 (org +pretty +babel)   ; Jupyter blocks enabled in config.el
 markdown
 python
 sh
 yaml

 :config
 (default +bindings +smartparens))


