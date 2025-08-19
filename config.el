;;; config.el -*- lexical-binding: t; -*-

;; --------------------------- Joker look ---------------------------
;; Base theme + neon-green/purple accents
(setq doom-theme 'doom-dracula
      display-line-numbers-type 'relative)

;; Fonts (install a Nerd Font for icons; "Hack Nerd Font" is great)
(setq doom-font (font-spec :family "Hack Nerd Font" :size 17)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 17))

;; Neon tweaks
(custom-set-faces!
  '(cursor :background "#39ff14")
  '(mode-line :background "#2a1535" :foreground "#39ff14" :weight bold)
  '(mode-line-inactive :background "#1a0f22" :foreground "#7a7a7a")
  '(vertico-current :background "#2a1535" :foreground "#ffffff" :weight bold)
  '(font-lock-string-face :foreground "#39ff14") ; strings pop neon green
  '(font-lock-constant-face :foreground "#b48ead") ; purple constants
  )

;; --- Org-roam core ---
(use-package! org-roam
  :after org
  :init
  (setq org-roam-v2-ack t
        ;; Point Org-roam into your Obsidian vault (subfolder "roam")
        org-roam-directory (file-truename "~/obsidian-sync/roam")
        ;; Keep backlinks up to date automatically
        org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  :config
  (org-roam-db-autosync-mode)

  ;; Helpful templates (adjust titles & fields to taste)
  (setq org-roam-capture-templates
        '(("n" "Note" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+created: %U\n")
           :unnarrowed t)
          ("p" "Project" plain
           "* Overview\n%?\n* Tasks\n- [ ] \n* Notes\n"
           :if-new (file+head "projects/${slug}.org"
                              "#+title: ${title}\n#+category: Project\n#+created: %U\n")
           :unnarrowed t)
          ;; Example for your divisions
          ("w" "Work item (SunCo)" plain
           "* TODO %^{Task}\n:PROPERTIES:\n:division: %^{Division|Turf|Commercial Maintenance|Residential Maintenance|Residential Sprinklers|Commercial Sprinklers}\n:END:\n%?\n"
           :if-new (file+head "work/${slug}.org"
                              "#+title: ${title}\n#+category: Work\n#+created: %U\n")
           :unnarrowed t)))

  ;; Doom-style leader keys for roam
  (map! :leader
        :desc "Roam: Find node"    "n r f"  #'org-roam-node-find
        :desc "Roam: Insert node"  "n r i"  #'org-roam-node-insert
        :desc "Roam: Backlinks"    "n r b"  #'org-roam-buffer-toggle
        :desc "Roam: Graph"        "n r g"  #'org-roam-graph))

;; --- Dailies (Zettelkasten-style) ---
(use-package! org-roam-dailies
  :after org-roam
  :init
  ;; Dailies live alongside your notes inside the vault
  (setq org-roam-dailies-directory "daily/")
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "Default" entry
           "* %<%H:%M> %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+created: %U\n"))))
  (map! :leader
        :desc "Daily: Today"   "n r d t" #'org-roam-dailies-capture-today
        :desc "Daily: Find"    "n r d f" #'org-roam-dailies-goto-date
        :desc "Daily: Yesterday" "n r d y" #'org-roam-dailies-capture-yesterday
        :desc "Daily: Tomorrow"  "n r d m" #'org-roam-dailies-capture-tomorrow))

;; --- Optional: Org-roam UI (browser graph) ---
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))  ;; set to t if you want it to auto-open



;; Which-key fast + readable
(setq which-key-idle-delay 0.25)
(after! which-key
  (setq which-key-max-description-length 50))

;; --------------------------- Terminal & File Tree -----------------

(setq treemacs-position 'left)

(defun my/open-treemacs-on-startup ()
  (require 'treemacs))

(add-hook 'window-setup-hook  #'treemacs 'append)

(after! vterm
  (setq vterm-shell (or (getenv "SHELL") "/bin/bash")))
(defun +term/here ()
  "Open vterm rooted at the project (or current) directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (vterm)))
(map! :leader :desc "Terminal here (vterm)" "t v" #'+term/here)

;; --------------------------- Org / Jupyter ------------------------
(setq org-directory "~/obsidian-sync/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-directory
                             (expand-file-name "Agenda" org-directory)
                             (expand-file-name "Journal" org-directory)))

(after! org
  (setq org-capture-templates
        '(("n" "Quick Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n%U\n%i\n%a")
          ("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\nSCHEDULED: %t\n%U\n")
          ("j" "Journal (datetree)" entry
           (file+olp+datetree (expand-file-name "Journal/journal.org" org-directory))
           "* %U - %?\n%i\n")))
  ;; Enable Jupyter blocks (requires the `jupyter` package)
  (add-to-list 'org-babel-load-languages '(jupyter . t)))

;; --------------------------- Python IDE ---------------------------
;; Prefer IPython if present
(let ((ip (executable-find "ipython")))
  (when ip
    (setq python-shell-interpreter ip
          python-shell-interpreter-args "-i --simple-prompt")))

;; LSP behavior (Pyright by default if installed)
(after! lsp-mode
  (setq lsp-idle-delay 0.25
        lsp-log-io nil
        lsp-file-watch-threshold 10000))

;; Start Ruff LSP too, if available (pip install ruff-lsp)
(add-hook 'python-mode-hook
          (defun +python/start-ruff-lsp ()
            (when (executable-find "ruff-lsp")
              (require 'lsp-mode)
              (lsp-deferred))))

;; Format with Black on save
(use-package! blacken
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 88))

;; Auto-activate project venv (.venv) or Poetry env
(defun +python/auto-activate-venv ()
  "Activate .venv or Poetry env automatically."
  (when-let* ((root (or (projectile-project-root) default-directory)))
    (cond
     ((file-directory-p (expand-file-name ".venv" root))
      (pyvenv-activate (expand-file-name ".venv" root)))
     ((executable-find "poetry")
      (let ((path (string-trim
                   (shell-command-to-string "poetry env info --path 2>/dev/null"))))
        (when (and (string-match-p "/" path) (file-directory-p path))
          (pyvenv-activate path)))))))
(add-hook 'python-mode-hook #'+python/auto-activate-venv)

;; Quick venv creator
(defun +python/project-new-venv ()
  "Create .venv with `python -m venv .venv` and activate."
  (interactive)
  (let ((root (or (projectile-project-root) default-directory)))
    (unless root (user-error "Not in a project"))
    (let ((default-directory root))
      (shell-command "python -m venv .venv")
      (pyvenv-activate (expand-file-name ".venv" root))
      (message "Activated %s" (expand-file-name ".venv" root)))))

;; Debugger (DAP)
(after! dap-mode
  (require 'dap-python)
  (setq dap-python-executable (or (executable-find "python") "python")
        dap-auto-configure-features '(sessions locals breakpoints expressions)))
(with-eval-after-load 'dap-python
  (dap-register-debug-template
   "Python :: Run file"
   (list :type "python" :request "launch" :name "Python :: Run file"
         :program "${file}" :cwd nil :console "integratedTerminal"))
  (dap-register-debug-template
   "Python :: Module (pytest)"
   (list :type "python" :request "launch" :name "Python :: Module (pytest)"
         :module "pytest" :args "" :cwd nil :console "integratedTerminal")))

;; Tests & Coverage
(use-package! python-pytest
  :commands (python-pytest-dispatch python-pytest-file python-pytest-function python-pytest-repeat))
(use-package! coverage
  :commands (coverage-mode)
  :init
  (defun +python/coverage-toggle () (interactive) (coverage-mode 'toggle)))

;; Docstrings
(use-package! python-docstring
  :hook (python-mode . python-docstring-mode)
  :config (setq python-docstring-style 'google))
(use-package! sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  :config (map! :localleader :map python-mode-map "d" #'sphinx-doc))

;; --------------------------- Keys you’ll actually use -------------
;; Files / buffers / windows / projects (Doom defaults are on, these add a few)
(map! :leader
      ;; files
      :desc "Find file"      "f f" #'find-file
      :desc "Save buffer"    "f s" #'save-buffer
      :desc "Recent files"   "f r" #'recentf-open-files
      ;; buffers
      :desc "Switch buffer"  "b b" #'switch-to-buffer
      :desc "Kill buffer"    "b k" #'kill-this-buffer
      ;; windows
      :desc "Split right"    "w /" #'split-window-right
      :desc "Split below"    "w -" #'split-window-below
      :desc "Delete window"  "w d" #'delete-window
      ;; projects
      :desc "Switch project" "p p" #'project-switch-project
      :desc "Project files"  "p f" #'project-find-file)

;; Python submap under SPC p y (so it’s memorable)
(map! :leader
      (:prefix ("p y" . "python")
       :desc "REPL"                 "r" #'python-shell-switch-to-shell
       :desc "Send buffer → REPL"   "b" #'python-shell-send-buffer
       :desc "Send region → REPL"   "e" #'python-shell-send-region
       :desc "Activate venv"        "v" #'+python/auto-activate-venv
       :desc "New .venv here"       "n" #'+python/project-new-venv
       :desc "PyTest (dispatch)"    "x" #'python-pytest-dispatch
       :desc "Coverage toggle"      "c" #'+python/coverage-toggle
       :desc "Start debug (DAP)"    "d" #'dap-debug))

