;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; ----- Identity -----
(setq user-full-name "Zach Harrison"
      user-mail-address "partsguy83@gmail.com")

;; ----- Fonts & Theme -----
;; Tip: ensure "Hack Nerd Font" is installed on your system
(setq doom-font (font-spec :family "Hack Nerd Font" :size 17)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 17)
      doom-theme 'doom-dracula
      display-line-numbers-type 'relative)

;; ----- Obsidian / Org basics -----
;; Point Org at your Obsidian vault (adjust this path to your vault)
(setq org-directory "~/obsidian-sync/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-directory
                             (expand-file-name "Agenda" org-directory)
                             (expand-file-name "Journal" org-directory)))

(after! org
  ;; Fast capture for notes, todos, and a datetree journal (works even without org-journal)
  (setq org-capture-templates
        '(("n" "Quick Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n%U\n%i\n%a")
          ("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\nSCHEDULED: %t\n%U\n")
          ("j" "Journal (datetree)" entry
           (file+olp+datetree (expand-file-name "Journal/journal.org" org-directory))
           "* %U - %?\n%i\n"))))

;; Optional: org-journal (nice daily files). Add `(package! org-journal)` to packages.el if you enable this.
(when (featurep! :lang org)
  (use-package! org-journal
    :defer t
    :init
    (setq org-journal-directory (expand-file-name "Journal/" org-directory)
          org-journal-file-type 'daily
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-enable-agenda-integration t
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* ")))


;; ----- Python: formatting, REPL, venv helpers -----
;; Uses Black on save (install 'black' in your venv or system).
;; Add `(package! blacken)` to packages.el.
(use-package! blacken
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 88))

;; Make Doom’s formatter system prefer Black too (if you use :editor (format +onsave))
(after! python
  (setq python-shell-interpreter "python"))

;; Handy Python keybinds (REPL, send buffer/region, pick venv)
(map! :leader
      (:prefix ("p" . "python")
       :desc "Python REPL"          "r" #'python-shell-switch-to-shell
       :desc "Send buffer to REPL"  "b" #'python-shell-send-buffer
       :desc "Send region to REPL"  "e" #'python-shell-send-region
       :desc "Choose venv (pyvenv)" "v" #'pyvenv-activate
       :desc "Deactivate venv"      "V" #'pyvenv-deactivate))

;; ----- Ops under SPC o (safer explicit prefix) -----
(map! :leader
      (:prefix ("o" . "Ops")
       :desc "Org Agenda"     "a" #'org-agenda
       :desc "New Capture"    "c" #'org-capture
       :desc "Calendar"       "C" #'calendar
       :desc "Vault (dired)"  "o" (cmd! (dired org-directory))
       :desc "Open vterm"     "t" #'vterm
       :desc "Toggle tree"    "f" #'treemacs))

;; ----- “My Menu” (simple, discoverable prefix under SPC m) -----
(map! :leader
      (:prefix ("m" . "My Menu")
       :desc "Notes inbox capture"      "n" (cmd! (org-capture nil "n"))
       :desc "Todo capture"             "t" (cmd! (org-capture nil "t"))
       :desc "Journal today (datetree)" "j" (cmd! (org-capture nil "j"))
       :desc "Agenda"                   "a" #'org-agenda
       :desc "Vault in dired"           "o" (cmd! (dired org-directory))
       :desc "Terminal"                 "T" #'vterm
       :desc "File tree"                "F" #'treemacs
       :desc "Python REPL"              "r" #'python-shell-switch-to-shell))

;; --- Python workflow cheat sheet quick access ---
(defun +open-python-cheatsheet ()
  "Open my Doom Python workflow cheat sheet."
  (interactive)
  (find-file "~/obsidian-sync/Doom-Python-Cheatsheet.org"))

(map! :leader
      :desc "Python workflow cheatsheet"
      "h P" #'+open-python-cheatsheet)


;; ----- Keybind Cheat Sheet (lives in ~/.doom.d/KEYBINDINGS.org) -----
(defun +keys/open-cheatsheet ()
  "Open or create a simple keybind cheat sheet in your Doom dir."
  (interactive)
  (let* ((file (expand-file-name "KEYBINDINGS.org" doom-user-dir))
         (exists (file-exists-p file)))
    (unless exists
      (with-temp-file file
        (insert "#+TITLE: Doom Keybinds Cheat Sheet\n\n"
                "* Global\n"
                "- SPC o t :: Open vterm\n"
                "- SPC o f :: Toggle Treemacs file tree\n"
                "- SPC o a :: Org Agenda\n"
                "- SPC o c :: Org Capture\n"
                "- SPC o C :: Calendar\n"
                "- SPC o o :: Open Obsidian vault (dired)\n\n"
                "* My Menu (SPC m)\n"
                "- m n :: Capture Note\n"
                "- m t :: Capture Todo\n"
                "- m j :: Journal (datetree)\n"
                "- m a :: Agenda\n"
                "- m o :: Vault in dired\n"
                "- m T :: Terminal\n"
                "- m F :: File tree\n"
                "- m r :: Python REPL\n\n"
                "* Python (SPC p …)\n"
                "- p r :: Python REPL\n"
                "- p b :: Send buffer to REPL\n"
                "- p e :: Send region to REPL\n"
                "- p v :: Choose venv (pyvenv)\n"
                "- p V :: Deactivate venv\n")))
    (find-file file)))

(map! :leader :desc "Keybind cheat sheet" "h k" #'+keys/open-cheatsheet)

;; ----- Quality of life -----
(setq confirm-kill-emacs nil)   ; live dangerously ;)
(setq which-key-idle-delay 0.25)
(after! which-key
  (setq which-key-max-description-length 40))

