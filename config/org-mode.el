(use-package org
  :preface
  (defun mg/my-org-mode-hook ()
      (org-bullets-mode 1)
      (visual-line-mode 1)
      (org-indent-mode 1)
      (set-fill-column 90)
      ;; Keybindings
      (local-set-key (kbd "C-c w") 'mg/org-copy-node-id))
  (defun mg/open-notes-file ()
    "Open an org file in the root of my notes directory"
    (interactive)
    (let* ((default-directory "~/Notes/")
           (org-file-glob (expand-file-name "*.org"))
           (files (mapcar 'file-name-nondirectory
                          (file-expand-wildcards org-file-glob)))
           (file (completing-read "Switch to org file: " files)))
      (find-file (expand-file-name file))))
    ;; Helpers -----------------------------------------------------------
  (defun mg/org-decorate-nodes-with-ids ()
    "Add ID properties to nodes in the current file which
does not already have one."
    (interactive)
    (org-map-entries 'org-id-get-create))
  ;; copy the id of a node to the kill-ring (generate id if nonexistent)
  (defun mg/org-copy-node-id ()
    (interactive)
    (let ((temporary-id (funcall 'org-id-get-create)))
      (kill-new temporary-id)
      (message "Copied %s to kill-ring" temporary-id)))
  :hook (org-mode . mg/my-org-mode-hook)
  :bind (:map ctl-x-map
              ("C-n" . org-capture)
              ("a" . org-agenda)
              ("C-a" . mg/open-notes-file))
  :config
  ;; Navigation
  ;; Make C-a and C-e jump to logical positions and make kill-line
  ;; delete logical parts of the line (headline first, then tags, etc)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  ;; Code blocks and code evaluation
  ;; Use the font lock from the given major mode to highlight the code
  ;; block and don not ask for confirmation when evaluating code
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  ;; org-capture, org-contacts, and org-agenda
  ;; (require 'org-contacts)
  (setq org-directory "~/Notes/"
        org-default-notes-file "capture.org"
        org-capture-templates '(("i" "Inbox" entry
                                 (file "capture.org")
                                 "* %?\n %i\n " :empty-lines 1)
                                ("j" "Journal Entry" entry
                                 (file+datetree "journal.org")
                                 "* %U %?" :empty-lines 1)
                                ("n" "Note (for currently clocked task)" item
                                 (clock) "  - %U %?" :empty-lines 1)
                                ("c" "Contact" entry
                                 (file+headline "contacts.org" "Acquaintances")
                                 "* PERSON %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:NICKNAME: %?
:END:"))
        org-contacts-icon-use-gravatar nil)
  (let ((default-directory org-directory)
        (location-format "archive/%Y-%W-archive.org::* From %s"))
    (setq org-agenda-files (list (expand-file-name "capture.org")
                                 (expand-file-name "private.org")
                                 (expand-file-name "work.org")
                                 (expand-file-name "contacts.org"))
          org-refile-targets '(("private.org" :maxlevel . 2)
                               ("work.org" :maxlevel . 2)
                               ("contacts.org" :maxlevel . 1)
                               ("someday-maybe.org" :maxlevel . 1))
          org-contacts-files (list (expand-file-name "contacts.org"))
          org-archive-location (expand-file-name (format-time-string location-format))
          org-id-locations-file (expand-file-name ".org-id-locations")))
  ;; Item state changes and log drawer
  (setq org-log-into-drawer t
        org-log-done 'time
        org-log-reschedule 'note)
  ;; enable ordered tasks
  (setq org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag t
        org-agenda-dim-blocked-tasks t
        org-enforce-todo-dependencies t)
  ;; Advices -----------------------------------------------------------
  ;; Preserve top level headings when archiving to a file
  ;; http://orgmode.org/worg/org-hacks.html#orgheadline59
  (defadvice org-archive-subtree (around my-org-archive-subtree activate)
    (let ((org-archive-location
           (if (save-excursion (org-back-to-heading)
                               (> (org-outline-level) 1))
               (concat (car (split-string org-archive-location "::"))
                       "::* "
                       (car (org-get-outline-path)))
             org-archive-location)))
      ad-do-it)))

(use-package org-agenda
  :config
  (setq org-agenda-start-on-weekday nil))

(use-package org-bullets)

(use-package ob-shell
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(shell . t)))
