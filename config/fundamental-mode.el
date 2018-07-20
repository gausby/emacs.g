(use-package simple
  :config
  ;; mode-line
  (setq uniquify-buffer-name-style 'forward)
  (column-number-mode))

;; shackle
(use-package shackle
  :preface
  (defun mg/add-shackle-rule (rule) (add-to-list 'shackle-rules rule))
  :config
  (setq shackle-rules
        '(("*Apropos*" :select t :align below :size 0.5)
          ("*Buffer List*" :select t :align below :size 0.33)
          ("*Help*" :select t :align below :size 0.5)
          ("*compilation*" :align right :size 0.33)))
  (shackle-mode))

(use-package swiper
  :config
  (ivy-mode 1))

(use-package ivy
  :config
  (setq ivy-use-selectable-prompt t
        ;; don't show recent closed items in various buffers
        ivy-use-virtual-buffers nil))

(use-package counsel
  :after ivy
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package avy
  :bind (("M-g SPC" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

;;
;; Resize text in all buffers at once
;;
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;;
;; Crux - A Collection of Ridiculously Useful eXtensions for Emacs
;;
(use-package crux
  :demand t
  :bind (("C-a" . crux-move-beginning-of-line)))


;; compilation
(use-package compile
  :preface
  (defun mg/my-compilation-mode-hook ()
    (setq compilation-scroll-output 'first-error)
    (visual-line-mode 1)
    (text-scale-set -3))
  (require 'ansi-color)
  (defun mg/my-compilation-filter-hook ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook ((compilation-mode . mg/my-compilation-mode-hook)
         (next-error . recenter)
         (compilation-filter . mg/my-compilation-filter-hook)))


(use-package ripgrep
  :after counsel
  :config
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
  (mg/add-shackle-rule '("*ripgrep-search*" :align below :size 0.16)))

(use-package projectile
  :preface
  (defun mg/update-projectile-project-list ()
    "Discover projects in `~/Development/github.com' and
`~/Development/gitlab.com' and add them to the project list used
by the Projectile project switcher"
    (interactive)
    ;; Perform cleanup before adding projects
    (projectile-cleanup-known-projects)
    ;; Find the projects in the structure and add them
    (let* ((default-directory "~/Development")
           (project-site-globs '("github.com/*/*" "gitlab.com/*/*")))
      ;; The project structure is ~/Development/github.com/USER/PROJECT/
      (dolist (project-site-glob project-site-globs)
        (let ((projects-glob (expand-file-name project-site-glob)))
          (dolist (project (file-expand-wildcards projects-glob))
            (projectile-add-known-project project)))))
    ;; Add my Emacs config folder as well ...
    (projectile-add-known-project "~/.emacs.d"))
  ;; Run upon initialization
  :config
  (mg/update-projectile-project-list))

(use-package counsel-projectile
  :after (counsel projectile)
  :demand t
  :commands counsel-projectile-find-file
  :preface
  (defun universal-argument-find-file ()
    "wrap the `find-file'-command, bound to `C-x C-f', with a
check for whether or not the universal argument has been applied,
and how many times.
Zero times: normal behavior (find file); Once: find file in
project; Twice: find/open project"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'find-file))
          ((equal current-prefix-arg (list 4))
           (counsel-projectile-find-file))
          ((equal current-prefix-arg (list 16))
           (projectile-switch-project))
          ))
  (defun universal-argument-switch-to-buffer ()
    "wrap the `switch-to-buffer'-command, bound to `C-x b', with a
check for whether or not the universal argument has been applied,
and how many times.
Zero times: normal behavior (ivy-switch-buffer); Once: switch to
buffer in project/erc; twice to switch between open projects."
    (interactive)
    (cond ((and (equal current-prefix-arg (list 4)) (equal major-mode 'erc-mode))
           (call-interactively 'mg/erc-switch-to-buffer))
          ((and (equal current-prefix-arg (list 4)) (projectile-project-p))
           (call-interactively 'counsel-projectile-switch-to-buffer))
          ((equal current-prefix-arg (list 16))
           (projectile-switch-open-project))
          (t (call-interactively 'switch-to-buffer))
          ))
  (defun universal-argument-kill-buffer ()
    "wrap the `kill-buffer'-command, bound to `C-x k', with a
check for whether or not the universal argument has been applied
or not.
Zero times: normal behavior (kill-buffer);
Once: (projectile-kill-buffers)"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'kill-buffer))
          ((equal current-prefix-arg (list 4))
           (call-interactively 'projectile-kill-buffers))
          ))
  :bind (:map ctl-x-map
              ("C-f" . universal-argument-find-file)
              ("C-b" . universal-argument-switch-to-buffer)
              ("k" . universal-argument-kill-buffer)
              ("p s" . projectile-ripgrep))
  :config
  (setq projectile-completion-system 'ivy)
  ;; add directories and files to the projectile ignore list
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".beam"))

;;
;; Flycheck
;;
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;
;; Completion
;;
(use-package company
  :init
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  :config
  (company-statistics-mode 1)
  (global-company-mode 1))

;;
;; Git related
;;
(use-package magit
  :defer t
  :bind ((:map ctl-x-map
          ("g" . magit-status)
          ("M-g" . magit-dispatch-popup)))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package git-timemachine)

;;
;;
;;
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;;
;; Dired
;;
(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

;;
;; Help systems
;;
(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package man
  :defer t
  :config
  (setq Man-width 80))

;;
;; Eshell - the best shell in existence
;;
(use-package eshell
  :after projectile
  :preface
  (defun universal-argument-eshell ()
    "wrap the `eshell'-command, with a check for whether or not
the universal argument has been applied, and how many times.
Zero times: normal behavior (eshell); Once: open a shell in the
current project root"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'eshell))
          ((equal current-prefix-arg (list 4))
           (projectile-run-eshell))
          ))
  ;; mode hook
  (defun mg/my-eshell-mode-hook ()
    (set (make-local-variable 'global-hl-line-mode) nil))
  :hook (eshell-mode . mg/my-eshell-mode-hook)
  :bind
  ((:map ctl-x-map
         ("C-t" . universal-argument-eshell))))

;;
;; BSD Makefiles
;;
(use-package make-mode
  :bind
  ((:map makefile-mode-map
         ;; I use make to build most of my projects with, so I'll keep
         ;; the keybindings I would normally have for building a
         ;; project in the make-mode
         ("C-c C-c" . projectile-compile-project)
         ("C-c C-t" . projectile-test-project)
         ("M-p" . makefile-previous-dependency)
         ("M-n" . makefile-next-dependency))))

;;
;; docker
;;
(use-package docker
  :if (file-exists-p "~/.docker")
  :preface
  (defun mg/update-docker-machine-env (machine-name)
  "Update the system environment with the information provided by
the `docker-machine env machine-name' command."
  ;; todo, make this function list the machines available on the
  ;; machine (docker-machines-entries) can be used for that: warn if
  ;; no one is present, use the only option if only one is there, and
  ;; use the completing-read function if more than one is present
  (interactive "sMachine name: ")
  (let* ((docker-machine-env-cmd
          (format "docker-machine env %s" machine-name))
         (machine-env-result
          (shell-command-to-string docker-machine-env-cmd)))
    (seq-do
     (lambda (line)
       ;; we are only concerned with lines starting with export
       (if (string-prefix-p "export " line)
           ;; remove "export " and split the line in key/value
           (let* ((foo (string-remove-prefix "export " line))
                  (kvpair (split-string-and-unquote foo "=")))
             ;; update emacs env with docker machine env settings
             (setenv (car kvpair) (cadr kvpair)))))
     (split-string machine-env-result "\n" 'strip-empty)))))

;; for editing `Dockerfile`s
(use-package dockerfile-mode)

;; yaml, for editing docker-compose.yaml files
(use-package yaml-mode
  :bind
  ((:map yaml-mode-map
         ("C-m" . newline-and-indent))))

;; allow tramping into docker containers
(use-package docker-tramp
  :if (file-exists-p "~/.docker")
  :config
  (push (cons
         "docker"
         '((tramp-login-program "docker")
           (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
           (tramp-remote-shell "/bin/sh")
           (tramp-remote-shell-args ("-i") ("-c"))))
        tramp-methods)
  (defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it)))
