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
    (text-scale-set -3))
  :hook ((compilation-mode . mg/my-compilation-mode-hook))
  :config
  (ignore-errors
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook
              (lambda ()
                (when (eq major-mode 'compilation-mode)
                  (ansi-color-apply-on-region compilation-filter-start (point-max)))))
    (add-hook 'next-error-hook 'recenter)))

(use-package ripgrep)

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
  :config
  (company-statistics-mode 1)
  (global-company-mode 1)
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t))

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

;;
;;
;;
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

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
