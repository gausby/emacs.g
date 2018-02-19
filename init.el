;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
(progn ; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq package-enable-at-startup nil)
  ;;
  (setq load-prefer-newer t))

(progn ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

;; set the os path
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package mg-settings
  :init
  ;; startup screen
  (setq inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message nil
        initial-buffer-choice t
        initial-scratch-message
        ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n")
  ;; defaults
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;; set backup behaviour
  (setq create-lockfiles nil
        auto-save-default nil
        backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups")))))
  ;; scroll one line at a time
  (setq scroll-step 1
        ;; ...and don't go bananas when scrolling
        scroll-conservatively 10000)
  ;; chrome
  (set-default 'indicate-buffer-boundaries '((top . nil) (bottom . right)))
  (provide 'mg-settings)
  :bind (:map ctl-x-map
              ;; Make it difficult to quit emacs
              ("C-c" . delete-frame)
              ("C-S-c" . save-buffers-kill-terminal))
  :config
  ;; set text input and behaviour ----------------------------------------
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  ;; overwrite regions on keypress
  (delete-selection-mode t)
  ;; declutter interface
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when window-system
    (tooltip-mode -1)
    (blink-cursor-mode -1))
  ;; highlight current line
  (global-hl-line-mode t)
  ;; Remove warnings when using certain commands
  (put 'narrow-to-region 'disabled nil)
  ;; remove whitespace when buffers are saved
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package subr-x
  :config
  (put 'if-let   'byte-obsolete-info nil)
  (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :preface
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :if (file-exists-p custom-file)
  :config
  (load custom-file))

(use-package server
  :config (or (server-running-p) (server-mode)))

;; advice build-in Emacs functions
(use-package advice
  :config
  (defadvice kill-buffer (around kill-buffer-around-advice activate)
    "Bury *scratch* buffers instead of killing them."
    (let ((buffer-to-kill (ad-get-arg 0)))
      (if (equal buffer-to-kill "*scratch*")
          (bury-buffer)
        ad-do-it))))

(use-package bind-key
  ;; a place to put all the functions that I'll keybind globally
  :init
  (setq bind-key-describe-special-forms t)
  ;; functions to bind
  (defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
  ;;
  :bind
  ("M-q" . endless/fill-or-unfill))

(use-package autorevert
  :config
  ;; auto load files when they change on disk
  (global-auto-revert-mode t))

;;; Long tail
(use-package ns
  :if (eq window-system 'ns)
  :config
  ;; mac keyboard
  (setq mac-option-modifier nil
      ns-function-modifier 'super
      mac-right-command-modifier 'hyper
      mac-right-option-modifier 'alt
      mac-command-modifier 'meta)
  ;; use srgb colorspace and disable the native fullscreen in macOS,
  ;; which has an annoying transition when running in its native form
  (setq ns-use-srgb-colorspace t
        ns-use-native-fullscreen nil)
  (add-hook 'after-init-hook 'toggle-frame-fullscreen))

(use-package dash
  :config (dash-enable-font-lock))

(progn ; `isearch'
  (setq isearch-allow-scroll t))

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(progn ; personalize
  (let* ((files (list "config/fundamental-mode"
                      "config/text-mode"
                      "config/prog-mode"
                      "config/communication"
                      "config/org-mode"
                      user-real-login-name)) ; load USER-NAME.el
         (default-directory user-emacs-directory))
    (dolist (f files)
      (let ((file (expand-file-name (concat f ".el"))))
        (if (file-exists-p file)
          (progn (load file)
                 (message "Done loading config file: %s" file))
          (message "Please create file: %s" file))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
