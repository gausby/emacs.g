;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
(progn ; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq package-enable-at-startup nil)
  ;;
  (setq inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message nil
        initial-buffer-choice t
        initial-scratch-message
        ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n"
        load-prefer-newer t
        ;; disable files from being created
        create-lockfiles nil
        auto-save-default nil
        backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups")))))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(when window-system
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(progn ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

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

;; set the os path
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

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
  ;; the native fullscreen in macOS is annoying
  (setq ns-use-srgb-colorspace t)
  ;; disable osx native fullscreen
  (setq ns-use-native-fullscreen nil)
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
