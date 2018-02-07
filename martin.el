;; auto load files when they change on disk
(global-auto-revert-mode t)
;; remove whitespace when buffers are saved
(add-hook 'before-save-hook 'whitespace-cleanup)

;; set text input and behaviour ----------------------------------------
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; overwrite regions on keypress
(delete-selection-mode t)

(global-hl-line-mode t)
(set-default 'indicate-buffer-boundaries '((top . nil) (bottom . right)))

;; scroll one line at a time
(setq scroll-step 1
      ;; don't go bananas when scrolling
      scroll-conservatively 10000)

;; Make it difficult to quit emacs
(define-key ctl-x-map (kbd "C-S-c") 'save-buffers-kill-terminal)
(define-key ctl-x-map (kbd "C-c") 'delete-frame)

;; Remove warnings when using certain commands
(put 'narrow-to-region 'disabled nil)


;; mode line -----------------------------------------------------------
(setq uniquify-buffer-name-style 'forward)



;; keep
(use-package material-theme
  :config
  (setq custom-safe-themes t)
  (load-theme 'material)
  ;; Remove the background box on headlines in org-mode buffers
  (let ((headline `(:background nil :box nil))
        (class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (:height 1.0))))
     `(org-level-3 ((t (:height 1.0))))
     `(org-level-2 ((t (,@headline :height 1.0))))
     `(org-level-1 ((t (,@headline :height 1.0))))
     `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
     `(org-block-begin-line ((t (:box nil))))
     `(org-block-end-line ((t (:box nil))))
     `(org-block ((t (:foreground nil :background "#1e2930"))))
     `(org-code ((t (:background "#1e2930"))))
     ;; I find this rainbow-delimiters scheme better for non-lisp modes:
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,"#C0C0C0"))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#B388FF"))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#26A69A"))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#EF6C00"))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#2196F3"))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#FFCDD2"))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#8BC34A"))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#FFF59D"))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#795548"))))
     `(rainbow-delimiters-unmatched-face ((,class (:foreground ,"#F0F0F0" :background ,"#F36C60"))))
    )))

(when (eq window-system 'ns)
  ;; set fonts
  (let ((font "source code pro"))
    (set-frame-font font)
    (set-face-font 'default font)
    (set-face-attribute 'default nil :height 144)
    (set-face-attribute 'mode-line nil :font font :height 90)
    (set-face-attribute 'mode-line-inactive nil :font font :height 90)))

;; Remap PgUp and PgDn to macro definition and execution. These keys
;; are located far away from the home-row on a Saber68 keyboard.
(global-set-key [(prior)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(next)] 'kmacro-end-or-call-macro)
