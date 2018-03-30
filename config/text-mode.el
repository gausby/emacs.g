(use-package text-mode
  :preface
  (defun mg/self-insert-or-wrap-command (n)
    "Insert the typed character, or wrap the current selection
in the typed character if a region is active."
    (interactive "p")
    (if (not (use-region-p))
        (self-insert-command n)
      (let* ((start-pos (point))
             (start (region-beginning))
             (end (region-end)))
        (goto-char end) (self-insert-command n)
        (goto-char start) (self-insert-command n)
        ;; position the cursor after the inserts
        (goto-char
         (if (= start-pos end)
             (+ start-pos (* 2 n))
           start-pos))))))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package flyspell
  :hook (text-mode . flyspell-mode))

(use-package flyspell-correct-ivy
  :after (ivy flyspell)
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

(use-package pandoc-mode)

(use-package markdown-mode
  :mode (("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind ((:map markdown-mode-map
               ("*" . mg/self-insert-or-wrap-command)
               ("_" . mg/self-insert-or-wrap-command)
               ("`" . mg/self-insert-or-wrap-command))))

(use-package fountain-mode
  :mode ("\\.fountain\\'" . fountain-mode)
  :bind ((:map fountain-mode-map
               ("C-c SPC" . imenu)
               ("#" . mg/self-insert-or-wrap-command)
               ("*" . mg/self-insert-or-wrap-command)
               ("_" . mg/self-insert-or-wrap-command)))
  :hook ((fountain-mode . smartparens-mode)))

(use-package ledger-mode
  :preface
  (defun mg/my-ledger-mode-hook ()
    ;; company is currently a bit slow in ledger
    (company-mode 0))
  :hook (ledger-mode . mg/my-ledger-mode-hook)
  :mode "\\.dat\\'"
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-highlight-xact-under-point nil))

(use-package flycheck-ledger
  :after (ledger-mode flycheck)
  :config
  (setq flycheck-ledger-executable "hledger"))
