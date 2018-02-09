(use-package prog-mode
  :config
  (global-prettify-symbols-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook
  (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (show-smartparens-global-mode))

;;
;; Lisp
;;
(use-package lisp-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   (emacs-lisp-mode . reveal-mode))
  :bind ((:map emacs-lisp-mode-map
                 ("C-c C-]" . outline-toggle-children))))

(use-package eldoc
  :after lisp-mode
  :if (version< "25" emacs-version)
  :config (global-eldoc-mode))

;;
;; Erlang, todo, see if we can get this into use-package, somehow
;;
(let* ((default-directory "/usr/local/lib/erlang")
       (tools-version "2.11.1")
       (erlang-tools-dir (concat "lib/tools-" tools-version "/emacs")))
  (add-to-list 'load-path (expand-file-name erlang-tools-dir))
  (add-to-list 'exec-path (expand-file-name "bin"))
  (use-package erlang-start)
  (use-package erlang
    :preface
    (defun mg/my-erlang-mode-hook ()
      (set (make-local-variable 'compilation-read-command) nil)
      (flycheck-mode 1)
      (smartparens-mode 1))
    :hook ((erlang-mode . mg/my-erlang-mode-hook)
           (erlang-mode . flyspell-prog-mode))
    :bind ((:map erlang-mode-map
                 ("C-c C-c" . projectile-compile-project)
                 ("C-c C-t" . projectile-test-project)
                 ("C-c SPC" . imenu)))
    :config
    (projectile-register-project-type 'erlang '("rebar.config")
                                      :compile "make compile"
                                      :test "make dialyzer && make test"
                                      :run "make dev-rel")))
;;
;; Elixir
;;
(use-package elixir-mode
  :preface
  (defun mg/elixir-create-scratch-buffer ()
    "Open a buffer in elixir/alchemist mode; evaluating the
expressions with Elixir"
    (interactive)
    (switch-to-buffer "*elixir scratch*")
    (elixir-mode))
  (defun mg/my-elixir-mode-hook ()
    (set (make-local-variable 'compilation-read-command) nil))
  :hook ((elixir-mode . flyspell-prog-mode)
         (elixir-mode . smartparens-mode)
         (elixir-mode . alchemist-mode)
         (elixir-mode . mg/my-elixir-mode-hook))
  :bind ((:map elixir-mode-map
               ("C-c C-c" . projectile-compile-project)
               ("C-c C-t" . projectile-test-project)))
  :config
  (projectile-register-project-type 'elixir '("mix.exs")
                                    :compile "mix compile"
                                    :test "mix test"
                                    :run "mix app.start --temporary")
  (let ((default-directory "~/Development/github.com/gausby/"))
    (add-to-list 'load-path (expand-file-name "mg-elixir-snippets/"))
    (require 'mg-elixir-snippets)))

(use-package alchemist
  :after (elixir-mode)
  :bind ((:map elixir-mode-map
               ("C-c SPC" . alchemist-mix)
               ("M-g ." . alchemist-goto-definition-at-point)
               ("C-c h a" . alchemist-help)
               ("C-c h ." . alchemist-help-search-at-point)))
  :config
  (let ((default-directory "~/.exenv/shims/"))
    (setq alchemist-mix-command (expand-file-name "mix")
          alchemist-execute-command (expand-file-name "elixir")
          alchemist-compile-command (expand-file-name "elixirc")
          alchemist-iex-program-name (expand-file-name "iex"))))
