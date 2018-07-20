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
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)))

;;
;; restclient
;;
(use-package restclient
  :hook ((restclient-mode . smartparens-mode)
         (restclient-mode . flyspell-prog-mode))
  :config
  (mg/add-shackle-rule '("*HTTP Response*" :select t :align below :size 0.33)))

;;
;; Erlang, todo, see if we can get this into use-package, somehow
;;
(let* ((default-directory "/usr/local/lib/erlang")
       (tools-version "3.0")
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
  :after projectile
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
  :init
  (projectile-register-project-type 'elixir '("mix.exs")
                                    :compile "mix compile"
                                    :test "mix test"
                                    :run "mix app.start --temporary"
                                    :src-dir "lib/"
                                    :test-dir "test/")
  :config
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
  :init
  (let ((default-directory "~/.exenv/shims/"))
    (setq alchemist-mix-command (expand-file-name "mix")
          alchemist-execute-command (expand-file-name "elixir")
          alchemist-compile-command (expand-file-name "elixirc")
          alchemist-iex-program-name (expand-file-name "iex"))))

(use-package request)
(use-package graphql-mode
  :after request)

;;
;; JSON is one of the worst formats ever to end up as Lingua franca
;;
;; Unfortunately a lot of software export and import to this gosh darn
;; awful format, so it is needed.
;;
;; The `json-snatcher` package seems to work 20% of the time, but
;; `json-mode` require it for some reason--other than that json-mode
;; seems pretty good, considering it is dealing with JSON.
;;
;; json-reformat can be used to make the horrible JSON format a bit
;; nicer to read--although it has a ton of bugs that will actually
;; delete data from the structure in certain situations.
;;
;; All this might get removed at some point.
;;
(use-package json-snatcher)
(use-package json-mode
  :after json-snatcher
  :hook ((json-mode . smartparens-mode)))
(use-package json-reformat
  :after json-mode
  :preface
  (defun mg/json-reformat ()
    "Attempt to json-reformat the region if selected, otherwise
mark the entire buffer and run the json-reformat command on that"
    (interactive)
    (if (use-region-p) (call-interactively 'json-reformat-region)
      (json-reformat-region (point-min) (point-max))))
  :bind ((:map json-mode-map
               ("C-c TAB" . mg/json-reformat))))

;;
;; elm
;;
;; todo, get elm-format working and bind it to a suitable binding
;;
(use-package elm-mode
  :defer t
  :preface
  (defun mg/my-elm-mode-hook ()
    (flycheck-mode 1)
    (smartparens-mode 1))
  :hook ((elm-mode . mg/my-elm-mode-hook)))
(use-package flycheck-elm
  :after (flycheck elm-mode)
  :init
  (setq flycheck-elm-reporting-mode 'all)
  :config
  (flycheck-elm-setup))

;;
;; OCaml
;;
(use-package tuareg
  :preface
  (setq opam-bin (ignore-errors (car (process-lines "opam" "config" "var" "bin"))))
  (setq opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share"))))
  :if (file-exists-p opam-bin)
  :bind ((:map tuareg-mode-map
               ("C-c SPC" . imenu)))
  :config
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (use-package merlin
    :hook (tuareg-mode . merlin-mode))
  ;; todo, make flycheck work in ocaml; enabling this will fail with a
  ;; call to an undefined function:

  ;; (use-package flycheck-ocaml
  ;;   :after (flycheck merlin)
  ;;   :hook (merlin-mode . flycheck-mode)
  ;;   :preface
  ;;   (setq merlin-error-after-save nil)
  ;;   :init
  ;;   (flycheck-ocaml-setup))
  )
