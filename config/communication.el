(use-package message
  :hook (message-setup . mml-secure-message-sign-pgpmime))

(use-package notmuch
  :commands (notmuch)
  :preface
  (defun mg/notmuch-filter-search-view-unread (&optional beg end)
      "display only unread messages for the current search view"
      (interactive (notmuch-search-interactive-region))
      (notmuch-search-filter-by-tag "unread"))
  :bind ((:map notmuch-search-mode-map
               ("u" . mg/notmuch-filter-search-view-unread))
         (:map notmuch-show-mode-map
               ("C-c C-o" . browse-url-at-point)))
  :config
  (setq notmuch-hello-sections '(notmuch-hello-insert-search
                                 notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-saved-searches
        '((:name "unread" :query "tag:unread and not (tag:mailing-list or tag:github)" :key "u")
          (:name "inbox" :query "tag:inbox" :key "i")
          (:name "flagged and not replied" :query "tag:flagged and not tag:replied" :key "f")
          (:name "flagged" :query "tag:flagged" :key "F")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "today" :query "tag:inbox and date:-24h..now and not tag:mailing-list" :key "t")
          (:name "mailing-lists" :key "m"
                 :query "tag:mailing-list and tag:unread" :sort-order 'newest-first
                 :count-query "tag:mailing-list and tag:unread")
          (:name "mailing-list archive" :key "M"
                 :query "tag:mailing-list" :sort-order 'newest-first)
          (:name "github" :key "g"
                 :query "tag:github and tag:unread" :sort-order 'newest-first
                 :count-query "tag:github and tag:unread")
          (:name "github archive" :key "G"
                 :query "tag:github" :sort-order 'newest-first)
          (:name "connections" :key "c"
                 :query "tag:friend-request date:-7d..now"
                 :count-query "tag:friend-request date:-24h..now"))))

(use-package counsel-notmuch
  :after (notmuch counsel))
