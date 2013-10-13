;; -----------------------------------------------------------------------------
;; JABBER
;; -----------------------------------------------------------------------------

(require-package 'emacs-jabber 'jabber-connect-all t)

(eval-after-load 'jabber '(progn
  
  ;; accounts
  (setq jabber-account-list '(
    ("bertfrees@jabber.belnet.be")
    ("bertfrees@gmail.com" (:network-server . "talk.google.com")
                           (:port . 5223)
                           (:connection-type . ssl))
    ("bertfrees@chat.facebook.com")))
  
  ;; history
  (setq
    jabber-history-enabled t
    jabber-use-global-history nil
    jabber-backlog-number 40
    jabber-backlog-days 30)))