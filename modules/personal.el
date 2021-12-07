;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

(provide 'personal)

;;; RCIRC

(setq rcirc-server-alist
    '(("irc.libera.chat" :port 6697 :encryption tls
       :channels ("#rcirc" "#emacs" "#emacs-beginners" "#emacs-til" "#emacswiki"))))
(setq rcirc-default-nick  "cgloz")

