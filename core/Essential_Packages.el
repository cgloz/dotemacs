;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;;; Org-Setup
;; =======================


  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
(defun my/org-todo-setup ()
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-special '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-warning '((t (:inherit (bold error org-todo)))) ""))
  (setq org-todo-keywords
	;; @ = leave note and timestamp ! = Timestamp
	;; /@ /! allow for note/stamp on LEAVING status
        '((sequence      ; --GENERAL ITEMS--
           "TODO(t)"     ; A task that needs doing & is ready to do
           "ACTV(a)"     ; A task that is in progress
           "WAIT(w@/!)"  ; Something external is holding up this task
           "PEND(p@)"    ; This task is paused/on hold because of me
	   "ASAP(s@)"    ; needs immediate attention
           "|"
           "DONE(d!)"    ; Task successfully completed
           "KILL(k!)")   ; Task was cancelled, aborted or is no longer applicable
	  (sequence      ; --SPECIAL TYPES OF TASKS--
           "PROJ(j@)"    ; A project, which usually contains other tasks
           "RCUR(r)"     ; A recurring task
           "IDEA(i)"     ; An unconfirmed and unapproved task or notion
	   "BRNR(b)"     ; Task fell on the back-burner
	   "PERP(P)"     ; Perpetual, work on it whenever
	   "REVU(R)"     ; Review details of task (usually in post)
	   )
          (sequence      ; --CHECKBOXES--
           "[ ](T)"      ; A task that needs doing
           "[-](-)"      ; Task is in progress
           "[?](?)"      ; Task is being held up or paused
	   "[*](*)"      ; requires action on my part
	   "[!](S)"      ; immediate attention
           "|"
           "[X](X)")     ; Task was completed
	  (sequence      ;--PROSPECTING--
	   "COLD(c)"     ; cold prospect
	   "ATMPT(>)"    ; attempted: started trying to engage
	   "FOLUP(f@/!)" ; Follow-up at later date
	   "REJCT(R@/!)" ; Response, not now or soft rejection
	   "MEET(M@/@)"  ; meeting booked
	   "|"
	   "DISQ(D@/@)"  ; diqualified: not a fit
	   "REFRL(o@)"   ; referred to other contact
	   "OPPT(O@)"    ; opportunity created
           "LOST(l@/!)") ; Lost to competitor
	  (sequence      ;--SALES--
	   "DISC(d!)"    ; Discovery phase
	   "RECO(e/@)"   ; Recommendation made
	   "DARK(D!/@)"  ; opp went dark
	   "NEGO(N!/@)"    ; Negotioate
	   "|"
	   "WON(w@)"     ; Close-Won
	   "LOST(l@/!)"  ; lost
	   ))
        org-todo-keyword-faces
        '(("ACTV"  . +org-todo-active)
	  ("[-]"   . +org-todo-active)
	  ("ATMPT" . +org-todo-active)
	  ("MEET"  . +org-todo-active)
          ("WAIT"  . +org-todo-onhold)
          ("PEND"  . +org-todo-onhold)
          ("[?]"   . +org-todo-onhold)
          ("[*]"   . +org-todo-onhold)
          ("FOLUP" . +org-todo-onhold)
          ("NURTR" . +org-todo-onhold)
          ("DARK"  . +org-todo-onhold)
          ("PROJ"  . +org-todo-special)
	  ("RCUR"  . +org-todo-special)  
          ("IDEA"  . +org-todo-special)
 	  ("BRNR"  . +org-todo-special)
 	  ("PERP"  . +org-todo-special)
 	  ("REVU"  . +org-todo-special)
 	  ("ASAP"  . +org-todo-warning)
 	  ("KILL"  . +org-todo-warning)
 	  ("[!]"   . +org-todo-warning)
 	  ("DISQ"  . +org-todo-warning)
 	  ("LOST"  . +org-todo-warning)))
  (setq org-priority-highest  1
	org-priority-lowest   10
	org-priority-default  5))
(defun my/org-mode-setup ()
  (org-num-mode) ; auto-numbered headings
  (org-indent-mode) ; indent based on heading level
  (visual-line-mode 1)
  (setq org-startup-indented t
	org-list-allow-alphabetical t ; ex. a. b. lists
        org-hide-leading-stars t ; for headings
	)
  (setq org-log-into-drawer t
        org-log-done 'time
	org-agenda-start-with-log-mode t)
)
(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (my/org-todo-setup))

;; allows for snippet expansion (see org  structure template manual)
;;  templates are execute for example "<s[tab]
(require 'org-tempo)
(require 'org-persist)
(require 'org-element)

;;; Org-Roam
;; ==========

(use-package emacsql-sqlite3
  :ensure t)

;; Org-Roam Second Brain
(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename (expand-file-name "Docs/PKB" emacs-parent-dir)))
  ;; I set emacs-parent-dir in my site-start.el, it sits over
  ;; wherever I have my emacs install
  (org-roam-completion-everywhere t)
  :config
;  (setq org-roam-database-connector 'sqlite3)
  (org-roam-db-autosync-mode)
)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>: %?"
         :if-new (file+head "%<%Y%m%d_%a>.org" "#+title: %<%a %d %b %Y>\n"))))

(setq org-roam-dailies-directory "CalendarNotes/")

;;; Misc
;; ======

;; speed read
(use-package spray)

;; launches apps from emacs
(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; rainbow delimiters helps see delimiters like paren()
;; better when editting code (particularly Lisp)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t)

;; Font-Lock Debugger
(use-package font-lock-studio)

;;; Completion System
;; ===================

;; gives move info when you M-x 
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for describe-bindings
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ;; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))

; helpful gives more in depth info in help dialogue
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
