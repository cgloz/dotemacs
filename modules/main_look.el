;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;; Modules need to provide the variable
;;   that will be (required) in the init
(provide 'main_look)

;;; Look and Feel
;; ===============

(setq window-divider-mode nil)

(defun mode-line-render (left-text mid-text right-text)
  "Function to render the modeline LEFT, CENTER and RIGHT."
  ;; mode-line-render requires 3 "format-mode-line" calls, each will
  ;;  be properly placed in the far left, middle, and right
  (concat (propertize " " 'display `(space
				     :align-to
				     (- left)))
	  left-text
	  (propertize " " 'display `(space
				     :align-to
				     (- center ,(/ (length mid-text) 2))))
	  mid-text
	  (propertize " " 'display `(space
				     :align-to (- right ,(length right-text))))
	  right-text))
(setq-default header-line-format
     '((:eval
       (mode-line-render
       (format-mode-line
       (if (and buffer-file-name (buffer-modified-p))
             (propertize " %*%+ " 'face '(:foreground "red" :inherit bold))
         (propertize " %*%+ " 'face 'bold)))
       (format-mode-line (propertize "%b" 'face 'variable-pitch))
       (format-mode-line
        (propertize "%l,%2c  %p%%" 'face 'bold))))))
;; I added a space after "%2c" because in the terminal
;;   the line gets cut off on the right vertical border
;;   between windows

;;; visual packages
(use-package badwolf-theme
  :disabled
  :init
    (load-theme 'badwolf t))
(use-package green-phosphor-theme
  :disabled
  :init
    (set-face-attribute 'line-number-current-line nil :weight 'bold :inverse-video t)
    (set-face-attribute 'line-number nil :foreground "darkolivegreen" :background "#000D00")
    (set-face-attribute 'fringe nil :foreground "darkolivegreen")
    (load-theme 'green-phosphor t)
:config
    (defun clear-hl()
      (set-face-attribute 'highlight nil :underline t :background nil :foreground nil))
    (clear-hl)
:hook (minibuffer-setup . clear-hl))
;; modus-themes
;; a dark and light accessibility theme
;; vivendi:dark::operandi:light
(use-package modus-themes
;  :disabled
  :init  
	(load-theme 'modus-vivendi t)
;       (load-theme 'modus-operandi t)
)

;;; Olivetti
;; Olivetti cleans up fringes and centers text
;; like a "writeroom" or "zen" modee
(use-package olivetti
  :ensure t
  :init
;  (set-face-background 'olivetti-fringe (face-background 'line-number))
  :config
  (set-face-background 'olivetti-fringe (face-background 'line-number))
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style nil)
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-margin-width 1)
  (set-face-attribute 'olivetti-fringe nil  :background (face-background 'line-number))
  :hook (text-mode . olivetti-mode))


;;;### autoload
(defun clean-borders()
  "make a clean border the same color as the line number background"
  
  (setq left-margin-width 1)
  (setq right-margin-width 5)
  (set-face-background 'internal-border (face-background 'line-number) (selected-frame))
  (set-frame-parameter (selected-frame) 'internal-border-width 3)
  (set-face-attribute 'fringe nil :background (face-background 'line-number)) 
  (set-face-attribute 'header-line nil :background (face-background 'line-number))  
  (set-face-attribute 'line-number-current-line nil :background (face-background 'default))  
;;; defined custom variable line-bg with let and set it to line-number bg
;;    then I can use that to set box color properly (it creates issues when
;;    using set-face-attribute... (:box '(:color)" for whatever reason
(let ((line-bg (face-attribute 'line-number :background)))
(custom-set-faces
 `(header-line ((t :box (:line-width
			 5 :color ,line-bg) :background ,line-bg)))
 `(vertical-border ((t :background ,line-bg :foreground ,line-bg)))))
)
;; function only seems to load right
;; if it loads after everything else
(add-hook 'emacs-startup-hook 'clean-borders)

;;; Mini-Modeline
;; puts modeline in mini-buffer creating a sort
;;   of global modeline
(use-package mini-modeline
  :after (:any badwolf-theme modus-themes green-phosphor-theme)
  :init
  ;;(display-battery-mode t)
  (setq battery-mode-line-format "[%p]")
  (set-face-attribute 'mode-line nil :box t :foreground nil :background (face-background 'line-number))
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil :foreground nil :background nil)
  :config
  (mini-modeline-mode t)
(setq mode-line-format "%b") 
   (set-face-attribute 'mini-modeline-mode-line nil :height 0.4 :background (face-background 'line-number))
   (set-face-attribute 'mini-modeline-mode-line-inactive nil :height 0.4 :background (face-background 'line-number)) 
   ;; display mode info on the left side
   (setq mini-modeline-l-format mode-line-modes)
   ;; display global info like day, time, battery on right
   (setq mini-modeline-r-format (list
			 "%e "
		         '(:eval (propertize (format-time-string " %a") 'face 'bold)) " "
		         '(:eval (propertize (format-time-string "%d %b %Y"))) " "
		         '(:eval (propertize (format-time-string "%H") 'face 'bold))
		         '(:eval (propertize (format-time-string ":%M"))) " "
		         '(:eval (propertize battery-mode-line-string))
			 " "))
   
  ;; the real mode-line is shrunk in mini-modeline mode. Some modes
  ;;   still use it, the function resizes it for those use-cases 
   (defun mode-line-height-delta(new-height)
     (set-face-attribute 'mini-modeline-mode-line nil
			 :height new-height)
     (set-face-attribute 'mini-modeline-mode-line-inactive nil
			 :height new-height))

(add-hook 'minibuffer-exit-hook (lambda()(mode-line-height-delta 0.4)))
(add-hook 'eval-expression-minibuffer-setup-hook (lambda()( mode-line-height-delta 1.0)))
)


;; Font Settings
;; -------------

(defface variable-pitch-serif
  '((t :family "serif" :foundry "outline"))
  "The basic variable pitch face with serifs."
  :group 'basic-faces
  )

(set-frame-font
 (cond
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas" nil ))
  ((string-equal system-type "darwin")
   (if (member "Menlo" (font-family-list)) "Menlo-16" nil ))
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil ))
  (t nil))
 t t)

;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman skum disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 )

;; Change Default Font Height
(set-face-attribute 'default nil :height 125)

;; ORG Mode Settings
;; =================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '(" ")))

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-fixed-pitch-faces (-union mixed-pitch-fixed-pitch-faces (list
            'org-date
            'org-footnote
            'org-special-keyword
	    'org-drawer
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todos
            'org-done
	    'org-special-keyword
            'font-lock-comment-face))))

(defun cgloz/org-mode-setup ()
  (org-num-mode) ; auto-numbered headings
  (org-indent-mode) ; indent based on heading level
  (setq org-startup-indented t
        org-hide-leading-stars t)
  (visual-line-mode 1)
; (set-window-buffer nil (current-buffer))
)

 (defun cgloz/org-font-setup ()
    ;; Set faces for heading levels
   (dolist (face '((org-document-title . 1.75)
 		  (org-level-1 . 1.5)
                   (org-level-2 . 1.4)
                   (org-level-3 . 1.35)
                   (org-level-4 . 1.3)
                   (org-level-5 . 1.25)

                   (org-level-6 . 1.25)
                   (org-level-7 . 1.25)
                   (org-level-8 . 1.1)
 		  (org-table . 0.75)))
     (set-face-attribute (car face) nil :height (cdr face) :width 'normal :inherit 'variable-pitch-serif))
  
   ;; Ensure that anything that should be fixed-pitch in Org files appears that way

    (set-face-attribute 'org-code nil     :inherit 'shadow)
    (set-face-attribute 'org-table nil    :inherit 'shadow)
    (set-face-attribute 'org-verbatim nil :inherit 'shadow)
    (set-face-attribute 'org-special-keyword nil :inherit 'font-lock-comment-face)
    (set-face-attribute 'org-meta-line nil :inherit 'font-lock-comment-face)
 )



(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . cgloz/org-mode-setup)
  :config
  (setq org-ellipsis "...")
  (setq org-ellipsis '(:inherit (variable-pitch shadow) :underline nil))
  (setq org-num-face '(:inherit variable-pitch-serif :weight bold))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
 (cgloz/org-font-setup)
  )
