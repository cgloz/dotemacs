;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

(provide 'elegant_look)

;;; Look and Feel
;; ===============

(set-face-attribute 'default nil :height 125)
(set-face-attribute 'mode-line nil :height 300)

;;; theme
;; modus-themes
;; a dark and light accessibility theme
;; vivendi:dark::operandi:light
(use-package modus-themes
  :disabled
  :after elegance
	:init  
	(load-theme 'modus-vivendi t)
; (load-theme 'modus-operandi t)
)

;;; Olivetti
;; Olivetti cleans up fringes and centers text
;; like a "writeroom" or "zen" mode
(use-package olivetti
  ; :disabled
  :ensure t
  :config
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style t)
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-margin-width 1)
  (set-face-attribute 'olivetti-fringe nil  :background (face-background 'line-number))
  :hook (text-mode . olivetti-mode))

(use-package nano
  :disabled
  :straight '(nano-emacs :host github :repo "rougier/nano-emacs")
)

(use-package elegant
; :disabled
  :defer 0
  :straight '(elegant-emacs :host github :repo "rougier/elegant-emacs")
  :init
  ;; DUE TO SOME QUIRKS IN MODELINE RENDERING, I had to rebuild
  ;; the Modeline in the init process to it to render properly
  ;; This code below is copied from elegant.el
  (defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (concat left
          (propertize " " 'display `(space :align-to (- right ,(length right))))
          right))
(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line
        (propertize "%4l:%2c" 'face `(:inherit face-faded)))))))
    ;;END mode-line rehash
 :config
 (load-theme 'elegant-dark t)
 (set-face-attribute 'default nil :height 100)
)
  

(use-package mini-modeline
  :init
  (mini-modeline-mode t)
  
  ; (mini-modeline-r-format '())
  :after elegant
  :config
  (display-battery-mode t)
  (setq battery-mode-line-format "[%p]")
  (set-face-attribute 'mini-modeline-mode-line nil :height 0.1 :overline t :foreground nil :background nil)
  (set-face-attribute 'mini-modeline-mode-line-inactive nil :height 0.1 :overline t :foreground nil :background nil)
  (setq mini-modeline-r-format (list
		         '(:eval (propertize (format-time-string " %a") 'face 'bold)) " "
		         '(:eval (propertize (format-time-string "%d %b %Y"))) " "
		         '(:eval (propertize (format-time-string "%H") 'face 'bold))
		         '(:eval (propertize (format-time-string ":%M"))) " "
		         '(:eval (propertize battery-mode-line-string))
			 " "))
)
