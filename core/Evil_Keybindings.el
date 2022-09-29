(use-package general)


;; ====================
;;;       EVIL
;; ====================

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
  
;;; EVIL Sanity Adjustments
;; =========================

;; force normal and emacs states
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-\\ C-n") 'evil-normal-state)
(global-set-key (kbd "C-\\ C-e") 'evil-emacs-state)

(define-key evil-insert-state-map (kbd "M-j") 'evil-next-line)
(define-key evil-insert-state-map (kbd "M-k") 'evil-previous-line)
(define-key evil-insert-state-map (kbd "M-h") 'evil-backward-char)
(define-key evil-insert-state-map (kbd "M-l") 'evil-forward-char)

(define-key evil-insert-state-map (kbd "M-v") 'evil-visual-char-state)

(define-key evil-visual-state-map (kbd "C-c") #'copy-to-clipboard)
(define-key evil-insert-state-map (kbd "C-v") #'paste-from-clipboard)


(define-key evil-visual-state-map (kbd "C-x C-e") #'eval-region)

(define-key evil-motion-state-map (kbd "<SPC>") #'ignore) 

(general-create-definer my-leader-def
  ;; :prefix my-leader
  ;; RET = Enter
  :prefix "RET")

(my-leader-def 'normal 'override
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" (kbd "C-x")
  "RET" 'embark-dwim)

;; Disable SYSTEM Clipboard Integration
;; (Useful for Proper VIM Emulation in EVIL)
(setq x-select-enable-clipboard nil)

;; paste from SYSTEM clipboard command
(defun paste-from-clipboard ()
  (interactive)
  (setq x-select-enable-clipboard t)
  (yank)
  (setq x-select-enable-clipboard nil))

;; copy to SYSTEM clipboard command
(defun copy-to-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (kill-ring-save (region-beginning) (region-end))
  (setq x-select-enable-clipboard nil))

;; cursor colors per state
(setq evil-emacs-state-cursor '("violet" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("white" box))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '(hollow)) 

;; Missing Functions
;; -----------------
;; from https://emacs.stackexchange.com/questions/8126/zs-and-ze-from-vim
(defun hscroll-cursor-left ()
  (interactive "@")
  (set-window-hscroll (selected-window) (current-column)))

(defun hscroll-cursor-right ()
  (interactive "@")
  (set-window-hscroll (selected-window) (- (current-column) (window-width) -1)))

(define-key evil-normal-state-map "zs" 'hscroll-cursor-left)
(define-key evil-normal-state-map "ze" 'hscroll-cursor-right)
(setq auto-hscroll-mode 't)
(setq hscroll-margin 0
      hscroll-step 1)

(setq evil-emacs-state-modes '())
(setq evil-want-minibuffer t)
;; Org
;; ---

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; Sensible Evil bindings courtesy of DOOM
(define-key evil-normal-state-map "]h" 'outline-forward-same-level)
(define-key evil-normal-state-map "[h" 'outline-backward-same-level)
(define-key evil-normal-state-map "]l" 'org-next-link)
(define-key evil-normal-state-map "[l" 'org-previous-link)
(define-key evil-normal-state-map "]c" 'org-babel-next-src-block)
(define-key evil-normal-state-map "[c" 'org-babel-previous-src-block)
(define-key evil-normal-state-map "gQ" 'org-fill-paragraph)
; Evil folding
(define-key evil-normal-state-map "za" 'outline-toggle-children)
(define-key evil-normal-state-map "zA" 'org-shifttab) ;; need better solution
(define-key evil-normal-state-map "zc" 'outline-hide-subtree)
(define-key evil-normal-state-map "zC" '+org/somethingIneeeddoom) ;; need better solution
;;(define-key evil-normal-state-map "zm" '+org/hide-next-fold-level) ;; problem finding a suitable org-cycle but in reverse...
(define-key evil-normal-state-map "zM" 'org-content)
(define-key evil-normal-state-map "zn" 'org-tree-to-indirect-buffer)
(define-key evil-normal-state-map "zo" 'outline-show-subtree)
(define-key evil-normal-state-map "zO" '+org/somethingdoom) ;; need better solution
;; (define-key evil-normal-state-map "zr" '+org/show-next-fold-level) ;; issues finding suitable zr solution, trying doom-emacs 
(define-key evil-normal-state-map "zR" 'outline-show-all)
(define-key evil-normal-state-map "zi" 'org-toggle-inline-images)
