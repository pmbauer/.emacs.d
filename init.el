;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings slime slime-repl undo-tree clojure-mode clojure-test-mode color-theme-solarized haskell-mode auto-complete ac-nrepl))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load-path extensions
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/extras")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "/opt/lono-dev-env/emacs")

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                       (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; lonopair
(require 'lonopair)
(defun lono-slime-hook-function ()
  (local-set-key (kbd "C-c C-s") 'nrepl-send-dwim))

;; paredit tweaks
(defun paredit-duplicate-after-point
  ()
  "Duplicates the content of the line that is after the point."
  (interactive)
  ;; skips to the next sexp
  (while (looking-at " ")
    (forward-char))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(eval-after-load "paredit"
  '(progn
     ;;(define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-barf-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)
     ;;(define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-barf-sexp)
     ;;(define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
     ;;(define-key paredit-mode-map (kbd "C-k") 'paredit-eager-kill-line)
     (define-key paredit-mode-map (kbd "C-S-d") 'paredit-duplicate-after-point)
     ;;(define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
     ))

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "M-]") 'find-tag)
(define-key evil-normal-state-map (kbd "M-,") 'nrepl-jump-back)
(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)
(define-key evil-normal-state-map (kbd "C-x M-x") 'nrepl-send-dwim)
;(define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)
;(define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
;(define-key evil-normal-state-map (kbd "C-x M-x") 'slime-send-dwim)

;; nrepl
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-mode-hook 'paredit-mode)
; autocomplete
(require 'ac-nrepl)
 (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
 (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; visual
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-light)
     ))

(set-default-font "Ubuntu Mono-11")
(blink-cursor-mode)

(if window-system
    (progn  ; Non-terminal adjustments
      )

  (progn    ; Terminal adjustments
    ;; The light colored hl-line is fine for GUI emacs but makes
    ;; it impossible to read things in a white-on-black terminal
    (defadvice hl-line-mode (after
                             abrooks-advise-hl-line-mode
                             activate compile)
      (set-face-background hl-line-face "white"))))

;; prevent extraneous tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; setup dired+
(require 'dired+)

;; setup revive
(require 'revive)

;; clojure related
(require 'slamhound)
(setq auto-mode-alist (cons '("\\.txn$" . clojure-mode) auto-mode-alist))

;; elm related
(setq auto-mode-alist (cons '("\\.elm$" . haskell-mode) auto-mode-alist))
