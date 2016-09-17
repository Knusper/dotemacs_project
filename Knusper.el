(server-start)

(setq custom-file "~/.gnu-emacs-custom")
(load custom-file)

(setq user-full-name "Edmund Christian Herenz"
      user-mail-address "cherenz@aip.de")

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defvar my/install-packages
  '(;; appearance
    powerline
    beacon
    zenburn-theme  

    ;; fun
    xkcd
    fireplace
    tea-time
    
    ;; essential
    buffer-move
    smex
    rainbow-delimiters
    htmlize
    iedit  
    auto-complete
    ido
    writeroom-mode

    ;; email
    muttrc-mode
    offlineimap
    
    ;; language specific
    markdown-mode
    ;; auctex (I prefer to have always a recent stable release manuaully
    ;; installed)
    
    ;; python 
    jedi
    
    ;; org-mode (I prefer to have always a recent stable release manually
    ;; installed)
    org-bullets
  ))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(defvar packages-refreshed? nil)

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (unless packages-refreshed?
      (package-refresh-contents)
      (setq packages-refreshed? t))
    (package-install pack)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn))

(use-package htmlize
  :ensure t)

(use-package xkcd
 :ensure t
 :defer t)

(use-package iedit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :init (setq auto-mode-alist
              (cons '("\\.mdml$" . markdown-mode) auto-mode-alist)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "<S-s-up>")     'buf-move-up)
  (global-set-key (kbd "<S-s-down>")   'buf-move-down)
  (global-set-key (kbd "<S-s-left>")   'buf-move-left)
  (global-set-key (kbd "<S-s-right>")  'buf-move-right)
  )

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-dont-blink-commands nil) ;; always blink
  )

(use-package jedi
  :ensure t
  )

(use-package writeroom-mode
  :ensure t)

(use-package muttrc-mode
  :ensure t
  :config
   (setq auto-mode-alist
            (append '((".muttrc\\'" . muttrc-mode))
                    auto-mode-alist)))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))

(use-package tea-time
  :ensure t
  :config
  (setq tea-time-sound "~/.sounds/tea.ogg")
  (setq tea-time-sound-command "ogg123 -q %s")
  )

(use-package ebib
  :ensure t
)

(use-package post)

(use-package simple-wiki)

(use-package wikidot-mode)

(use-package printing
  :config
  (pr-update-menus t))

(require 'uniquify)

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "4:30am"))

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-max-prospects 50)
  (setq ido-max-window-height 0.25)
  )

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode 1))

(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)

(global-set-key (kbd "<menu>") 'nil)

(blink-cursor-mode 0)

(setq shift-select-mode nil)

(setq mouse-yank-at-point t)

(setq transient-mark-mode t)

(show-paren-mode t)

(recentf-mode 1)

(global-set-key "\M- " 'hippie-expand)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
            kill-emacs-query-functions))

(put 'upcase-region 'disabled nil)

(desktop-save-mode 1)
(setq desktop-restore-eager 10)
(setq desktop-save t) ;; save without asking

(defalias 'list-buffers 'ibuffer)

(electric-pair-mode 1)
(defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
(defun markdown-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%d.%m.%Y, %H:%M")))

(defun my-count-words-region (posBegin posEnd)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting …")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))

      (message "Words: %d. Chars: %d." wordCount charCount)
      )))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(setq LaTeX-math-menu-unicode t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)
(setq reftex-cite-format 'natbib)

(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
