
# Configuration<a id="orgheadline69"></a>

-   Somewhat inspired by [Sacha Chua's Emacs configuration](http://sach.ac/dotemacs).

-   Github repository for this file: <https://github.com/Knusper/dotemacs_project>

-   HTML: <http://fortune-teller-amy-88756.netlify.com/Knusper.html>

-   Org: <http://fortune-teller-amy-88756.netlify.com/Knusper.org>

-   .el: <http://fortune-teller-amy-88756.netlify.com/Knusper.el>

## Init file<a id="orgheadline6"></a>

We still need an [Init File](http://www.emacswiki.org/emacs/InitFile) (`~/.emacs.d/init.el` or `~/.emacs`)
that loads `org-mode`, sets up the paths for packages that we don't
install via the package manager.

    ;; ~/.emacs.d/init.el
    
    ;; This sets up the load path so that we can override it
    (package-initialize nil)
    (add-to-list 'load-path (expand-file-name "~/emacs-scripts/"))
    ;; org-mode
    (add-to-list 'load-path (expand-file-name "~/emacs-scripts/org-8.3.4/lisp/"))
    (add-to-list  'Info-default-directory-list "~/emacs-scripts/org-8.3.4/doc/")
    ;; auctex
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auctex"))
    (load "auctex.el" nil t t)
    (load "preview-latex.el" nil t t)
    
    ;; Load the rest of the packages
    (package-initialize nil)
    (setq package-enable-at-startup nil)
    (org-babel-load-file "~/dotemacs_project/Knusper.org")

Here `org-babel-load-file` loads this file, then uses `org-babel-tangle`
to extract all `emacs-lisp` code into `Knusper.el`, and finally uses
`load-file` to load it.

### Notes on org-mode installation<a id="orgheadline1"></a>

-   Just run `make`
-   Version currently installed: 8.3.4

### Notes on AucTeX installation<a id="orgheadline2"></a>

-   Download AucTeX from <https://www.gnu.org/software/auctex/download-for-unix.html>
-   Compile using `./configure --prefix=$HOME/.emacs.d/site-lisp/auctex --with-lispdir=$HOME/.emacs.d/site-lisp/auctex --without-texmf-dir`
-   Version currently installed: 11.89

### General<a id="orgheadline5"></a>

#### Start the emacs server<a id="orgheadline3"></a>

    (server-start)

#### Load Custom File<a id="orgheadline4"></a>

    (setq custom-file "~/.gnu-emacs-custom")
    (load custom-file)

^

## Personal Information<a id="orgheadline7"></a>

    (setq user-full-name "Edmund Christian Herenz"
          user-mail-address "cherenz@aip.de")

## Packages<a id="orgheadline40"></a>

### Package Archives<a id="orgheadline8"></a>

MELPA is the king of emacs package archives.  Follow [MELPA on Twitter](https://twitter.com/melpa_emacs).

    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

### List of packages I use<a id="orgheadline9"></a>

I use the following MELPA packages.

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

### use-package - automatically download and install packages<a id="orgheadline10"></a>

We use [`use-package`](https://github.com/jwiegley/use-package) to automatically download and install packages,
if they are not present on the system.  Of course, when firing up
EMACS for the first time on a fresh system, `use-package` needs to be
automagically installed first.

    (if (not (package-installed-p 'use-package))
        (progn
          (package-refresh-contents)
          (package-install 'use-package)))
    
    (require 'use-package)

Now install all packaages that are not on the system using the list in [1.3.2](#orgheadline9).

    (defvar packages-refreshed? nil)
    
    (dolist (pack my/install-packages)
      (unless (package-installed-p pack)
        (unless packages-refreshed?
          (package-refresh-contents)
          (setq packages-refreshed? t))
        (package-install pack)))

(via <http://writequit.org/org/settings.html>)

### Configuration for packages from MELPA<a id="orgheadline29"></a>

#### zenburn color theme<a id="orgheadline11"></a>

    (use-package zenburn-theme
      :ensure t
      :config
      (load-theme 'zenburn))

#### htmlize<a id="orgheadline12"></a>

<http://www.emacswiki.org/emacs/Htmlize>

    (use-package htmlize
      :ensure t)

#### xkcd<a id="orgheadline13"></a>

<https://github.com/vibhavp/emacs-xkcd>

[xkcd](http://xkcd.com/) reader for Emacs. Nerd on!

    (use-package xkcd
     :ensure t
     :defer t)

#### iedit<a id="orgheadline14"></a>

Simultaneously edit multiple regions in buffer.
<http://www.emacswiki.org/emacs/Iedit>

    (use-package iedit
      :ensure t)

#### markdown-mode<a id="orgheadline15"></a>

<http://jblevins.org/projects/markdown-mode/>

    (use-package markdown-mode
      :ensure t
      :init (setq auto-mode-alist
                  (cons '("\\.mdml$" . markdown-mode) auto-mode-alist)))

#### rainbow-delimiters<a id="orgheadline16"></a>

<https://www.emacswiki.org/emacs/RainbowDelimiters>

    (use-package rainbow-delimiters
      :ensure t
      :config
      (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
      )

#### buffer-move<a id="orgheadline17"></a>

<https://github.com/lukhas/buffer-move>

    (use-package buffer-move
      :ensure t
      :config
      (global-set-key (kbd "<S-s-up>")     'buf-move-up)
      (global-set-key (kbd "<S-s-down>")   'buf-move-down)
      (global-set-key (kbd "<S-s-left>")   'buf-move-left)
      (global-set-key (kbd "<S-s-right>")  'buf-move-right)
      )

#### smex<a id="orgheadline18"></a>

    (use-package smex
      :ensure t
      :config
      (smex-initialize)
      (global-set-key (kbd "M-x") 'smex)
      )

#### powerline<a id="orgheadline19"></a>

    (use-package powerline
      :ensure t
      :config
      (powerline-default-theme)
      )

#### beacon<a id="orgheadline20"></a>

    (use-package beacon
      :ensure t
      :config
      (beacon-mode 1)
      (setq beacon-dont-blink-commands nil) ;; always blink
      )

#### jedi<a id="orgheadline21"></a>

<https://tkf.github.io/emacs-jedi/latest/>

    (use-package jedi
      :ensure t
      )

#### auto-complete<a id="orgheadline22"></a>

<http://auto-complete.org/>

    (use-package auto-complete
      :ensure t
      :config
      (global-auto-complete-mode t) 
      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:setup-keys t)
     )

#### writeroom-mode<a id="orgheadline23"></a>

Distraction free writing.
<https://github.com/joostkremers/writeroom-mode>

    (use-package writeroom-mode
      :ensure t)

#### muttrc-mode<a id="orgheadline24"></a>

Syntax highlighting in [muttrc file](http://linux.die.net/man/5/muttrc).

    (use-package muttrc-mode
      :ensure t
      :config
       (setq auto-mode-alist
                (append '((".muttrc\\'" . muttrc-mode))
                        auto-mode-alist)))

#### org-bullets<a id="orgheadline25"></a>

The following gold is from
<https://thraxys.wordpress.com/2016/01/14/pimp-up-your-org-agenda/>

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

#### tea-time<a id="orgheadline26"></a>

With `tea-time` I never forget about the tea.  Using  [this soundbite](http://movie-sounds.org/action-movie-sound-clips/the-italian-job-1969/have-a-cup-of-tea-ready)
from my favorite movie "The Italian Job".

    (use-package tea-time
      :ensure t
      :config
      (setq tea-time-sound "~/.sounds/tea.ogg")
      (setq tea-time-sound-command "ogg123 -q %s")
      )

#### ebib<a id="orgheadline27"></a>

Browse / edit BibTeX bibliographies in emacs.
<http://ebib.sourceforge.net/>

    (use-package ebib
      :ensure t
    )

#### fireplace<a id="orgheadline28"></a>

It can get cold in winter. `M-x fireplace`
<https://github.com/johanvts/emacs-fireplace>

### Packages not in ELPA or MELPA<a id="orgheadline33"></a>

These packages are in `~/emacs-scripts/` as specfied in the
load-path in [1.1](#orgheadline6).

#### post-mode for mutt<a id="orgheadline30"></a>

<http://post-mode.sourceforge.net/>

    (use-package post)

#### simple-wiki<a id="orgheadline31"></a>

<http://www.emacswiki.org/emacs/SimpleWikiMode>

    (use-package simple-wiki)

#### wikidot mode<a id="orgheadline32"></a>

An Emacs mode for editing Wikidot markup 

<https://github.com/infochimps-customers/wikidot-mode>

    (use-package wikidot-mode)

### Part of emacs<a id="orgheadline39"></a>

#### printing<a id="orgheadline34"></a>

<http://www.emacswiki.org/emacs/PrintingPackage>

    (use-package printing
      :config
      (pr-update-menus t))

#### uniquify<a id="orgheadline35"></a>

Uniquify buffer names. 
See e.g. [here](http://trey-jackson.blogspot.cl/2008/01/emacs-tip-11-uniquify.html) or [here.](http://www.emacswiki.org/emacs/uniquify)

    (require 'uniquify)

#### Midnight Mode<a id="orgheadline36"></a>

<http://www.emacswiki.org/emacs/MidnightMode>

Bury unused buffers after some time (4:30 in the morning).

    (use-package midnight
      :config
      (midnight-delay-set 'midnight-delay "4:30am"))

#### Abbrev-mode<a id="orgheadline37"></a>

<http://www.emacswiki.org/emacs/AbbrevMode>

-   but currently not used

    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    (setq save-abbrevs t)  
    ;; in org-mode, we want expansions with trailing or leading slashes -
    ;; this might need some modification
    (abbrev-table-put org-mode-abbrev-table :regexp "\\(\\\\[a-z0-9@]+\\)")

#### ido-mode<a id="orgheadline38"></a>

<http://www.emacswiki.org/emacs/InteractivelyDoThings>
Part of Emacs

    (use-package ido
      :ensure t
      :config
      (ido-mode t)
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (setq ido-max-prospects 50)
      (setq ido-max-window-height 0.25)
      )

## User Interface<a id="orgheadline61"></a>

### Window Configuration<a id="orgheadline41"></a>

-   no tooltips
-   no toolbar
-   menu yes
-   scroll-bars yes

    (when window-system
      (tooltip-mode -1)
      (tool-bar-mode -1)
      (menu-bar-mode 1)
      (scroll-bar-mode 1))

### Various settings<a id="orgheadline58"></a>

#### move around between windows using C-S-Arrow keys (wind-move)<a id="orgheadline42"></a>

    (global-set-key (kbd "<C-S-up>")     'windmove-up)
    (global-set-key (kbd "<C-S-down>")   'windmove-down)
    (global-set-key (kbd "<C-S-left>")   'windmove-left)
    (global-set-key (kbd "<C-S-right>")  'windmove-right)

#### disable <menu>-key<a id="orgheadline43"></a>

    (global-set-key (kbd "<menu>") 'nil)

#### disable blinking cursor<a id="orgheadline44"></a>

    (blink-cursor-mode 0)

#### disable Shift+Arrow to select text<a id="orgheadline45"></a>

    (setq shift-select-mode nil)

#### middle-click pastes at point, not at mouse position<a id="orgheadline46"></a>

    (setq mouse-yank-at-point t)

#### transient-mark-mode<a id="orgheadline47"></a>

    (setq transient-mark-mode t)

#### highlight matching parenthesis based on point<a id="orgheadline48"></a>

    (show-paren-mode t)

#### recent files mode<a id="orgheadline49"></a>

    (recentf-mode 1)

#### Bind hippie-expand to M-<SPC> - Peace!<a id="orgheadline50"></a>

    (global-set-key "\M- " 'hippie-expand)

#### never truncate the lines in my buffer [DISABLED]<a id="orgheadline51"></a>

    (setq truncate-lines t)

#### always truncate lines, but never the mini-buffer<a id="orgheadline52"></a>

    (setq truncate-lines t)
    (add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines nil)))

#### Emacs close confirmation<a id="orgheadline53"></a>

(do not accidentally close emacs)

    (setq kill-emacs-query-functions
          (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
                kill-emacs-query-functions))

#### enable disabled command - upcase region<a id="orgheadline54"></a>

    (put 'upcase-region 'disabled nil)

#### desktop-save-mode<a id="orgheadline55"></a>

(see Sect. 42 "Saving Emacs Sessions" in Emacs
User Manual)

    (desktop-save-mode 1)
    (setq desktop-restore-eager 10)
    (setq desktop-save t) ;; save without asking

#### user ibuffer insted of list-buffers<a id="orgheadline56"></a>

    (defalias 'list-buffers 'ibuffer)

#### eshell-stuff<a id="orgheadline57"></a>

em-joc - not used anymore

     (require 'em-joc)
      (defun eshell/info (subject)
        "Read the Info manual on SUBJECT."
        (let ((buf (current-buffer)))
          (Info-directory)
          (let ((node-exists (ignore-errors (Info-menu subject))))
            (if node-exists
                0
    ;;          We want to switch back to *eshell* if the requested
    ;;          Info manual doesn't exist.
              (switch-to-buffer buf)
              (eshell-print (format "There is no Info manual on %s.\n"
                                    subject))
              1))))

### Electric Pairs<a id="orgheadline59"></a>

    (electric-pair-mode 1)
    (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
    (defun markdown-add-electric-pairs ()
      (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
      (setq-local electric-pair-text-pairs electric-pair-pairs))
    (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

### Move around between windows (wind-move)<a id="orgheadline60"></a>

Move around between windows using C-S-Arrow keys (wind-move). Better
than pressing repeatedly C-x o. (Seems not to work in org-mode?)

    (global-set-key (kbd "<C-S-up>")     'windmove-up)
    (global-set-key (kbd "<C-S-down>")   'windmove-down)
    (global-set-key (kbd "<C-S-left>")   'windmove-left)
    (global-set-key (kbd "<C-S-right>")  'windmove-right)

## Convenience functions not shipped in emacs<a id="orgheadline65"></a>

### Timestamps<a id="orgheadline62"></a>

Command to insert timestamps into text - e.g.: 27.10.2015, 12:25
Inspired from <http://emacswiki.org/emacs/InsertingTodaysDate>

    (defun timestamp ()
      (interactive)
      (insert (format-time-string "%d.%m.%Y, %H:%M")))

### Count Words & Characters<a id="orgheadline63"></a>

From <http://ergoemacs.org/emacs/elisp_count-region.html>

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

### Unfill Region / Unfill Paragraph<a id="orgheadline64"></a>

Source: <http://ergoemacs.org/emacs/emacs_unfill-paragraph.html>

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

## Mode-specific settings<a id="orgheadline68"></a>

AucTeX + org-mode are installed manually.

### LateX<a id="orgheadline66"></a>

    (setq LaTeX-math-menu-unicode t)
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
    (require 'reftex)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    (setq-default TeX-master nil)
    (setq reftex-cite-format 'natbib)

### Org-Mode<a id="orgheadline67"></a>

    (setq auto-mode-alist
          (cons '("\\.org$" . org-mode) auto-mode-alist))
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)
    (setq org-log-done t)
