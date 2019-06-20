(require 'package)
(add-to-list 'load-path "~/.emacs.d/lib/pullover/")
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package evil ; vim emulation
  :ensure t
  :init
  (setq evil-want-C-u-scroll t) ; chooses vim's CTRL-u
  (setq evil-symbol-word-search t) ; searches symbol (includes underscores and dashes etc) instead of word
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "M-.") nil) ; unset this so SLIME can use it
  (setq evil-vsplit-window-right t) ; --> gives vim's "split botright" behavior
  (setq evil-split-window-below t)  ; -^

  (use-package evil-indent-plus
    :ensure t)

  (use-package evil-org ; vim bindings in org mode
    :ensure t
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package slime ; lisp stuff
  :ensure t
  :config
  (if (string-equal system-type "darwin")
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq inferior-lisp-program "/usr/bin/sbcl"))
  (setq slime-contribs '(slime-fancy)))

(use-package helm ; narrowing auto-completion framework
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-split-window-inside-p t)  ; make helm split inside current window
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)))

(use-package helm-descbinds ; interactive binding description
  :ensure t
  :after helm
  :bind ("C-h b" . helm-descbinds))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(when (string-equal system-type "darwin")
  (use-package grab-mac-link
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c g") 'grab-mac-link)))))

(use-package all-the-icons ; icon pack
  :ensure t)

(use-package doom-themes ; theme framework
  :ensure t
  :after all-the-icons
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (if (string-equal system-type "darwin")
    (load-theme 'doom-nord t)
    (load-theme 'doom-dracula t)) ; load a different theme on linux, so I can tell where I am
  (doom-themes-org-config))

(use-package doom-modeline ; mode line theme
  :ensure t
  :defer t
  :after doom-themes
  :hook (after-init . doom-modeline-init)
  :config
  (set-face-foreground 'doom-modeline-evil-emacs-state "#2e3440")
  (set-face-background 'doom-modeline-evil-emacs-state "#a3be8c")
  (set-face-foreground 'doom-modeline-evil-insert-state "#2e3440")
  (set-face-background 'doom-modeline-evil-insert-state "#ebcb8b")
  (set-face-foreground 'doom-modeline-evil-motion-state "#2e3440")
  (set-face-background 'doom-modeline-evil-motion-state "plum3") ; don't even know what motion state is
  (set-face-foreground 'doom-modeline-evil-normal-state "#2e3440")
  (set-face-background 'doom-modeline-evil-normal-state "#b48ead")
  (set-face-foreground 'doom-modeline-evil-operator-state "#2e3440")
  (set-face-background 'doom-modeline-evil-operator-state "DarkGoldenrod2") ; or this either
  (set-face-foreground 'doom-modeline-evil-visual-state "#2e3440")
  (set-face-background 'doom-modeline-evil-visual-state "#4c566a")
  (set-face-foreground 'doom-modeline-evil-replace-state "#2e3440")
  (set-face-background 'doom-modeline-evil-replace-state "#88c0d0")

  (setq doom-modeline-height 40) ; make the mode line fatter
  (setq doom-modeline-icon t)    ; use icons as well as text
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t))

(use-package nyan-mode ; file position bar in mode line
  :ensure t
  :config
  (nyan-mode 1) ; turn on
  (setq nyan-wavy-trail t))

(use-package neotree ; file tree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme 'icons))

(use-package magit ; git integration
  :ensure t)

(when (string-equal system-type "darwin")
  (use-package pullover
    :config (server-start)
    :custom (pullover-clipboard-timeout 200)))

(use-package diff-hl ; highlight diffs in fringe using vcs info
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package highlight-indent-guides ; indent guides with highlight for current nest level
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character) ; use lines (actually pipe chars) instead of spaces
  (setq highlight-indent-guides-responsive 'top)   ; indicate nesting level
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package org-bullets ; prettifies org mode bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

(use-package rainbow-delimiters ; gives matching-color rainbow parens (and brackets etc)
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-line-numbers-type (quote relative))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org/inbox.org" "~/org/gtd.org")))
 '(package-selected-packages
   (quote
    (shell-pop grab-mac-link org-mac-link smooth-scrolling auto-complete highlight-indent-guides evil ##)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; CUSTOMIZATIONS
(setq-default indent-tabs-mode nil) ; death to tab chars!
(setq-default tab-width 4)
(evil-define-key 'insert prog-mode-map (kbd "TAB") 'tab-to-tab-stop) ; makes TAB insert spaces (rather than do nothing, or correct indentation)

; code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(evil-define-key 'normal prog-mode-map (kbd "<tab>") 'hs-toggle-hiding) ; makes TAB fold code in normal mode

(defun pop-shell (arg)
  "Pop a shell in a side window. Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'shell))
      (current-buffer))
    '((side . bottom)))))
(global-set-key "\C-c\p" 'pop-shell)
(setq server-window 'pop-to-buffer) ; related to shell--allows using "emacsclient <myfile> &" to open file in a split window
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ; enables ansi-color for shell output
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output) ; turns on ansi-color
(server-start) ; starts the emacs server (to allow 'emacsclient' to point somewhere)

; font preference--to standardize size between mac & linux (on my screen)
(if (string-equal system-type "darwin")
 (set-face-attribute 'default nil
                     :family "Menlo" :height 150 :weight 'normal)
 (set-face-attribute 'default nil
                     :family "Hack Nerd Font" :height 110 :weight 'normal))
(global-hl-line-mode t) ; highlight current line

; window splitting behavior
(setq split-width-threshold 100 ; only split to the right if we have 100 columns
      window-min-height 20)     ; make splits at least 20 lines high

; window resize bindings
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(winner-mode 1) ; allows using C-c left and C-c right to undo and redo (respectively) changes to window splits

(window-divider-mode) ; adds a divider between windows
(setq window-divider-default-right-width 5) ; 5 px shows enough fringe that diff-hl can properly show live diff info

; scroll settings
(setq scroll-conservatively 1000          ; don't try to auto-center the cursor bc that's annoying
      scroll-margin 7                     ; margin to maintain at top and bottom of window
      scroll-preserve-screen-position 't) ; keeps cursor at screen position when window size changes

; enable ESC to quit everything consistently (ref: https://stackoverflow.com/questions/8483182/evil-mode-best-practice/10166400#10166400)
; - this keeps ESC-ESC-ESC from screwing with the window layout
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

; evil keybindings for occur mode
(add-hook 'occur-mode-hook
	  (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/")       'evil-search-forward
              (kbd "n")       'evil-search-next
              (kbd "N")       'evil-search-previous
              (kbd "C-d")     'evil-scroll-down
              (kbd "C-u")     'evil-scroll-up
              (kbd "C-w C-w") 'other-window)))

; line numbering
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(setq display-line-numbers-type 'relative)     ; relative to current line
(setq display-line-numbers-current-absolute t) ; current line gets the absolute line num
(setq display-line-numbers-width 5)            ; 5 digits
(setq display-line-numbers-widen t)            ; keep enough space for all digits

; turn on column number
(column-number-mode 1)

; hooks to make line numbers go absolute in insert mode
(add-hook 'evil-insert-state-entry-hook
	  (lambda () (setq display-line-numbers t)))
(add-hook 'evil-insert-state-exit-hook
	  (lambda () (setq display-line-numbers 'relative)))

; visual line wrapping
; doing it this way prevents visual wrapping in the minibuffer
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'prog-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Make evil-mode up/down operate in screen lines (i.e., when a line is wrapped) instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

; give visual cue of active window
(setq cursor-in-non-selected-windows nil) ; remove cursor on inactive windows (so active one is more apparent)
(set-face-attribute 'mode-line ; emphasize active mode line with 3-d border
                    nil 
                    :box '(:line-width 2 :style released-button))
(set-face-attribute 'mode-line-inactive ; de-emphasize inactive mode line with dark foreground text
                    nil 
                    :foreground "#4c566a")

; GUI preference stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-appearance . dark))

; ---- org-mode ----
(setq org-directory "~/org")
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-tag-alist '((:startgrouptag)
                      ("LOCATION")
                        (:grouptags)
                        ("@HOME" . ?h)
                        ("@WORK" . ?w)
                        (:endgrouptag)
                      (:startgrouptag)
                      ("CONTEXT")
                        (:grouptags)
                        ("COMPUTER" . ?c)
                        ("READING" . ?r)
                        ("EXERCISE" . ?e)
                        ("PHONE" . ?p)
                      ))
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/inbox.org" "Actions")
         "** TODO %?\nCAPTURED: %U\n%a\n" :empty-lines 1)
        ("i" "idea" entry (file+headline "~/org/inbox.org" "Ideas")
         "** %?\nCAPTURED: %U\n%a\n :IDEA:" :empty-lines 1)
        ("m" "meeting" entry (file+headline "~/org/inbox.org" "Meetings")
         "** MEETING with %? :MEETING:\n   %T" :empty-lines 1)))

; org-mode refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 4))
      org-refile-use-outline-path 'file               ; use file-path-like completion
      org-outline-path-complete-in-steps nil          ; complete step-by-step
      org-refile-allow-creating-parent-nodes 'confirm ; refile can create new nodes (with confirmation)
      org-log-refile 'time)

;; shortcuts to org-mode views
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-hide-emphasis-markers t    ; hides the *bold* and /italic/ markers, etc
      org-deadline-warning-days 7    ; warn a week before deadlines
      org-return-follows-link t      ; open link with enter
      org-list-allow-alphabetical t  ; give alphabetical option
      org-tags-column 72             ; offset tags
      org-agenda-tags-column 72      ; yes in agenda too
      org-pretty-entities t          ; render LaTeX
      org-pretty-entities-include-sub-superscripts t ; yes even those
      org-archive-location "~/org/archived-tasks.org::datetree/" ; org-archived tasks go here
      org-log-states-order-reversed t) ; log state changes on TODO items in ascending date order

(add-hook 'org-mode-hook 'org-indent-mode)
; -----------------

;-------- STRIDE IN EMACS ---------
; Got this from Will; some changes made to correct paths
; To use: M-x stride<enter>
;don't foget you can use ielm to see how this is working!
(defun stride (&optional gui)
  (interactive);makes this defun a new command available in m-x
;first kill any current running lisp
  (when (slime-connected-p)
    (slime-quit-lisp)
    ;(slime-disconnect)
    ;(slime-kill-all-buffers)
  )
  (setenv "STRIDE_ROOT" "/home/parallels/STRIDE_project/STRIDE")
  (setenv "LD_LIBRARY_PATH" ;"/home/parallels/STRIDE_project/STRIDE/SolidModelInterface/Debug:/home/parallels/STRIDE_project/Parasolid/Parasolid/intel_linux/base/shared_object:/home/parallels/STRIDE_project/STRIDE/ModelRenderInterface/Debug:/home/parallels/STRIDE_project/HOOPS/HOOPS_Visualize/bin/linux64:/home/parallels/STRIDE_project/STEPTranslator/PSSTEP/Bin/Release/lx86_64_gcc4:/home/parallels/STRIDE_project/STEPTranslator/Common/Bin/Release/lx86_64_gcc4:/usr/lib64/:/usr/lib/:/home/parallels/STRIDE_project/STRIDE/CSFLib/:/home/parallels/STRIDE_project/STRIDE/fortran-stuff/");for some reason, just running ld_library_path.sh does not work.  could run that first and then load emacs in the same shell, but this is easy enough
"/home/parallels/STRIDE_project/STRIDE/cpp-code/SolidModelInterface/Debug:/home/parallels/STRIDE_project/Parasolid/Parasolid/intel_linux/base/shared_object:/home/parallels/STRIDE_project/STRIDE/cpp-code/ModelRenderInterface/Debug:/home/parallels/STRIDE_project/HOOPS/HOOPS_Visualize/bin/linux64:/home/parallels/STRIDE_project/STEPTranslator/PSSTEP/Bin/Release/lx86_64_gcc4:/home/parallels/STRIDE_project/STEPTranslator/Common/Bin/Release/lx86_64_gcc4:/usr/lib64/:/usr/lib/:/home/parallels/STRIDE_project/STRIDE/CSFLib/:/home/parallels/STRIDE_project/STRIDE/fortran-stuff/")
   (if gui
      (setenv "NOGUI" "nil")
      (setenv "NOGUI" "t"))
  (setq inferior-lisp-program
    (if (not gui)
      (if (string-equal system-type "darwin")
        "/usr/local/bin/sbcl --dynamic-space-size 20000 --no-sysinit --no-userinit --load /Users/kgeohagan/STRIDE_project/STRIDE/lisp/launch-emacs.lisp"
        "~/STRIDE_project/sbcl-install/bin/sbcl --dynamic-space-size 20000 --no-sysinit --no-userinit --load /home/parallels/STRIDE_project/STRIDE/lisp/launch-emacs.lisp")))
  (slime))

(defun stride-gui ()
  (interactive)
  (stride 'yes-gui-please))
;----------------------------------
