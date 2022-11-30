(require 'package)
(defvar gma-mode nil)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package elpy
  :ensure t
  :config
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

(use-package live-py-mode
  :ensure t)

(use-package origami
  :ensure t
  :config
  (global-origami-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq flycheck-check-syntax-automatically '(save)))
  ;;(require 'exec-path-from-shell)
  ;;(exec-path-from-shell-initialize))

(use-package meson-mode
  :ensure t
  :config
  (add-hook 'meson-mode-hook 'company-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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

  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install))

  (use-package evil-org ; vim bindings in org mode
    :ensure t
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package all-the-icons ; icon pack
  :ensure t)

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format t)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<ret>") 'ivy-alt-done))

(use-package counsel
  :ensure t
  :config (counsel-mode))

(use-package ivy-rich
  :ensure t
  :preface
  (setq ivy-rich-path-style 'abbrev)   ;; globally enables abbreviated paths in ivy-rich
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :init
  (setq ivy-rich--display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-find-file
          (:columns
           ((ivy-rich-candidate               (:width 60))
            (ivy-rich-file-user               (:width 4 :face font-lock-doc-face))
            (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
            (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
            (ivy-rich-file-size               (:width 6 :face font-lock-doc-face))
            (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy
  :ensure t
  :diminish
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

(use-package ivy-posframe
  :ensure t
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 4)))
  (setq ivy-posframe-width 200)
  (ivy-posframe-mode 1))

(use-package prescient ;; sort/filter for ivy/company
  :ensure t
  :diminish
  :config
  (prescient-persist-mode 1));; persists usage statistics across sessions

(use-package ivy-prescient
  :ensure t
  :diminish
  :custom
  (ivy-prescient-sort-commands
   '(:not ivy-switch-buffer
          swiper
          swiper-multi))
  :config
  (ivy-prescient-mode 1));; tells ivy to use prescient

(use-package swiper
  :ensure t
  :config
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil))
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit))

(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
  (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
  (setq parrot-rotate-dict
        '(
          (:rot ("first" "second" "third" "fourth" "fifth"))
          (:rot ("yes" "no"))
          (:rot ("True" "False"))
          (:rot ("and" "or"))
          (:rot ("prev" "next"))
          (:rot ("&&" "||"))
          (:rot ("==" "!=" "<" ">"))
          (:rot ("." "->"))
          (:rot ("get" "set"))
          (:rot ("else" "elif"))
          (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
          (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
          (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
          (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th")))))

(use-package slime ; lisp stuff
  :ensure t
  :config
  (if (string-equal system-type "darwin")
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq inferior-lisp-program "/usr/bin/sbcl"))
  (setq slime-contribs '(slime-fancy)))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package ivy-yasnippet
  :ensure t
  :config
  (evil-define-key 'visual prog-mode-map (kbd "C-s") 'ivy-yasnippet)
  (evil-define-key 'normal prog-mode-map (kbd "C-s") 'ivy-yasnippet))

;; c++ IDE features
(use-package projectile ;; project-awareness package
  :config
  (define-key evil-normal-state-map (kbd "` o f") 'projectile-find-other-file)
  (define-key evil-normal-state-map (kbd "` o F") 'projectile-find-other-file-other-window)
  (add-hook 'c++-mode-hook (projectile-mode +1))
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching 't)
  (setq projectile-completion-system 'ivy))
  
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp-deferred)
  :commands lsp lsp-deferred
  :config
  (setq lsp-file-watch-threshold 6000)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; completion engine
(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        :map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-c-headers
  :ensure t
  :config
  (push 'company-c-headers company-backends)
  (setq company-c-headers-path-user (list "~/Documents/_Projects/USAF_task/cave/include/")))

;; lsp-company interaction layer
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet 1))

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode))

;; c++ language server
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "c:/Users/KevinGeohagan/_Software/ccls-0.20201025/Release/ccls.exe"))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-executable "/usr/bin/clang-format")
  (fset 'c-indent-region 'clang-format-region)
  (evil-define-key 'visual prog-mode-map (kbd "=") 'indent-region))

(use-package clang-format+
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; debugger
;;(use-package dap-mode)
;; (use-package dap-c++)

(use-package doom-themes ; theme framework
  :ensure t
  :after all-the-icons
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (if (string-equal system-type "darwin") (load-theme 'doom-nord t))
  (if (string-equal system-type "gnu/linux") (load-theme 'doom-outrun-electric t)) ; load a different theme on linux, so I can tell where I am
  (if (string-equal system-type "windows-nt") (load-theme 'doom-one t))
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'extra-bold)
  (set-face-attribute 'font-lock-function-name-face nil :underline t)
  (set-face-attribute 'font-lock-variable-name-face nil :underline t)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  (doom-themes-org-config))

(use-package doom-modeline ; mode line theme
  :ensure t
  :defer t
  :after doom-themes
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 40) ; make the mode line fatter
  (setq doom-modeline-icon t)    ; use icons as well as text
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-buffer-state-icon t))

(use-package nyan-mode ; file position bar in mode line
  :ensure t
  :config
  (nyan-mode 1) ; turn on
  (setq nyan-wavy-trail t))

(use-package diff-hl ; highlight diffs in fringe using vcs info
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package magit ; git integration
  :ensure t
  :after diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook
  (emacs-lisp-mode . rainbow-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3B4252" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#ECEFF4"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "d5d2ab76985738c142adbe6a35dc51c8d15baf612fdf6745c901856457650314" "07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "777a3a89c0b7436e37f6fa8f350cbbff80bcc1255f0c16ab7c1e82041b06fccd" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "d5e13e2100e2a1167cd521558c5344809c154cc211ccd39a9c06bb83d254927c" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6e2d579b02aadc933f434003f49d269d004f5c7094eb53658afbacc817761d83" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(display-line-numbers-type 'relative)
 '(fci-rule-color "#4C566A")
 '(global-display-line-numbers-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(inhibit-startup-screen t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-prescient-sort-commands '(:not ivy-switch-buffer swiper swiper-multi))
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'full)
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#fb4934")
 '(org-agenda-files '("~/org/inbox.org" "~/org/gtd.org"))
 '(package-selected-packages
   '(matlab-mode golden-ratio evil-exchange macrostep shrink-path origami live-py-mode elpy ivy-yasnippet clang-format+ meson-mode minimap company-c-headers google-c-style all-the-icons-ivy org-starter-swiper counsel ivy-rich ivy ccls lsp-ui company-lsp company exec-path-from-shell flycheck lsp-mode jupyter shell-pop grab-mac-link org-mac-link smooth-scrolling auto-complete highlight-indent-guides evil ##))
 '(pdf-view-midnight-colors (cons "#ebdbb2" "#282828"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(pullover-clipboard-timeout 200)
 '(rustic-ansi-faces
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/mode-width (if (eq (powerline-current-separator) 'arrow) 'right 'full))
 '(sml/pos-id-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'powerline-active1 'powerline-active2)))
     (:propertize " " face powerline-active2)))
 '(sml/pos-minor-modes-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active1 'sml/global)))
     (:propertize " " face sml/global)))
 '(sml/pre-id-separator
   '(""
     (:propertize " " face sml/global)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'sml/global 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-minor-modes-separator
   '(""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active2 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#3B4252")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a15b66")
    (cons 300 "#825663")
    (cons 320 "#625160")
    (cons 340 "#4C566A")
    (cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; CUSTOMIZATIONS
;; ==== C++ stuff ====
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(setq-default c-basic-offset 4)    ; 4-space indent
(setq-default c-electric-flag nil) ; don't reformat while typing
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
;; ===================

(setq vc-follow-symlinks t) ; follow symlinks to version-controlled files by default
(show-paren-mode t)  ; shows matching parens
(global-auto-revert-mode) ; automatically revert buffers to what's on disk
(setq-default indent-tabs-mode nil) ; death to tab chars!
(setq-default tab-width 4)
;; python indentation
(add-hook 'python-mode-hook
    (lambda ()
       (setq indent-tabs-mode nil)
       (setq tab-width 4)))
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
;; ipython shell integration
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(setq lisp-indent-level 2)
(add-hook 'lisp-mode-hook
          '(lambda ()
             (setq evil-shift-width lisp-indent-level)))
(evil-define-key 'insert prog-mode-map (kbd "TAB") 'tab-to-tab-stop) ; makes TAB insert spaces (rather than do nothing, or correct indentation)
;; make :bd keep split (use :bq to kill both)
(evil-ex-define-cmd "bd" 'kill-this-buffer)
(evil-ex-define-cmd "bq" 'kill-buffer-and-window)

;; find/create buffers with switch-buffer
(define-key evil-ex-map "b " 'ivy-switch-buffer)
;; use counsel-find-file for :e (it's just better)
(define-key evil-ex-map "e " 'counsel-find-file)

; highlight parens
(setq show-paren-mode t)

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
                     :family "Menlo" :height 170 :weight 'normal))
(if (string-equal system-type "gnu/linux")
    (set-face-attribute 'default nil
                     :family "MesloLGS NF" :height 110 :weight 'normal))
(if (string-equal system-type "windows-nt")
    (set-face-attribute 'default nil
                     :family "Consolas" :height 110 :weight 'normal))
    
(global-hl-line-mode t) ; highlight current line

; window splitting behavior
(setq split-width-threshold 160 ; only split to the right if we have 160 columns
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

; make ESC actually quit things
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
(setq display-line-numbers-grow-only t)        ; avoids weird column jumping that occasionally happens

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

; explicitly set the wrap column
(setq-default fill-column 80)

;; Make evil-mode up/down operate in screen lines (i.e., when a line is wrapped) instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

;; Easy window/buffer nav
(define-key evil-normal-state-map (kbd "C-j") 'next-buffer)
(define-key evil-normal-state-map (kbd "C-k") 'previous-buffer)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "C-j") 'next-buffer)
(define-key evil-motion-state-map (kbd "C-k") 'previous-buffer)
(define-key evil-motion-state-map (kbd "C-S-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-S-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-S-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-S-l") 'evil-window-right)

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

(defun vim-set-nowrap ()
  "
  Provide wrap and unwrap via M-x because I can never remember the shitty emacs name.
  "
  (interactive)
  (set-default 'truncate-lines t))

(defun vim-set-wrap ()
  "
  Provide wrap and unwrap via M-x because I can never remember the shitty emacs name.
  "
  (interactive)
  (set-default 'truncate-lines nil))

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
