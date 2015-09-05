;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     emacs-lisp
     (git :variables
          git-magit-status-fullscreen t
          git-enable-github-support t
          git-gutter-use-fringe t)
     markdown
     org
     semantic
     shell
     python
     html
     javascript
     c-c++
     lua
     syntax-checking
     version-control
     (chinese :variables chinese-default-input-method 'pinyin
              chinese-enable-youdao-dict t)
     ;; my layer definition
     zilongshanren
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape
                                    ;; remove mode for git layer
                                    magit-gh-pulls
                                    magit-gitflow
                                    magit-svn
                                    smeargle
                                    ;; remove mode for python layer
                                    nose
                                    pony-mode
                                    hy-mode
                                    )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged.
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven           
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced.
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses."
   dotspacemacs-helm-resize nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one).
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (global-linum-mode 1) ; always show line numbers

  (setq powerline-default-separator 'arrow)
  (menu-bar-mode t)
  (setq vc-follow-symlinks t)
  ;; 设置中文等宽字体
  
  (setq default-input-method 'eim-wb)
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil :font
                        (format   "%s:pixelsize=%d"  english english-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size))))

  (set-font   "Source Code Pro" "Hiragino Sans GB" 14 16)

  (when (eq system-type 'darwin) ;; mac specific settings
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (setq default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    )

  ;; add helm everywhere
  (global-set-key (kbd "M-t") 'helm-ls-git-ls)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h f") 'helm-apropos)
  (global-set-key (kbd "C-h r") 'helm-info-emacs)
  (global-set-key (kbd "C-h C-l") 'helm-locate-library)
  (global-set-key (kbd "C-c f") 'helm-recentf)

  (eval-after-load 'company
    '(progn
       (setq company-echo-delay 0)
       (setq company-idle-delay 0.08)
       (setq company-auto-complete nil)
       (setq company-show-numbers t)
       (setq company-begin-commands '(self-insert-command))
       (setq company-tooltip-limit 10)
       (setq company-minimum-prefix-length 1)
       (let ((map company-active-map))
         (define-key map (kbd "C-d") 'company-show-doc-buffer)
         (define-key map (kbd "C-n") 'company-select-next)
         (define-key map (kbd "C-p") 'company-select-previous)
         (define-key map (kbd "C-l") 'company-complete-selection))
       ))

  (eval-after-load 'company
    '(progn
       (let ((map company-active-map))
         (define-key map (kbd "C-n") 'company-select-next)
         (define-key map (kbd "C-p") 'company-select-previous)))
    )

  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --smart-case")
   '(helm-ag-command-option "--all-text")
   '(helm-ag-insert-at-point 'symbol))


  ;; Define indention key
;;; It's kind of sad this doesn't exist normally...
  (defun indent-rigidly-n (n)
    "Indent the region, or otherwise the current line, by N spaces."
    (let* ((use-region (and transient-mark-mode mark-active))
           (rstart (if use-region (region-beginning) (point-at-bol)))
           (rend   (if use-region (region-end)       (point-at-eol)))
           (deactivate-mark "irrelevant")) ; avoid deactivating mark
      (indent-rigidly rstart rend n)))
  (defun indent-rigidly-4 ()
    "Indent the region, or otherwise the current line, by 4 spaces."
    (interactive)
    (indent-rigidly-n 4))
  (defun outdent-rigidly-4 ()
    "Indent the region, or otherwise the current line, by -4 spaces."
    (interactive)
    (indent-rigidly-n -4))

  (global-set-key (kbd "M-]") 'indent-rigidly-4)
  (global-set-key (kbd "M-[") 'outdent-rigidly-4)


  ;; 有关大小写p的区别.
  ;; 小写的p, 总是将任意的参数转换为一个有意义的数字.即使不指定参数, 即参数为nil, 默认代表1.
  ;; 大写的p, 除非显式的通过C-u或其他方式指定参数, 否则所有的nil, 当作无参数处理.
  (defun window-move-up (&optional arg)
    "Current window move-up 3 lines."
    (interactive "P")
    (if (region-active-p)
        (next-line nil)
      (if arg
          (scroll-up arg)
        (scroll-up 3))))
  (defun window-move-down (&optional arg)
    "Current window move-down 3 lines."
    (interactive "P")
    (if (region-active-p)
        (previous-line nil)
      (if arg
          (scroll-down arg)
        (scroll-down 3))))

  (global-set-key (kbd "M-p") 'window-move-down) ;光标位置不变，窗口向下移动两行
  (global-set-key (kbd "M-n") 'window-move-up)

  (eval-after-load 'anaconda-mode
    '(progn
       (define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-goto-definitions)
       (define-key anaconda-mode-map (kbd "M-,") 'anaconda-nav-pop-marker)
       (define-key anaconda-mode-map (kbd "C-c d") 'anaconda-mode-view-doc)
       ))

  ;; Comment and Uncomment
  (defun comment-or-uncomment-line-or-region ()
    "Comments or uncomments the current line or region."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      )
    )
  (global-set-key (kbd "M-/") 'comment-or-uncomment-line-or-region)
  (global-set-key (kbd "<C-return>") 'yas-expand)
  (setq helm-ag-always-set-extra-option t)

  (eval-after-load 'lua-mode
    '(progn
       (setq lua-indent-level 4)
       ))

  )



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(helm-ag-base-command "ag --nocolor --nogroup --smart-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point (quote symbol))
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval setenv "PYTHONPATH" "../common")
     ))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
