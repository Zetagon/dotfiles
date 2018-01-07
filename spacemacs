    ;; -*- mode: emacs-lisp -*-
      ;; This file is loaded by Spacemacs at startup.
      ;; It must be stored in your home directory .

      (defun dotspacemacs/layers ()
        "Configuration Layers declaration.
    You should not put any user code in this function besides modifying the variable
    values."
        (setq-default
         ;; Base distribution to use. This is a layer contained in the directory
         ;; `+distribution'. For now available distributions are `spacemacs-base'
         ;; or `spacemacs'. (default 'spacemacs)
         dotspacemacs-distribution 'spacemacs
         ;; Lazy installation of layers (i.e. layers are installed only when a file
         ;; with a supported type is opened). Possible values are `all', `unused'
         ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
         ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
         ;; lazy install any layer that support lazy installation even the layers
         ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
         ;; installation feature and you have to explicitly list a layer in the
         ;; variable `dotspacemacs-configuration-layers' to install it.
         ;; (default 'unused)
         dotspacemacs-enable-lazy-installation 'unused
         ;; If non-nil then Spacemacs will ask for confirmation before installing
         ;; a layer lazily. (default t)
         dotspacemacs-ask-for-lazy-installation t
         ;; If non-nil layers with lazy install support are lazy installed.
         ;; List of additional paths where to look for configuration layers.
         ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
         dotspacemacs-configuration-layer-path '()
         ;; List of configuration layers to load.
         dotspacemacs-configuration-layers
         '(csv
           clojure
           (haskell :variables
                    haskell-completion-backend 'intero
                    haskell-enable-hindent-style "johan-tibell") 
           org-rifle
           org-brain
           lua
           lispy
           org
           rust
           yaml
           themes-megapack
           search-engine
           spotify
           ranger
           python
           javascript
           typescript
           html
           latex
           auto-completion
           xkcd
           c-c++
           MyLayer
           mu4e
            react
           ;; ----------------------------------------------------------------
           ;; Example of useful layers you may want to use right away.
           ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
           ;; <M-m f e R> (Emacs style) to install them.
           ;; ----------------------------------------------------------------
           helm
           ;; auto-completion
           ;; better-defaults
           emacs-lisp
           git
           markdown
           pdf-reference
           (shell :variables
                   shell-default-height 30
                   shell-default-position 'bottom)
           evil-commentary
           spell-checking
           syntax-checking
           version-control
           )
         evil-shift-round nil
         ;; List of additional packages that will be installed without being
         ;; wrapped in a layer. If you need some configuration for these
         ;; packages, then consider creating a layer. You can also put the
         ;; configuration in `dotspacemacs/user-config'.
         dotspacemacs-additional-packages '(scad-preview)
         ;; A list of packages that cannot be updated.
         dotspacemacs-frozen-packages '()
         ;; A list of packages that will not be installed and loaded.
         dotspacemacs-excluded-packages '(auto-complete)
         ;; Defines the behaviour of Spacemacs when installing packages.
         ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
         ;; `used-only' installs only explicitly used packages and uninstall any
         ;; unused packages as well as their unused dependencies.
         ;; `used-but-keep-unused' installs only the used packages but won't uninstall
         ;; them if they become unused. `all' installs *all* packages supported by
         ;; Spacemacs and never uninstall them. (default is `used-only')
         dotspacemacs-install-packages 'used-only))

      (defun dotspacemacs/init ()
        "Initialization function.
    This function is called at the very startup of Spacemacs initialization
    before layers configuration.
    You should not put any user code in there besides modifying the variable
    values."
        ;; This setq-default sexp is an exhaustive list of all the supported
        ;; spacemacs settings.
        (setq-default
         ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
         ;; possible. Set it to nil if you have no way to use HTTPS in your
         ;; environment, otherwise it is strongly recommended to let it set to t.
         ;; This variable has no effect if Emacs is launched with the parameter
         ;; `--insecure' which forces the value of this variable to nil.
         ;; (default t)
         dotspacemacs-elpa-https t
         ;; Maximum allowed time in seconds to contact an ELPA repository.
         dotspacemacs-elpa-timeout 5
         ;; If non nil then spacemacs will check for updates at startup
         ;; when the current branch is not `develop'. Note that checking for
         ;; new versions works via git commands, thus it calls GitHub services
         ;; whenever you start Emacs. (default nil)
         dotspacemacs-check-for-update nil
         ;; If non-nil, a form that evaluates to a package directory. For example, to
         ;; use different package directories for different Emacs versions, set this
         ;; to `emacs-version'.
         dotspacemacs-elpa-subdirectory nil
         ;; One of `vim', `emacs' or `hybrid'.
         ;; `hybrid' is like `vim' except that `insert state' is replaced by the
         ;; `hybrid state' with `emacs' key bindings. The value can also be a list
         ;; with `:variables' keyword (similar to layers). Check the editing styles
         ;; section of the documentation for details on available variables.
         ;; (default 'vim)
         dotspacemacs-editing-style 'vim
         ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
         dotspacemacs-verbose-loading nil
         ;; Specify the startup banner. Default value is `official', it displays
         ;; the official spacemacs logo. An integer value is the index of text
         ;; banner, `random' chooses a random text banner in `core/banners'
         ;; directory. A string value must be a path to an image format supported
         ;; by your Emacs build.
         ;; If the value is nil then no banner is displayed. (default 'official)
         dotspacemacs-startup-banner 'official
         ;; List of items to show in startup buffer or an association list of
         ;; the form `(list-type . list-size)`. If nil then it is disabled.
         ;; Possible values for list-type are:
         ;; `recents' `bookmarks' `projects' `agenda' `todos'."
         ;; List sizes may be nil, in which case
         ;; `spacemacs-buffer-startup-lists-length' takes effect.
         dotspacemacs-startup-lists '((agenda . 10)
                                      (bookmarks . 3))
         ;; True if the home buffer should respond to resize events.
         dotspacemacs-startup-buffer-responsive t
         ;; Default major mode of the scratch buffer (default `text-mode')
         dotspacemacs-scratch-mode 'text-mode
         ;; List of themes, the first of the list is loaded when spacemacs starts.
         ;; Press <SPC> T n to cycle to the next theme in the list (works great
         ;; with 2 themes variants, one dark and one light)
         dotspacemacs-themes '(sanityinc-tomorrow-bright 
                               zenburn)
         ;; If non nil the cursor color matches the state color in GUI Emacs.
         dotspacemacs-colorize-cursor-according-to-state t
         ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
         ;; quickly tweak the mode-line size to make separators look not too crappy.
         dotspacemacs-default-font '("Source Code Pro"
                                     :size 11
                                     :weight normal
                                     :width normal
                                     :powerline-scale 1.1)
         ;; The leader key
         dotspacemacs-leader-key "SPC"
         ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
         ;; (default "SPC")
         dotspacemacs-emacs-command-key "SPC"
         ;; The key used for Vim Ex commands (default ":")
         dotspacemacs-ex-command-key ":"
         ;; The leader key accessible in `emacs state' and `insert state'
         ;; (default "M-m")
         dotspacemacs-emacs-leader-key "M-m"
         ;; Major mode leader key is a shortcut key which is the equivalent of
         ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
         dotspacemacs-major-mode-leader-key ","
         ;; Major mode leader key accessible in `emacs state' and `insert state'.
         ;; (default "C-M-m")
         dotspacemacs-major-mode-emacs-leader-key "C-M-m"
         ;; These variables control whether separate commands are bound in the GUI to
         ;; the key pairs C-i, TAB and C-m, RET.
         ;; Setting it to a non-nil value, allows for separate commands under <C-i>
         ;; and TAB or <C-m> and RET.
         ;; In the terminal, these pairs are generally indistinguishable, so this only
         ;; works in the GUI. (default nil)
         dotspacemacs-distinguish-gui-tab nil
         ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
         dotspacemacs-remap-Y-to-y$ nil
         ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
         ;; there. (default t)
         dotspacemacs-retain-visual-state-on-shift t
         ;; If non-nil, J and K move lines up and down when in visual mode.
         ;; (default nil)
         dotspacemacs-visual-line-move-text nil
         ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
         ;; (default nil)
         dotspacemacs-ex-substitute-global nil
         ;; Name of the default layout (default "Default")
         dotspacemacs-default-layout-name "Default"
         ;; If non nil the default layout name is displayed in the mode-line.
         ;; (default nil)
         dotspacemacs-display-default-layout nil
         ;; If non nil then the last auto saved layouts are resume automatically upon
         ;; start. (default nil)
         dotspacemacs-auto-resume-layouts nil
         ;; Size (in MB) above which spacemacs will prompt to open the large file
         ;; literally to avoid performance issues. Opening a file literally means that
         ;; no major mode or minor modes are active. (default is 1)
         dotspacemacs-large-file-size 1
         ;; Location where to auto-save files. Possible values are `original' to
         ;; auto-save the file in-place, `cache' to auto-save the file to another
         ;; file stored in the cache directory and `nil' to disable auto-saving.
         ;; (default 'cache)
         dotspacemacs-auto-save-file-location 'cache
         ;; Maximum number of rollback slots to keep in the cache. (default 5)
         dotspacemacs-max-rollback-slots 5
         ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
         dotspacemacs-helm-resize nil
         ;; if non nil, the helm header is hidden when there is only one source.
         ;; (default nil)
         dotspacemacs-helm-no-header nil
         ;; define the position to display `helm', options are `bottom', `top',
         ;; `left', or `right'. (default 'bottom)
         dotspacemacs-helm-position 'bottom
         ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
         ;; in all non-asynchronous sources. If set to `source', preserve individual
         ;; source settings. Else, disable fuzzy matching in all sources.
         ;; (default 'always)
         dotspacemacs-helm-use-fuzzy 'always
         ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
         ;; several times cycle between the kill ring content. (default nil)
         dotspacemacs-enable-paste-transient-state nil
         ;; Which-key delay in seconds. The which-key buffer is the popup listing
         ;; the commands bound to the current keystroke sequence. (default 0.4)
         dotspacemacs-which-key-delay 0.4
         ;; Which-key frame position. Possible values are `right', `bottom' and
         ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
         ;; right; if there is insufficient space it displays it at the bottom.
         ;; (default 'bottom)
         dotspacemacs-which-key-position 'bottom
         ;; If non nil a progress bar is displayed when spacemacs is loading. This
         ;; may increase the boot time on some systems and emacs builds, set it to
         ;; nil to boost the loading time. (default t)
         dotspacemacs-loading-progress-bar t
         ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
         ;; (Emacs 24.4+ only)
         dotspacemacs-fullscreen-at-startup nil
         ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
         ;; Use to disable fullscreen animations in OSX. (default nil)
         dotspacemacs-fullscreen-use-non-native nil
         ;; If non nil the frame is maximized when Emacs starts up.
         ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
         ;; (default nil) (Emacs 24.4+ only)
         dotspacemacs-maximized-at-startup nil
         ;; A value from the range (0..100), in increasing opacity, which describes
         ;; the transparency level of a frame when it's active or selected.
         ;; Transparency can be toggled through `toggle-transparency'. (default 90)
         dotspacemacs-active-transparency 90
         ;; A value from the range (0..100), in increasing opacity, which describes
         ;; the transparency level of a frame when it's inactive or deselected.
         ;; Transparency can be toggled through `toggle-transparency'. (default 90)
         dotspacemacs-inactive-transparency 90
         ;; If non nil show the titles of transient states. (default t)
         dotspacemacs-show-transient-state-title t
         ;; If non nil show the color guide hint for transient state keys. (default t)
         dotspacemacs-show-transient-state-color-guide t
         ;; If non nil unicode symbols are displayed in the mode line. (default t)
         dotspacemacs-mode-line-unicode-symbols t
         ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
         ;; scrolling overrides the default behavior of Emacs which recenters point
         ;; when it reaches the top or bottom of the screen. (default t)
         dotspacemacs-smooth-scrolling t
         ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
         ;; derivatives. If set to `relative', also turns on relative line numbers.
         ;; (default nil)
         dotspacemacs-line-numbers nil
         ;; Code folding method. Possible values are `evil' and `origami'.
         ;; (default 'evil)
         dotspacemacs-folding-method 'evil
         ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
         ;; (default nil)
         dotspacemacs-smartparens-strict-mode nil
         ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
         ;; over any automatically added closing parenthesis, bracket, quote, etc…
         ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
         dotspacemacs-smart-closing-parenthesis nil
         ;; Select a scope to highlight delimiters. Possible values are `any',
         ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
         ;; emphasis the current one). (default 'all)
         dotspacemacs-highlight-delimiters 'all
         ;; If non nil, advise quit functions to keep server open when quitting.
         ;; (default nil)
         dotspacemacs-persistent-server nil
         ;; List of search tool executable names. Spacemacs uses the first installed
         ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
         ;; (default '("ag" "pt" "ack" "grep"))
         dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
         ;; The default package repository used if no explicit repository has been
         ;; specified with an installed package.
         ;; Not used for now. (default nil)
         dotspacemacs-default-package-repository nil
         ;; Delete whitespace while saving buffer. Possible values are `all'
         ;; to aggressively delete empty line and long sequences of whitespace,
         ;; `trailing' to delete only the whitespace at end of lines, `changed'to
         ;; delete only whitespace for changed lines or `nil' to disable cleanup.
         ;; (default nil)
         dotspacemacs-whitespace-cleanup nil
         ))

      (defun dotspacemacs/user-init ()
        "Initialization function for user code.
    It is called immediately after `dotspacemacs/init', before layer configuration
    executes.
     This function is mostly useful for variables that need to be set
    before packages are loaded. If you are unsure, you should try in setting them in
    `dotspacemacs/user-config' first."
        (setq-default evil-escape-key-sequence "jk")

        "Add functionality to org
       so that :ignore: tag can be used on org-headlines. Those headlines will not be exported
       but their subtrees will"
        ;; (require 'ox-extra)
        ;; (ox-extras-activate '(ignore-headlines))
        (setq-default dotspacemacs-line-numbers 'relative)

        "Set indentation to 4 spaces"
        (setq-default indent-tabs-mode nil)
        (setq-default save-place t)
        (setq-default tab-width 2)
        (setq indent-line-function 'insert-tab)
        (setq-default
         ;;js2-mode
         js2-basic-offset 2
         ;;web-mode
         css-indent-offset 2
         web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-attr-indent-offset 2)
        (add-hook 'js2-mode-hook  (function (lambda()
                                             (setq evil-shift-width 2)
                                             )))
    (defun hide-org-radio-targets ()
      (defcustom org-hidden-links-additional-re "\\(<<<\\)[[:print:]]+\\(>>>\\)"
        "Regular expression that matches strings where the invisible-property of the sub-matches 1 and 2 is set to org-link."
        :type '(choice (const :tag "Off" nil) regexp)
        :group 'org-link)
      (make-variable-buffer-local 'org-hidden-links-additional-re)

      (defun org-activate-hidden-links-additional (limit)
        "Put invisible-property org-link on strings matching `org-hide-links-additional-re'."
        (if org-hidden-links-additional-re
            (re-search-forward org-hidden-links-additional-re limit t)
          (goto-char limit)
          nil))

      (defun org-hidden-links-hook-function ()
        "Add rule for `org-activate-hidden-links-additional' to `org-font-lock-extra-keywords'.
      You can include this function in `org-font-lock-set-keywords-hook'."
        (add-to-list 'org-font-lock-extra-keywords
                                    '(org-activate-hidden-links-additional
                                      (1 '(face org-target invisible org-link))
                      (2 '(face org-target invisible org-link)))))


      (add-hook 'org-font-lock-set-keywords-hook #'org-hidden-links-hook-function))

      (add-hook 'latex-mode-hook '(lambda ()
                                  outline-minor-mode))
      (add-hook 'text-mode-hook (lambda() linum-relative-global-mode nil))
      (add-hook 'prog-mode-hook 'linum-relative-global-mode)
      (add-hook 'prog-mode-hook 'linum-relative-global-mode)
      (add-hook 'web-mode-hook 'electric-pair-mode)
      (hide-org-radio-targets)
          (setq
           auto-completion-return-key-behavior nil
           auto-completion-tab-key-behavior 'complete)

    (setq cider-cljs-lein-repl  "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
      )

      (defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files (quote ("~/org/skola.org" "~/org/todo.org")))
 '(org-babel-load-languages (quote ((awk . t) (C . t))))
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-tags-column -90)
 '(package-selected-packages
   (quote
    (zenburn-theme white-sand-theme toml-mode symon string-inflection scad-preview scad-mode sayid rebecca-theme realgud test-simple loc-changes load-relative racer password-generator overseer org-ref pdf-tools key-chord tablist org-category-capture org-brain nameless lispyville lispy zoutline swiper ivy intero interleave impatient-mode hlint-refactor hindent helm-purpose window-purpose imenu-list helm-org-rifle helm-hoogle helm-bibtex biblio parsebib biblio-core haskell-snippets google-c-style git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-rust flycheck-haskell exotica-theme evil-org ghub let-alist evil-lion engine-mode editorconfig diff-hl dante csv-mode company-lua lua-mode company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode cmake-ide levenshtein clojure-snippets clojure-cheatsheet clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode chronos cargo rust-mode browse-at-remote org-mime zonokai-theme zen-and-art-theme yaml-mode xterm-color xkcd winum underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spotify powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slime-company slime shell-pop seti-theme reverse-theme ranger railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme spinner orgit organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term mu4e-maildirs-extension mu4e-alert ht monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme skewer-mode simple-httpd light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme parent-mode heroku-theme hemisu-theme helm-spotify multi helm-company helm-c-yasnippet hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme fuzzy flycheck-pos-tip pos-tip epl flx flatui-theme flatland-theme firebelly-theme farmhouse-theme iedit evil-commentary espresso-theme eshell-z eshell-prompt-extras esh-help dracula-theme django-theme disaster darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-statistics company-c-headers company-auctex company-anaconda company common-lisp-snippets color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode clues-theme clang-format cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map badwolf-theme auto-yasnippet auctex apropospriate-theme anti-zenburn-theme pythonic s ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete popup which-key web-mode use-package toc-org tide typescript-mode flycheck restart-emacs pug-mode persp-mode org-plus-contrib org-download neotree move-text mmm-mode markdown-toc markdown-mode live-py-mode link-hint json-mode js2-refactor yasnippet info+ indent-guide hungry-delete highlight-indentation hide-comnt help-fns+ helm-projectile helm-make helm-gitignore helm-flx helm-ag git-timemachine git-link eyebrowse expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-mc evil-ediff evil-anzu dumb-jump diminish coffee-mode auto-compile packed anaconda-mode aggressive-indent ace-window ace-link avy smartparens highlight evil flyspell-correct helm helm-core magit magit-popup git-commit with-editor async projectile hydra f haml-mode js2-mode alert log4e request dash spacemacs-theme yapfify ws-butler window-numbering web-beautify volatile-highlights vi-tilde-fringe uuidgen undo-tree tagedit spaceline smeargle slim-mode slack scss-mode sass-mode rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort popwin pkg-info pip-requirements pcre2el paradox org-projectile org-present org-pomodoro org-bullets open-junk-file multiple-cursors magit-gitflow macrostep lorem-ipsum livid-mode linum-relative less-css-mode json-snatcher json-reformat js-doc ido-vertical-mode hy-mode htmlize hl-todo highlight-parentheses highlight-numbers helm-themes helm-swoop helm-pydoc helm-mode-manager helm-descbinds helm-css-scss goto-chg google-translate golden-ratio gnuplot gntp gitignore-mode gitconfig-mode gitattributes-mode git-messenger gh-md flyspell-correct-helm flx-ido fill-column-indicator fancy-battery evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args eval-sexp-fu emmet-mode elisp-slime-nav define-word cython-mode column-enforce-mode clean-aindent-mode bind-key auto-highlight-symbol auto-dictionary auctex-latexmk anzu adaptive-wrap ace-jump-helm-line)))
 '(paradox-github-token t)
 '(quote
   (org-file-apps
    (quote
     ((auto-mode . emacs)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . "mupdf %s")))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
) 
(defun dotspacemacs/user-config ()
        "Configuration function for user code.
    This function is called at the very end of Spacemacs initialization after
    layers configuration.
    This is the place where most of your configurations should be done. Unless it is
    explicitly specified that a variable should be set before a package is loaded
    you should place your code here."

                                        ;copied from the internet
        (defun increment-number-at-point ()
          (interactive)
          (skip-chars-backward "0-9")
          (or (looking-at "[0-9]+")
              (error "No number at point"))
          (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))) (require 'org)
        (global-set-key (kbd "C-c +") 'increment-number-at-point)

        (define-key global-map "\C-cl" 'org-store-link)
        (define-key global-map "\C-ca" 'org-agenda)
        (setq org-log-done t)
        (setq org-agenda-files '( "~/Dropbox/org/skola.org" "~/Dropbox/org/todo.org" "~/Dropbox/org/ifft_capture.org" "~/Dropbox/org/begrepp.org" "~/Dropbox/org/references/articles.org"))
        (setq org-refile-targets (quote (("~/org/skola.org" :maxlevel . 1)
                                         ("~/org/todo.org" :level . 1)
                                         ("~/org/brain/programming.org" :level . 1)
                                         ("~/org/someday.org" :maxlevel . 1))))

        "Org todo keywords"
        (setq org-todo-keywords
              '((sequence "SOMEDAY""TODO" "IN PROGRESS"  "|" "DONE" "CANCELLED")))

        (setq org-capture-templates
              '(("l" "Ny läxa" entry
                 (file "skola.org")
                 "* Läxa %?
    SCHEDULED: %t")
                ("t" "TODO" entry (file+headline "~/org/todo.org" "Tasks")
                 "* TODO %?
   Added: %T "
                 )
                ("r" "To Read" entry (file+headline "~/org/someday.org" "To Read")
                 "* %?
                 added: %t
                 "
                 )
                ("b" "Nytt begrepp" entry
                 (file "begrepp.org")
                 "* begrepp 
%?
** Answer")
                ))

        (setq bookmark-default-file "~/Dropbox/emacsbookmarks")
          ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
              ;; (add-to-list 'projectile-globally-ignored-directories "data")

        "Remap j and k to function with softwraps"
        (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
        (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
        (define-key evil-normal-state-map (kbd "Ö") 'evil-ex)
        (define-key evil-normal-state-map (kbd "¤") (kbd "$"))
        (define-key evil-normal-state-map (kbd "\'") 'evil-goto-mark)
        (define-key evil-normal-state-map (kbd ",;r") 'jump-to-register)
        (set-register ?I '(file . "~/Dropbox/org/skola.org" ))
        (add-hook 'text-mode-hook 'auto-fill-mode)
        (add-hook 'prog-mode-hook (lambda () (spacemacs/toggle-spelling-checking-off)))
  
        
          ;;Mu4e

        (setq mu4e-maildir "~/.mail"
              mu4e-trash-folder "/Trash"
              mu4e-refile-folder "/Archive"
              mu4e-attachment-dir "/Downloads"
              mu4e-get-mail-command "offlineimap"
              mu4e-update-interval nil
              mu4e-compose-signature-auto-include nil
              mu4e-view-show-images t
              mu4e-view-show-addresses t)

;;; Mail directory shortcuts
        (setq mu4e-maildir-shortcuts
              '( ("/INBOX"               . ?i)
                 ("/[Gmail].Sent Mail"   . ?s)
                 ("/[Gmail].Trash"       . ?t)
                 ("/[Gmail].All Mail"    . ?a)))

;;; Bookmarks
        (setq mu4e-bookmarks
              `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                ("date:today..now" "Today's messages" ?t)
                ("date:7d..now" "Last 7 days" ?w)
                ("mime:image/*" "Messages with images" ?p)
                (,(mapconcat 'identity
                             (mapcar
                              (lambda (maildir)
                                (concat "maildir:" (car maildir)))
                              mu4e-maildir-shortcuts) " OR ")
                 "All inboxes" ?i)))
          ;;end mu4e
                                        ; end Org Capture
        (setq bibtex-dialect 'biblatex)
        (setq shell-default-shell 'ansi-term)
        (setq evil-move-cursor-back t)
        ;; (projectile-register-project-type 'npm '("package.json")
        ;;                                   :compile "npm build"
        ;;                                   :test "npm test"
        ;;                                   :run "npm start"
        ;;                                   :test-suffix ".test")
        (defun node-run-tests()
          "Run test"
          (interactive)
          (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:tests" "npm test\n"))

        (defun reload-browser ()
          "Reload the browser by using xdotool
            Do this by sending M-3 , sleep 0.5 seconds and then send <F5>"
          (interactive)
              ;; (call-process "xdotool" nil nil nil "key" "Super_L+3")
          (call-process "awesome-client" nil nil nil "require('awful').screen.focused().tags[3]:view_only()")
          (call-process "sleep" nil nil nil "0.7")
          (call-process "xdotool" nil nil nil "key" "F5"))

        (defun node-restart-server()
          "Restart server that is running inside tmux"
          (interactive)
          (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:server" "C-c")
          (call-process "sleep" nil nil nil "0.1")
          (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:server" " node app.js" "enter"))

        (defun node-reload-server-and-browser ()
          (interactive)
          (node-restart-server)
          (reload-browser))

        (spacemacs/set-leader-keys "ob" 'reload-browser)
        (spacemacs/set-leader-keys "os" 'node-restart-server)
        (spacemacs/set-leader-keys "or" 'node-reload-server-and-browser)
        (spacemacs/set-leader-keys "ot" 'node-run-tests)
        (push '(javascript-docs
                :name "Javascript")
              search-engine-alist)
        (defengine javascript-docs
          "http://devdocs.io/#q=javascript %s"
          :docstring "Search devdocs with javascript tag")


        (global-company-mode)
        (setq-default TeX-master nil)
        (spacemacs/set-leader-keys "tt" 'hl-todo-mode)
      
        (require 'org-drill))

        (require 'ansi-color)
        (defun display-ansi-colors ()
          (interactive)
          (ansi-color-apply-on-region (point-min) (point-max)))

    ;; Do not write anything past this comment. This is where Emacs will
    ;; auto-generate custom variable definitions.
    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bibtex-completion-cite-commands
   (quote
    ("cite" "Cite" "parencite" "Parencite" "footcite" "footcitetext" "textcite" "Textcite" "smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite" "Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor" "citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear" "citeyear*" "citedate" "citedate*" "citeurl" "nocite" "fullcite" "footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite" "fnotecite" "textcquote" "blockcquote")))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files (quote ("~/org/skola.org" "~/org/todo.org")))
 '(org-babel-load-languages (quote ((awk . t) (C . t))))
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-tags-column -90)
 '(package-selected-packages
   (quote
    (lispy helm-org-rifle org-brain org-mime zonokai-theme zen-and-art-theme yaml-mode xterm-color xkcd winum underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spotify powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slime-company slime shell-pop seti-theme reverse-theme ranger railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme spinner orgit organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term mu4e-maildirs-extension mu4e-alert ht monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme skewer-mode simple-httpd light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme parent-mode heroku-theme hemisu-theme helm-spotify multi helm-company helm-c-yasnippet hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme fuzzy flycheck-pos-tip pos-tip epl flx flatui-theme flatland-theme firebelly-theme farmhouse-theme iedit evil-commentary espresso-theme eshell-z eshell-prompt-extras esh-help dracula-theme django-theme disaster darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-statistics company-c-headers company-auctex company-anaconda company common-lisp-snippets color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode clues-theme clang-format cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map badwolf-theme auto-yasnippet auctex apropospriate-theme anti-zenburn-theme pythonic s ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete popup which-key web-mode use-package toc-org tide typescript-mode flycheck restart-emacs pug-mode persp-mode org-plus-contrib org-download neotree move-text mmm-mode markdown-toc markdown-mode live-py-mode link-hint json-mode js2-refactor yasnippet info+ indent-guide hungry-delete highlight-indentation hide-comnt help-fns+ helm-projectile helm-make helm-gitignore helm-flx helm-ag git-timemachine git-link eyebrowse expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-mc evil-ediff evil-anzu dumb-jump diminish coffee-mode auto-compile packed anaconda-mode aggressive-indent ace-window ace-link avy smartparens highlight evil flyspell-correct helm helm-core magit magit-popup git-commit with-editor async projectile hydra f haml-mode js2-mode alert log4e request dash spacemacs-theme yapfify ws-butler window-numbering web-beautify volatile-highlights vi-tilde-fringe uuidgen undo-tree tagedit spaceline smeargle slim-mode slack scss-mode sass-mode rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort popwin pkg-info pip-requirements pcre2el paradox org-projectile org-present org-pomodoro org-bullets open-junk-file multiple-cursors magit-gitflow macrostep lorem-ipsum livid-mode linum-relative less-css-mode json-snatcher json-reformat js-doc ido-vertical-mode hy-mode htmlize hl-todo highlight-parentheses highlight-numbers helm-themes helm-swoop helm-pydoc helm-mode-manager helm-descbinds helm-css-scss goto-chg google-translate golden-ratio gnuplot gntp gitignore-mode gitconfig-mode gitattributes-mode git-messenger gh-md flyspell-correct-helm flx-ido fill-column-indicator fancy-battery evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args eval-sexp-fu emmet-mode elisp-slime-nav define-word cython-mode column-enforce-mode clean-aindent-mode bind-key auto-highlight-symbol auto-dictionary auctex-latexmk anzu adaptive-wrap ace-jump-helm-line)))
 '(paradox-github-token t)
 '(quote
   (org-file-apps
    (quote
     ((auto-mode . emacs)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . "mupdf %s")))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
    
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

