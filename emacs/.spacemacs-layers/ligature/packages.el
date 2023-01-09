;; added to `ligature-packages'. Then, for each package PACKAGE:
;;   function `ligature/init-PACKAGE' to load and initialize the package.
;;   define the functions `ligature/pre-init-PACKAGE' and/or
;;   `ligature/post-init-PACKAGE' to customize the package as it is loaded.

(defconst ligature-packages
  '(
    (ligature :location (recipe
                          :fetcher github
                          :repo "mickeynp/ligature.el"))
    ))

(defun ligature/init-ligature ()
  (use-package ligatures
    :defer t
    :config
    (ligature-set-ligatures
     't
     '("<==>" "<!--" "~~>" "***" "||>" ":::" "::"
       "===" "==>" "=>>" "=<<" "=/=" "!==" "!!"
       ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>"
       "#_(" "..<" "..." "+++" "/==" "///" "_|_" "&&" "^=" "~="
       "~>" "**" "*>" "*/" "||" "|=" "|>" "|-"
       "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." "..." ".?" "+>" "++" "?:"
       "?=" "?." "??" "/*" "/=" "</" "/>" "//" "__"
       "//"))))

; if you want to set it only for a specific mode
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)
;;; packages.el ends here

