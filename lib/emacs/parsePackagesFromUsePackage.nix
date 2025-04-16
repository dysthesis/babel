# From https://github.com/nix-community/emacs-overlay/blob/7236e0baef0a94fa7826f034ea96433224bcea89/parse.nix
# Get a list of packages declared wanted with `use-package` in the
# input string `config`. The goal is to only list packages that
# would be installed by `use-package` on evaluation; thus we look at
# the `:ensure` and `:disabled` keyword values to attempt to figure
# out which and whether the package should be installed.
#
# Example input:
#
# ''
#   (use-package org
#     :commands org-mode
#     :bind (("C-c a" . org-agenda)
#            :map org-mode-map
#            ([C-right] . org-demote-subtree)
#            ([C-left] . org-promote-subtree)))
#
#   (use-package direnv
#     :ensure t
#     :config (direnv-mode))
#
#   (use-package paredit-mode
#     :ensure paredit
#     :hook (emacs-lisp-mode lisp-mode lisp-interaction-mode))
# ''
# => [ "direnv" "paredit" ]
lib:
{
  configText,
  alwaysEnsure ? false,
  isOrgModeFile ? false,
  alwaysTangle ? false,
}:
let
  inherit (lib.babel.emacs.fromElisp)
    fromOrgModeBabelElisp'
    fromElisp
    ;
  readFunction =
    if isOrgModeFile then
      fromOrgModeBabelElisp' {
        ":tangle" = if alwaysTangle then "yes" else "no";
      }
    else
      fromElisp;

  find =
    item: list:
    if list == [ ] then
      [ ]
    else if builtins.head list == item then
      list
    else
      find item (builtins.tail list);

  getKeywordValue =
    keyword: list:
    let
      keywordList = find keyword list;
    in
    if keywordList != [ ] then
      let
        keywordValue = builtins.tail keywordList;
      in
      if keywordValue != [ ] then builtins.head keywordValue else true
    else
      null;

  isDisabled =
    item:
    let
      disabledValue = getKeywordValue ":disabled" item;
    in
    if disabledValue == [ ] then
      false
    else if builtins.isBool disabledValue then
      disabledValue
    else if builtins.isString disabledValue then
      true
    else
      false;

  getName =
    item:
    let
      ensureValue = getKeywordValue ":ensure" item;
      usePackageName = builtins.head (builtins.tail item);
    in
    if builtins.isString ensureValue then
      if lib.hasPrefix ":" ensureValue then usePackageName else ensureValue
    else if ensureValue || (ensureValue == null && alwaysEnsure) then
      usePackageName
    else
      [ ];

  recurse =
    item:
    if builtins.isList item && item != [ ] then
      let
        packageManager = builtins.head item;
      in
      if
        builtins.elem packageManager [
          "use-package"
          "leaf"
        ]
      then
        if !(isDisabled item) then
          [
            packageManager
            (getName item)
          ]
          ++ map recurse item
        else
          [ ]
      else
        map recurse item
    else
      [ ];
in
lib.flatten (map recurse (readFunction configText))
