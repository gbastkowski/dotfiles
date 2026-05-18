{ pkgs, ... }:
let
  ploverDir = if pkgs.stdenv.isDarwin
    then "Library/Application Support/plover"
    else ".config/plover";
in
{
  home.file."${ploverDir}/plover.cfg".source = ./plover/plover.cfg;
  home.file."${ploverDir}/main.json".source = ./plover/main.json;
  home.file."${ploverDir}/user.json".source = ./plover/user.json;
  home.file."${ploverDir}/commands.json".source = ./plover/commands.json;
  home.file."${ploverDir}/lapwing-base.json".source = ./plover/lapwing-base.json;
  home.file."${ploverDir}/lapwing-commands.json".source = ./plover/lapwing-commands.json;
  home.file."${ploverDir}/lapwing-movement.modal".source = ./plover/lapwing-movement.modal;
  home.file."${ploverDir}/lapwing-numbers.json".source = ./plover/lapwing-numbers.json;
  home.file."${ploverDir}/lapwing-proper-nouns.json".source = ./plover/lapwing-proper-nouns.json;
  home.file."${ploverDir}/lapwing-uk-additions.json".source = ./plover/lapwing-uk-additions.json;
  home.file."${ploverDir}/abby-left-hand-modifiers.py".source = ./plover/abby-left-hand-modifiers.py;
  home.file."${ploverDir}/emily-modifiers.py".source = ./plover/emily-modifiers.py;
  home.file."${ploverDir}/emily-symbols.py".source = ./plover/emily-symbols.py;
  home.file."${ploverDir}/jeff-phrasing.py".source = ./plover/jeff-phrasing.py;
}
