;;; tools/ai/config.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Voice"       "r"    #'whisper-run)
(setq! whisper-language "de")
