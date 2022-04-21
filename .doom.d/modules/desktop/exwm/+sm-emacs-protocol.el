(defun nanjigen/launch-sm-process ()
  "Launch SM as a process with 'start-process-shell-command'"
  (interactive)
  (start-process-shell-command
   "SuperMemo18"
   "*Supermemo18*"
   (combine-and-quote-strings '("env WINEPREFIX="
                                "/home/nanjigen/.local/share/wineprefixes/supermemo18"
                                " wine "
                                "/home/nanjigen/.local/share/wineprefixes/supermemo18/drive_c/SuperMemo/sm18.exe"
                                "") "\"")))

(defun nanjigen/launch-notepad-process ()
  "Launch SM as a process with 'start-process-shell-command'"
  (interactive)
  (start-process-shell-command
   "Notepad"
   "*notepad.exe*"
   (combine-and-quote-strings '("env WINEPREFIX="
                                "/home/nanjigen/.local/share/wineprefixes/supermemo18"
                                " wine "
                                "notepad.exe"
                                "") "\"")))
