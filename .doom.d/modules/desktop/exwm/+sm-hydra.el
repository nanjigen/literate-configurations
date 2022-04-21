;;; desktop/exwm/+sm-hydra.el -*- lexical-binding: t; -*-
(pretty-hydra-define nanjigen/sm-hydra
  (:color red
   ;; :title (--hydra-title)
   :hint nil)
  ("Learn"
   (("rr" (exwm-sm-core-remember) "Remember")
    ("c" (exwm-sm-core-cancel-grade) "Undo grading")
    ("s" (nanjigen/sm-subset-hydra/body) "Subset operations")
    ("rs" (exwm-sm-core-set-read-point) "Set read point")
    ("m" (exwm-sm-core-mercy) "Mercy")
    ("S" (exwm-sm-core-sorting-criteria) "Sorting Criteria"))
  "Editing"
   (("i" (exwm-sm-core-italic) "italics")
    ("b" (exwm-sm-core-bold) "bold")
    ("q" (exwm-sm-core-edit-question) "Edit Question")
    ("a" (exwm-sm-core-edit-answer) "Edit Answer")
    ("n" (exwm-sm-core-edit-answer) "Edit Next Component")
    ("f" (exwm-sm-core-edit-file) "Edit File"))
  "Misc"
   (("SPC" (exwm-sm-core-sm-commander) "SuperMemo Commander")
    ("rc" (exwm-sm-core-repair-collection) "Repair Collection")
    ("rl" (exwm-sm-core-restore-layout) "Restore layout")
    ("A" (exwm-sm-core-statistics-analysis) "Stats window"))))

;;; desktop/exwm/+sm-hydra.el -*- lexical-binding: t; -*-
(pretty-hydra-define nanjigen/sm-subset-hydra
  (:color red
   ;; :title (--hydra-title)
   :hint nil)
  ("Learn"
   (("b" (exwm-sm-core-branch-learning) "Branch learning")
    ("r" (exwm-sm-core-random-learning) "Random learning")
    ("t" (exwm-sm-core-random-test) "Random test")
    ("n" (exwm-sm-core-neural) "Go neural")
    ("s" (exwm-sm-core-subset-learning) "Subset learning"))))
