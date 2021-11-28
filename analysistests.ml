open Datastructures

let liveness_analysis_tests =
  [ "llprograms/analysis8.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis7.ll", lblm[
      "_entry", uids[]
      ; "body", uids["1"; "4"]
      ; "end", uids["1"]
      ; "guard", uids["1"; "4"]
    ]
  ; "llprograms/analysis5.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis4.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["2"; "3"]
      ; "l2", uids["3"]
    ]
  ; "llprograms/analysis3.ll", lblm[
      "_entry", uids[]
      ; "l2", uids["1"]
      ; "l3", uids["2"]
      ; "l4", uids["3"]
      ; "l5", uids["4"]
      ; "l6", uids["5"]
      ; "l7", uids["6"]
      ; "l8", uids["7"]
      ; "l9", uids["8"]
      ; "lexit", uids["9"]
    ]
  ; "llprograms/analysis2.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["1"; "3"]
      ; "l2", uids[]
    ]
  ; "llprograms/analysis19.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis15.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis14.ll", lblm[
      "_entry", uids[]
    ]
  ; "llprograms/analysis11.ll", lblm[
      "_entry", uids[]
      ; "foo", uids[]
    ]
  ; "llprograms/analysis10.ll", lblm[
      "_entry", uids[]
      ; "else", uids["1"; "7"]
      ; "merge", uids["1"; "7"]
      ; "then", uids["1"; "7"]
    ]
  ; "llprograms/analysis1.ll", lblm[
      "_entry", uids["argc"]
      ; "l1", uids["1"; "3"]
    ]
  ]


let alias_analysis_tests =
  let open Alias.SymPtr in
  [ "llprograms/analysis9.ll", lblm[
        "_entry", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "body", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "end", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ; "guard", uidm["1", Unique; "2", Unique; "argv", MayAlias]
      ]
  ; "llprograms/analysis8.ll", lblm[
      "_entry", uidm["argv", MayAlias; "tmp1", Unique]
    ]
  ; "llprograms/analysis7.ll", lblm[
      "_entry", uidm["2", Unique; "4", Unique; "argv", MayAlias]
    ; "body", uidm["2", Unique; "4", Unique; "argv", MayAlias]
    ; "end", uidm["2", Unique; "4", Unique; "argv", MayAlias]
    ; "guard", uidm["2", Unique; "4", Unique; "argv", MayAlias]
    ]
  ; "llprograms/analysis5.ll", lblm[
      "_entry", uidm["argv", MayAlias]
    ]
  ; "llprograms/analysis4.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
    ; "l1", uidm["3", MayAlias; "6", MayAlias; "7", MayAlias; "arcv", MayAlias]
    ; "l2", uidm["3", MayAlias; "6", MayAlias; "7", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis3.ll", lblm[
      "_entry", uidm["arcv", MayAlias]
    ; "l2", uidm["arcv", MayAlias]
    ; "l3", uidm["arcv", MayAlias]
    ; "l4", uidm["arcv", MayAlias]
    ; "l5", uidm["arcv", MayAlias]
    ; "l6", uidm["arcv", MayAlias]
    ; "l7", uidm["arcv", MayAlias]
    ; "l8", uidm["arcv", MayAlias]
    ; "l9", uidm["arcv", MayAlias]
    ; "lexit", uidm["arcv", MayAlias]
    ]
  ; "llprograms/analysis2.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
    ; "l1", uidm["3", MayAlias; "5", MayAlias; "arcv", MayAlias]
    ; "l2", uidm["3", Unique; "arcv", MayAlias]
    ]
  ; "llprograms/analysis19.ll", lblm[
      "_entry", uidm["1", Unique; "2", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis15.ll", lblm[
      "_entry", uidm["arcv", MayAlias; "head", MayAlias; "link", MayAlias; "link2", MayAlias; "next", MayAlias; "next2", MayAlias; "val", MayAlias; "val2", MayAlias]
    ]
  ; "llprograms/analysis14.ll", lblm[
      "_entry", uidm["1", MayAlias; "2", MayAlias; "arcv", MayAlias]
    ]
  ; "llprograms/analysis11.ll", lblm[
      "_entry", uidm["argv", MayAlias]
    ; "foo", uidm["4", Unique; "5", Unique]
    ]
  ; "llprograms/analysis1.ll", lblm[
      "_entry", uidm["3", Unique; "arcv", MayAlias]
    ; "l1", uidm["3", MayAlias; "4", MayAlias; "arcv", MayAlias]
    ]
  ]


  
let constprop_analysis_tests =
  let open Constprop.SymConst in
  [ "llprograms/analysis8.ll", lblm[
      "_entry", uidm["a", Const (1L); "argc", NonConst; "argv", NonConst; "b", Const (3L); "c", Const (6L); "d", Const (10L); "e", Const (15L); "f", Const (21L); "g", Const (28L); "h", Const (36L); "i", Const (45L); "j", Const (55L); "k", Const (66L); "l", Const (78L); "m", Const (91L); "n", NonConst; "o", NonConst; "p", NonConst; "q", NonConst; "r", NonConst; "s", NonConst; "t", NonConst; "tmp1", NonConst; "tmp2", NonConst; "u", NonConst; "v", NonConst; "w", NonConst; "x", NonConst; "y", NonConst; "z", NonConst]
    ]
  ; "llprograms/analysis7.ll", lblm[
      "_entry", uidm["1", Const (10L); "2", NonConst; "3", Const (1L); "4", NonConst; "argc", NonConst; "argv", NonConst]
      ; "body", uidm["1", Const (10L); "2", NonConst; "3", Const (1L); "4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "argc", NonConst; "argv", NonConst]
      ; "end", uidm["1", Const (10L); "2", NonConst; "3", Const (1L); "4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "argc", NonConst; "argv", NonConst]
      ; "guard", uidm["1", Const (10L); "2", NonConst; "3", Const (1L); "4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "argc", NonConst; "argv", NonConst]
    ]
  ; "llprograms/analysis6.ll", lblm[
      "_entry", uidm["1", Const (2L); "2", Const (3L); "3", Const (4L); "argc", NonConst; "argv", NonConst]
      ; "else", uidm["1", Const (2L); "2", Const (3L); "3", Const (4L); "4", Const (1L); "argc", NonConst; "argv", NonConst]
      ; "merge", uidm["1", Const (2L); "2", Const (3L); "3", Const (4L); "4", Const (1L); "argc", NonConst; "argv", NonConst]
      ; "then", uidm["1", Const (2L); "2", Const (3L); "3", Const (4L); "4", Const (1L); "argc", NonConst; "argv", NonConst]
    ]
  ; "llprograms/analysis5.ll", lblm[
      "_entry", uidm["1", Const (7L); "2", Const (7L); "3", Const (14L); "argc", NonConst; "argv", NonConst]
    ]
  ; "llprograms/analysis4.ll", lblm[
      "_entry", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "arcv", NonConst; "argc", NonConst]
      ; "l1", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "9", NonConst; "arcv", NonConst; "argc", NonConst]
      ; "l2", uidm["1", Const (49L); "10", NonConst; "2", NonConst; "3", NonConst; "4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "9", NonConst; "arcv", NonConst; "argc", NonConst]
    ]
  ; "llprograms/analysis3.ll", lblm[
      "_entry", uidm["1", Const (14L); "arcv", NonConst; "argc", NonConst]
      ; "l2", uidm["1", Const (14L); "2", Const (28L); "arcv", NonConst; "argc", NonConst]
      ; "l3", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "arcv", NonConst; "argc", NonConst]
      ; "l4", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "arcv", NonConst; "argc", NonConst]
      ; "l5", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "arcv", NonConst; "argc", NonConst]
      ; "l6", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "6", Const (3L); "arcv", NonConst; "argc", NonConst]
      ; "l7", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "6", Const (3L); "7", Const (3L); "arcv", NonConst; "argc", NonConst]
      ; "l8", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "6", Const (3L); "7", Const (3L); "8", Const (67L); "arcv", NonConst; "argc", NonConst]
      ; "l9", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "6", Const (3L); "7", Const (3L); "8", Const (67L); "9", Const (188L); "arcv", NonConst; "argc", NonConst]
      ; "lexit", uidm["1", Const (14L); "2", Const (28L); "3", Const (-4L); "4", Const (-8L); "5", Const (15L); "6", Const (3L); "7", Const (3L); "8", Const (67L); "9", Const (188L); "arcv", NonConst; "argc", NonConst]
    ]
  ; "llprograms/analysis2.ll", lblm[
      "_entry", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "4", Const (0L); "arcv", NonConst; "argc", NonConst]
      ; "l1", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "4", Const (0L); "5", NonConst; "arcv", NonConst; "argc", NonConst]
      ; "l2", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "4", Const (0L); "arcv", NonConst; "argc", NonConst]
    ]
  ; "llprograms/analysis19.ll", lblm[
      "_entry", uidm["1", NonConst; "2", NonConst; "3", NonConst; "arcv", NonConst; "argc", NonConst]
    ]
  ; "llprograms/analysis17.ll", lblm[
      "_entry", uidm["i1", Const (2L); "i2", Const (3L); "sx", NonConst; "sy", NonConst; "v1", NonConst; "v2", NonConst; "v3", NonConst; "x", NonConst; "y", NonConst]
      ; "l1", uidm["a1", NonConst; "arg1", Const (12L); "i1", Const (2L); "i2", Const (3L); "sx", NonConst; "sy", NonConst; "v1", NonConst; "v2", NonConst; "v3", NonConst; "v4", NonConst; "x", NonConst; "y", NonConst]
    ]
  ; "llprograms/analysis15.ll", lblm[
      "_entry", uidm["1", NonConst; "arcv", NonConst; "argc", NonConst; "head", NonConst; "link", NonConst; "link2", NonConst; "next", NonConst; "next2", NonConst; "val", NonConst; "val2", NonConst]
    ]
  ; "llprograms/analysis14.ll", lblm[
      "_entry", uidm["1", NonConst; "2", NonConst; "3", Const (2L); "4", Const (0L); "arcv", NonConst; "argc", NonConst]
    ]
  ; "llprograms/analysis11.ll", lblm[
      "_entry", uidm["1", Const (1L); "2", Const (2L); "3", Const (3L); "argc", NonConst; "argv", NonConst]
      ; "foo", uidm["4", NonConst; "5", NonConst; "6", NonConst; "7", NonConst; "8", NonConst; "9", NonConst]
    ]
  ; "llprograms/analysis10.ll", lblm[
      "_entry", uidm["1", Const (30L); "2", Const (6L); "3", Const (3L); "4", NonConst; "5", NonConst; "6", Const (12L); "7", NonConst; "8", NonConst; "9", Const (1L); "argc", NonConst; "argv", NonConst]
      ; "else", uidm["1", Const (30L); "12", NonConst; "13", NonConst; "2", Const (6L); "3", Const (3L); "4", NonConst; "5", NonConst; "6", Const (12L); "7", NonConst; "8", NonConst; "9", Const (1L); "argc", NonConst; "argv", NonConst]
      ; "merge", uidm["1", Const (30L); "10", NonConst; "11", NonConst; "12", NonConst; "13", NonConst; "14", NonConst; "15", Const (30L); "16", NonConst; "2", Const (6L); "3", Const (3L); "4", NonConst; "5", NonConst; "6", Const (12L); "7", NonConst; "8", NonConst; "9", Const (1L); "argc", NonConst; "argv", NonConst]
      ; "then", uidm["1", Const (30L); "10", NonConst; "11", NonConst; "2", Const (6L); "3", Const (3L); "4", NonConst; "5", NonConst; "6", Const (12L); "7", NonConst; "8", NonConst; "9", Const (1L); "argc", NonConst; "argv", NonConst]
    ]
  ; "llprograms/analysis1.ll", lblm[
      "_entry", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "arcv", NonConst; "argc", NonConst]
      ; "l1", uidm["1", Const (49L); "2", NonConst; "3", NonConst; "4", NonConst; "arcv", NonConst; "argc", NonConst]
    ]
  ]

