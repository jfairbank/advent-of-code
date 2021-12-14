module Day13Tests exposing (suite)

import Day13 exposing (puzzle1, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day13"
        [ describe "puzzle1"
            [ test "solves example input" <|
                \_ ->
                    puzzle1 exampleInput
                        |> Expect.equal 17
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal 610
            ]
        , describe "puzzle2"
            [ test "solves example input" <|
                \_ ->
                    puzzle2 exampleInput
                        |> Expect.equal 16
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal 95
            ]
        ]


exampleInput : String
exampleInput =
    """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""


puzzleInput : String
puzzleInput =
    """
1288,822
842,72
693,68
561,254
1007,192
405,794
929,45
58,296
411,317
164,3
894,317
371,306
272,205
646,865
1027,700
920,667
728,665
1240,105
390,227
972,257
561,640
954,128
440,194
1136,712
37,516
1072,93
1210,576
390,486
682,611
343,368
236,840
366,889
238,644
55,130
939,388
619,726
1092,23
1180,458
338,189
176,516
915,210
550,100
773,466
351,554
460,389
266,560
154,810
604,646
969,880
627,793
211,0
842,520
460,647
678,72
1007,702
863,170
371,836
716,233
1260,695
682,84
710,745
852,94
200,756
574,383
112,306
1072,698
1103,460
617,826
276,827
1158,73
959,702
454,327
90,502
790,463
480,742
985,577
1205,334
298,766
371,388
320,754
1004,252
765,437
1290,550
1191,210
1170,296
865,522
262,663
504,714
406,222
174,630
402,609
30,169
164,127
1074,54
663,77
136,353
316,632
164,52
512,718
1038,82
351,453
1027,413
68,600
1110,756
5,18
1160,180
632,822
600,793
798,718
428,625
964,443
904,672
596,122
174,745
366,403
648,252
1121,164
920,606
464,807
749,254
624,501
1282,543
1126,494
554,891
236,54
333,483
1233,459
843,504
928,171
500,278
1168,512
937,504
6,177
830,264
584,105
1287,788
375,250
982,58
788,695
386,324
1273,378
88,129
756,3
218,471
686,501
20,326
326,114
885,750
536,182
209,56
20,568
944,98
428,94
1101,693
644,413
373,410
912,830
1096,833
390,219
0,828
93,684
1038,306
1101,390
1037,493
1290,763
288,403
346,317
1191,140
840,821
962,131
1116,505
688,458
55,460
416,196
405,100
856,474
528,376
929,849
873,381
483,621
1002,317
1004,730
375,644
279,250
378,442
1128,317
1022,768
681,843
164,630
80,606
20,763
112,588
972,189
514,150
1084,766
433,782
373,484
1180,436
894,196
1089,560
1290,26
1192,731
576,672
1185,145
296,228
38,441
870,733
1059,747
1006,856
1255,434
1275,278
718,38
806,714
1304,177
313,14
412,563
877,112
238,698
976,502
1168,624
1139,397
1038,588
1010,308
242,115
679,393
935,838
959,620
62,550
8,29
1230,383
1256,745
1252,632
1184,376
140,296
1002,353
594,774
734,222
1242,70
1146,400
296,725
87,187
786,10
982,562
1128,577
1096,385
542,285
1198,588
433,112
161,82
492,854
209,504
80,624
984,556
325,364
126,868
1128,50
760,346
1287,554
1059,644
54,745
865,372
283,33
288,5
1038,597
944,348
863,252
994,262
788,505
37,378
1174,311
1124,374
990,140
383,764
1037,885
874,812
1120,171
662,754
1119,116
142,718
492,171
326,506
1146,630
1283,793
234,382
990,754
1230,606
1287,106
1116,4
1148,499
296,666
437,381
944,770
209,693
472,437
749,192
296,218
1248,102
1183,502
885,592
8,865
820,2
130,94
774,387
1086,263
119,140
962,252
1236,161
683,793
539,642
147,190
632,374
932,173
7,525
50,621
528,742
806,266
832,516
835,256
537,649
1014,423
15,816
1166,661
1223,747
105,334
408,507
636,465
1138,84
273,9
405,346
1139,716
761,560
559,12
328,836
1238,665
126,119
1310,828
981,418
390,606
348,252
80,383
600,275
502,98
875,130
405,548
756,563
351,441
232,292
510,213
358,808
241,11
1290,131
190,171
1232,672
306,730
340,404
1198,509
840,73
744,765
935,250
612,462
308,17
174,264
271,802
970,404
338,705
229,750
782,518
937,670
574,288
271,259
1148,575
1303,817
47,56
808,613
956,801
28,543
994,705
130,542
119,618
775,460
519,824
1248,744
574,848
1303,824
592,102
303,702
303,340
928,619
219,578
798,644
1037,849
150,180
1305,876
1180,94
908,285
706,646
490,52
214,283
6,588
236,137
584,718
218,306
850,330
981,866
1136,264
818,171
710,275
0,605
873,65
207,434
1255,460
952,808
850,135
1201,737
594,661
718,856
23,788
1184,308
1044,101
416,698
325,577
1165,887
535,140
162,575
1021,602
932,721
118,546
1184,294
634,868
584,294
528,518
596,150
902,507
627,742
810,278
63,266
115,476
985,317
870,161
529,590
214,611
20,131
49,520
1039,259
972,262
26,588
1261,520
375,838
1027,861
343,526
771,642
1146,127
838,577
77,459
907,634
492,275
1195,670
1235,634
126,308
62,38
58,324
390,288
191,116
905,794
478,378
846,737
1163,190
830,546
1230,507
1031,364
674,429
1084,317
1220,392
791,824
592,344
522,505
174,149
464,82
1220,502
276,4
346,353
112,385
266,793
1077,192
1062,448
42,432
528,824
346,577
1084,128
383,802
395,210
1027,33
592,150
1195,700
574,894
648,754
592,856
1267,77
1267,58
340,490
749,702
421,702
863,460
251,147
1238,341
698,462
440,611
249,346
964,281
172,138
664,865
115,726
136,801
155,845
810,362
283,506
412,779
219,477
944,5
226,128
1022,403
636,429
984,562
388,334
70,401
764,759
1074,502
50,695
846,157
972,632
666,413
1146,3
383,316
1149,82
356,128
1180,800
103,44
174,712
273,493
629,51
499,49
1274,742
416,252
683,242
596,296
316,637
882,129
1103,684
226,317
1077,702
1093,65
846,807
817,812
144,233
130,800
724,70
522,340
985,364
1310,156
1207,850
683,652
959,254
1297,462
390,675
856,518
1248,38
574,831
1061,548
447,460
818,40
182,801
676,26
308,613
272,689
1292,541
333,484
694,348
1146,842
792,353
1185,749
492,619
54,149
207,460
736,270
681,51
1222,250
985,353
319,402
316,262
272,306
20,282
406,672
990,541
1027,194
584,124
1260,646
190,801
850,759
1178,505
1091,130
1103,194
325,353
194,505
1223,187
686,277
105,177
308,283
216,763
288,546
1260,173
13,462
523,852
512,644
850,389
806,378
1263,56
764,135
718,150
736,0
773,354
1092,471
440,733
1138,810
874,530
775,684
959,640
818,275
775,434
1155,49
841,80
88,196
546,135
1265,873
782,586
283,413
1081,144
308,353
200,138
929,401
80,224
354,588
550,346
1222,765
522,695
832,378
920,219
937,581
171,397
728,229
830,854
25,51
226,331
1037,9
1176,431
686,871
694,98
928,597
1038,569
939,588
288,768
375,56
827,621
709,401
1021,292
340,42
281,448
1148,52
744,250
422,548
437,9
1014,725
662,140
1002,283
928,588
736,471
792,541
70,124
726,572
300,84
157,746
390,238
1260,273
1088,520
447,252
1078,346
234,512
577,883
1002,765
288,348
207,724
249,11
185,528
771,224
464,535
714,122
731,260
691,726
616,403
694,403
1168,718
279,530
894,698
686,23
771,726
1280,169
892,392
413,435
781,590
1175,742
774,182
313,880
828,138
88,250
539,224
582,105
724,518
551,268
490,891
23,106
991,402
229,144
566,250
1285,51
1068,779
182,577
472,885
770,840
1007,340
80,288
644,848
574,176
251,644
1014,218
1272,43
248,448
468,520
810,616
25,724
348,642
194,389
604,695
189,164
216,131
1116,564
674,465
125,749
907,147
10,413
937,410
550,822

fold along x=655
fold along y=447
fold along x=327
fold along y=223
fold along x=163
fold along y=111
fold along x=81
fold along y=55
fold along x=40
fold along y=27
fold along y=13
fold along y=6
    """
