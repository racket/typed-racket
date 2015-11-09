#lang racket/base

(require "tr-random-testing.rkt")

;; list of all the counterexamples that the random tester found on drdr,
;; as of drdr run #32529
;; we test that there has been no regression, and print any that are still bugs

(define counterexamples
  '(
(/
 (max
  (max 5.291710918926604 1.1928108739910326 1.429136489345783)
  (+ 3.134401165869889)
  (tan 1.5306769261091948))
 (unsafe-fl/ (unsafe-fl+ 0.0 0.0) (fl+ 0.0 -1.4988550184671405)))
(log (log 0.72188586f0))
(/ +inf.0 +inf.0 +inf.0)
(/ (max 0.09531933367867522 +inf.0) (flmax +inf.0 1.976262583365e-323))
(* 1/5 0.0f0 (max +inf.0 0.0 -18.8335626447589))
(/ (max -11/2 -0.6208988f0 0))
(fl* -0.0 -0.0)
(/ (min -inf.0 -1/2 0.19923423380607755))
(/ (abs (min -inf.0 -6.810114f+25 0)))
(/ (max 1.5005304f+34 -9/2 +inf.0))
(fltruncate (unsafe-fl* -0.0 -0.0))
(/ (max -0.0 0.0f0 -4/3))
(flexpt (ceiling 0.23308186803037037) -inf.0)
(flexpt (ceiling 0.1819412186378086) (flexpt +inf.0 0.24507424933264124))
(/ (max (sqrt +inf.0) (min 0.03663685909017611 0.04795201378269561 -1)))
(* 0.0f0 +inf.f)
(/ (max -7/11 0.0f0 -0.1725517f0))
(/ (max 0 +inf.0))
(- -2.2560916f0 -1/3 0.001284063623073798)
(-
 (- 3 -1.069543f0 6.562867812465617e-183)
 (* 4.616255654942392)
 0.1086093691621788)
(/ 4/3 2.460332f+31 (flexp 0.08423293999406778))
(floor (/ 1.7976931348623143e+308 (floor 1/4)))
(+ -0.25350437f0 1/7 (+ 1.721845751120134e-95 0.0 9.8813129168249e-324))
(* 0 (/ 1.0948245f+22 0 -676958.4f0))
(* (- 0) 0 (/ 0.0 0 +inf.f))
(round +nan.f)
(sqrt +nan.f)
(* 0 (/ 0.24441261818581836 0))
(* (/ -0.09734836534558132 0) (abs 0))
(cosh
 (+
  (cosh (- 3))
  (/ (real->double-flonum 746128.75f0))
  (max 2.3177779f0 0 1.1257266f+09)))
(tanh
 (*
  (lcm (exact-round 6) (exact-round -2.1790001f-30))
  (bitwise-not (exact-round 0))))
(tanh (abs (remainder (exact-round 6) (exact-round -1))))
(sinh (sqr (max (exact-round 1/2) (exact-round 0))))
(sinh (- (min 0)))
(tanh (min (remainder (exact-round 1/2) (exact-round 1.1763921f0))))
(sinh (floor (max (exact-round 0.36314553f0))))
(sinh (round (- (exact-round -1/6) (exact-round 0))))
(sinh (- 0))
(sinh (sqr (bitwise-xor (exact-round -1/3))))
(tanh (* (round (exact-round 3.4584595208887e-323))))
(+ (sinh (- 0)) 1.213183f-20)
(sinh (- (* (exact-round -1.4821969375237e-323) (exact-round 1.5342411f+31))))
(tanh (truncate -1/2))
(sinh (round (gcd (exact-round 2.7844077f-14) (exact-round 0.0))))
(sinh (- 0))
(sinh (truncate (max (exact-round -1/5))))
(tanh (- 0))
(tanh
 (abs (quotient (exact-round -2/3) (exact-round -2.2804670485636814e+98))))
(sinh
 (abs
  (bitwise-ior
   (exact-round -1.976262583365e-323)
   (exact-round 0)
   (exact-round 0.24133845f0))))
(/
 2.3454025f0
 (flmin
  (real->double-flonum 1.797693134862315e+308)
  (real->double-flonum -1.2848677f+32))
 (make-rectangular +nan.0 0.0))
(- (sqrt 1) -6.249515f+17 (tanh (min 0 19/9 0)))
(sinh (- 0))
(* (sinh (round 1/5)))
(tanh (ceiling -6/7))
(*
 (+ 15/4 -inf.0)
 (tanh (min 1 0 2))
 (min (real->double-flonum 17/8) (real->double-flonum 3)))
(exp (sinh (truncate -4/5)))
(tanh (round (- (exact-round -2.2143375f-34))))
(sinh (floor (bitwise-xor (exact-round -5.592468f-24))))
(/
 (fl/ (real->double-flonum 1) (real->double-flonum 2.267724318104924))
 (make-rectangular -inf.f -1.2723509554070223)
 2.4891517f-05)
(tanh (min (sub1 (exact-round 2)) (round -1/2)))
(tanh (add1 (bitwise-ior (exact-round -6.327578f0) (exact-round -1))))
(tanh (- (sqr (exact-round 0.0f0))))
(sinh
 (-
  (* (exact-round 131245384.0f0) (exact-round 1/19) (exact-round 3))
  (quotient (exact-round 0) (exact-round 1))))
(/
 (+ (real->double-flonum 0.41378116537811604) (real->double-flonum 1))
 (round (real->double-flonum 1.8869765f0))
 (make-rectangular -1/2 1.7976931348623157e+308))
(sin (sinh (- 0)))
(tanh (+ (* (exact-round 1/2) (exact-round 1.51136f-11))))
(sinh
 (min
  (*
   (exact-round 9.8813129168249e-324)
   (exact-round 1.7800292572466936e+144))))
(tanh (abs (remainder (exact-round -2/7) (exact-round 1))))
(-
 (* 1 7 -inf.f)
 (log -1.6259106f0)
 (make-rectangular 1.0812368f0 -1.7976931348623157e+308))
(make-polar (sinh (add1 -1)) (abs 2.709792f-24))
(tanh (- (abs (exact-round 2/7))))
(tanh (sinh (+ (arithmetic-shift (exact-round 1/13) (exact-round 6)))))
(tanh (* (min (exact-round 0) (exact-round 0) (exact-round -1.08295304f-29))))
(sinh (floor (bitwise-and (exact-round 4.9406564584125e-324))))
(sinh (tanh (round -1/4)))
(sinh
 (max (ceiling (exact-round -23/6)) (round (exact-round 1.976262583365e-323))))
(sinh (round (round 1/9)))
(sinh (round (integer-length (exact-round 0.23142774f0))))
(tanh (min (* +inf.f 0) (integer-length (exact-round -1/6)) (ceiling 27)))
(tanh (round -1/2))
(tanh (- (round (exact-round 0.02347728403277505))))
(sinh (sqr (ceiling (exact-round -1/2))))
(sinh (max (round (exact-round 6.9156096f-34))))
(tanh (* (max (exact-round -7) (exact-round 1/3))))
(tanh (tanh (* (min (exact-round 13.884806f0)) (sqr (exact-round 1/2)))))
(tanh (min 1/7 0))
(sinh (truncate 11/26))
(sinh (add1 -1))
(sinh (- (+ 0)))
(tanh (- 0))
(sinh
 (+
  (make-polar 4.8063810141303426e-57 -1.9082319f0)
  (sin (real->double-flonum -1))))
(tanh (- (lcm (exact-round 5.928787750095e-323))))
(sinh (- (lcm (exact-round -2.2155745000357632e-178) (exact-round 1))))
(sinh (* (truncate (exact-round 5/48))))
(- (tanh (- 0)))
(/ (make-rectangular 1/7 +inf.0))
(tanh
 (min
  (max (exact-round -7.765142f-15))
  (sqr (exact-round 0))
  (quotient (exact-round 1) (exact-round -1.7159756280409113))))
(/
 (/
  (real->double-flonum 4/7)
  (real->double-flonum -4.2317797f-37)
  (real->double-flonum -1/6))
 -11.191491f0
 (make-rectangular 5 +inf.0))
(tanh (round (min 0 12 0)))
(tanh (* (gcd (exact-round 3.5735087f-33))))
(tanh (truncate (lcm (exact-round -0.29972658f0))))
(tanh (max (max (exact-round 0.0031026218f0) (exact-round -4/3)) 0))
(tanh (- 0))
(tanh
 (abs
  (min
   (exact-round 0.0)
   (exact-round -1/9)
   (exact-round 4.033356541333312e-08))))
(sinh
 (min (quotient (exact-round -9.232606568676525) (exact-round 10.016169f0))))
(tanh (- (lcm (exact-round 2.2755875663910166e-147))))
(tanh (abs (ceiling -1/2)))
(tanh (tanh (floor (- -1/4))))
(round (bitwise-and (exact-round -1) (exact-round 3.4055861964043973e+281)))
(sinh (abs (bitwise-xor (exact-round 2) (exact-round 2.352828872943168))))
(sinh (- (remainder (exact-round -1.8771651f0) (exact-round -2))))
(tanh (max 0 (sub1 (exact-round -4.67788658365992e-41))))
(sinh (min 0 2 0))
(tanh (min (add1 (exact-round 5.477436111395335e+112)) (- (exact-round 0))))
(tanh
 (*
  (truncate (exact-round 5))
  (* (exact-round 7) (exact-round 6) (exact-round 0.1389169448941857))
  (gcd (exact-round 2) (exact-round 3) (exact-round -1.9134427f0))))
(sinh (+ (* (exact-round -1/4)) 0))
(sinh
 (sqr
  (bitwise-and
   (exact-round -1.7976931348623157e+308)
   (exact-round 13.534782f0)
   (exact-round 10))))
(tanh (- 0))
(sinh (- (bitwise-and (exact-round 3.3552675f-07) (exact-round -0.0))))
(tanh (sqr (gcd (exact-round -1.9518888f-30))))
(tanh (- 0))
(tanh (/ (max (exact-round 7.6979246f-32)) 4))
(tanh
 (min
  (min (exact-round 3) (exact-round 0))
  (lcm (exact-round -8.50503736511622e+249))
  (sub1 (exact-round 3))))
(tanh (min (lcm (exact-round -5.2226904f+10) (exact-round 6.9677464f+15)) 0))
(sinh (- 0))
(tanh (* -1 (- (exact-round 0.0f0))))
(sinh (min (sub1 (exact-round 10)) (* (exact-round 1.797693134862315e+308)) 0))
(*
 (tanh (truncate -2/5))
 (flmin (real->double-flonum -inf.0) (real->double-flonum 2)))
(sinh
 (min
  (min
   (exact-round 5.4347221042537e-323)
   (exact-round 1.7976931348623157e+308))))
(sinh
 (-
  (min
   (exact-round 1.0995611268076682e-78)
   (exact-round 14.208988f0)
   (exact-round 1.7976931348623151e+308))))
(tanh (* (min 9 0) (* (exact-round 14) (exact-round 9.8813129168249e-324))))
(tanh (- 0))
(abs (sinh (truncate 2/7)))
(tanh (min 2 0))
(/ (make-rectangular -inf.f 1.4821969375237e-323))
(sinh (sqr (arithmetic-shift (exact-round -8.2284605f-38) (exact-round 1))))
(sinh (max 0 (* -2)))
(tanh (min (* (exact-round 1/2) (exact-round 17))))
(sinh (- (tan 0)))
(/ 1/7 (make-rectangular +inf.0 6) (- 0 1/2))
(tanh (max (- (exact-round 0))))
(tanh
 (tanh
  (round
   (modulo (exact-round 1.4821969375237e-323) (exact-round -1.5959707f0)))))
(tanh (- 0))
(/ (make-rectangular 9.8813129168249e-324 +inf.0))
(tanh (round (lcm (exact-round 2/7) (exact-round -1.6457217893840803))))
(tanh (max (min (exact-round 4) (exact-round 0) (exact-round 0.0))))
(tanh (+ (min (exact-round 0.008915729f0)) 0))
(tanh (tanh (+ (integer-length (exact-round 0.28622946f0)))))
(sinh (sqr (+ (exact-round 2.7228466658514113e-107))))
(/
 (+ (exact-round 1.8655746f+35) (exact-round 1))
 2.0324421f-21
 (make-rectangular 4 1.7976931348623157e+308))
(sinh (truncate 1/3))
(tanh
 (tanh
  (floor
   (arithmetic-shift (exact-round 0) (exact-round 2.9643938750475e-323)))))
(tanh (min 0 0 1/2))
(sinh
 (sqr
  (arithmetic-shift
   (exact-round 0.40748192417299584)
   (exact-round -10.049699650875125))))
(sinh
 (*
  (*
   (exact-round -21.934877f0)
   (exact-round -1/4)
   (exact-round -1.0979695f0))))
(tanh (truncate -1/2))
(sinh (max (round (exact-round -9)) (min (exact-round -1/5) (exact-round 3))))
(sinh (max (min (exact-round 0) (exact-round -2.8196778f-13))))
(sinh (min 7 1/5 0))
(sinh
 (max (min -8/5 6) (min (exact-round 3) (exact-round 0) (exact-round 0.0f0))))
(tanh (round -1/4))
(sinh (- (remainder (exact-round 4/3) (exact-round 1))))
(sinh
 (*
  (- (exact-round 0))
  (max (exact-round 2) (exact-round 3.95252516673e-323))))
(sinh (/ (- (exact-round -13/42)) 7))
(sinh (ceiling (quotient (exact-round 4.4465908125712e-323) (exact-round 1))))
(/
 (make-polar
  (ceiling +inf.0)
  (fl- (real->double-flonum 9) (real->double-flonum -0.0f0)))
 (exp 0.0f0))
(/
 (make-rectangular -7.403526f0 -1.7976931348623143e+308)
 (/ -4 1.9067208f+09)
 (flabs (real->double-flonum 1/7)))
(/
 (make-rectangular -inf.f -inf.0)
 (flceiling (real->double-flonum -1.797693134862315e+308)))
(sinh (* (* (exact-round 2) (exact-round 0.09098263f0))))
(tanh (+ (truncate (exact-round 1/2))))
(sinh (min 0 (max 1 3 -1) (abs (exact-round 0))))
(/
 (bitwise-not (exact-round 0.9279912613796584))
 (flcos (real->double-flonum -1))
 (+ (make-rectangular 1 3.95252516673e-323)))
(sinh (* (arithmetic-shift (exact-round 0.0) (exact-round 5))))
(- (tanh (min 0 3/10 3)))
(sinh (min (bitwise-xor (exact-round 0) (exact-round 1.976262583365e-323))))
(tanh (min 0 5))
(tanh (abs (arithmetic-shift (exact-round -0.0f0) (exact-round 3))))
(sinh (ceiling (bitwise-xor (exact-round 2.8510355038856043e-154))))
(sinh (max -1 0))
(sinh
 (abs (quotient (exact-round 3.0935251079895583) (exact-round -1.635804f+33))))
(sinh (- (bitwise-and (exact-round 24) (exact-round 0))))
(+
 1.5245886f+12
 (max (exact-round 2) (exact-round 5/4))
 (tanh (make-rectangular 1.4291365847030308e-64 -0.76987815f0)))
(/ -1/4 (make-rectangular -inf.0 -2))
(sinh (max (ceiling 0) (integer-length (exact-round 0))))
(sinh
 (sqr
  (min
   (exact-round 1/2)
   (exact-round 0)
   (exact-round -5.524229786036656e-164))))
(tanh (round 1/10))
(sin (sinh (truncate -1/2)))
(sinh
 (min
  (max (exact-round 0) (exact-round -8.157221982588571e+43))
  (gcd (exact-round 2) (exact-round 9.8813129168249e-324) (exact-round 1))
  (abs -2)))
(sinh (truncate (* (exact-round 1/5))))
(/ (make-rectangular -1.7976931348623157e+308 -1.0118355f-14))
(tanh (- (max (exact-round 0.0))))
(+
 (cos 1)
 (* -1.338113f+27 -29.544191f0)
 (make-polar 9.8813129168249e-324 8.389207776771219))
(cosh
 (/
  (make-rectangular +inf.0 1.9165856529632936)
  (fltan (real->double-flonum 1))))
(sinh (- (bitwise-ior (exact-round 0))))
(/
 (max -4.4064098f+24 -inf.0 1/3)
 (make-rectangular 4.971020894390071e+51 +inf.0)
 -1.709600729934065)
(- (tanh (round 1/2)))
(sinh
 (min
  (max 0)
  (ceiling (exact-round -2.5244543529738035e-23))
  (* (exact-round -1) (exact-round 0) (exact-round -1.6624979f0))))
(tanh (ceiling (+ (exact-round 4.9406564584125e-324))))
(tanh (floor (max -1/3 0)))
(sinh (round (remainder (exact-round 5.75456f0) (exact-round -1.5700808f0))))
(tanh (min (floor (exact-round 0)) 1 (max (exact-round 11))))
(tanh
 (round (remainder (exact-round 0.0) (exact-round 1.048512178375371e+274))))
(sinh (- 0))
(sinh (max (gcd (exact-round 0)) -2))
(tanh (* (modulo (exact-round -2.552142604920734e-296) (exact-round 3))))
(tanh (- (+ 0)))
(tanh (+ (bitwise-xor (exact-round 8.414699f-29) (exact-round 0))))
(+
 (flround (real->double-flonum 1.60541635f-24))
 (make-polar -2.4861934f+36 7.734753f-36))
(tanh
 (floor
  (arithmetic-shift (exact-round 1) (exact-round -2.018803398217005e+278))))
(tanh (+ (* 0 -4.844077f0 4.659485590796607e+264) (abs (exact-round -1/3)) 0))
(sinh (+ (min (exact-round 9.8813129168249e-324) (exact-round 10))))
(tanh (sinh (floor 2/5)))
(tanh (- (- (exact-round 0))))
(/ (make-rectangular -5 1.976262583365e-323))
(tanh
 (round
  (bitwise-and (exact-round -4.9406564584125e-324) (exact-round 13.596185f0))))
(tanh (max (round (exact-round 2.4703282292062e-323))))
(tanh
 (min
  (gcd (exact-round -2) (exact-round -5.094050997695298) (exact-round 5/4))
  (ceiling 0)))
(sinh (- (+ 0)))
(sinh (ceiling -3/4))
(sinh (+ 0 (gcd (exact-round 1/4)) 0))
(sinh (- (* (exact-round 4.070678f-34) (exact-round 1.7976931348623155e+308))))
(sinh
 (min
  (min
   (exact-round 0.14853434f0)
   (exact-round 4.9406564584125e-324)
   (exact-round 2.4760883f0))))
(sinh (round -1/25))
(/
 -1144.6757051834838
 1.8702761f-23
 (make-rectangular 9.8813129168249e-324 -16.04883f0))
(sinh
 (max
  (lcm
   (exact-round -1.4676713532377365e+205)
   (exact-round -7.029835175908767e-40))))
(/ (make-rectangular -inf.0 -1.8590675f-10) 7.1919174f+11)
(sinh (- 0))
(sinh
 (- (bitwise-ior (exact-round 0) (exact-round 1) (exact-round 1)) (max 1)))
(sinh (- (integer-length (exact-round -1/2))))
(sinh (* (bitwise-and (exact-round 1/2))))
(/ (make-rectangular -inf.f -7.936336217209017e-143))
(sinh (truncate (lcm (exact-round -1.2922209f-34))))
(sinh (- 0))
(sinh (min 2 0))
(/ (make-rectangular +inf.0 7))
(*
 (/
  (make-rectangular +inf.f -1.3086925781062948e-124)
  (exp 2)
  (bitwise-and (exact-round -3/11)))
 -4.880003479031522e-08)
(sinh
 (floor
  (min
   (exact-round -4.593328323419585e-295)
   (exact-round 6.49027206399e-09)
   (exact-round -0.0))))
(* -1/3 (sinh (- 0)))
(/
 (make-rectangular
  (sqr (real->double-flonum +inf.0))
  (max
   (real->double-flonum 4)
   (real->double-flonum -2)
   (real->double-flonum 5))))
(+
 (make-rectangular -1.7976931348623151e+308 1/15)
 (make-polar -12/43 0.008068093f0)
 (tan (real->double-flonum 1)))
(+ (make-polar 1.4128605f0 2.973313f-33) (max (real->double-flonum 2)))
(/ 4 (cosh (make-rectangular 1 9.8813129168249e-324)))
(/
 1.7976931348623141e+308
 (make-rectangular
  (min 1.4821969375237e-323 +inf.0 -1.7976931348623147e+308)
  -0.766481613292698)
 (unsafe-fl+
  (sub1 (real->double-flonum 0.6712944f0))
  (real->double-flonum 0.0)))
(/
 -1.797693134862314e+308
 (flexp (real->double-flonum 0))
 (make-rectangular (sqrt 5.0788827f0) (min 1.976262583365e-323)))
(/
 (fl+ (real->double-flonum 2) (real->double-flonum 1.5539016f+30))
 (make-rectangular 3 +inf.0)
 (unsafe-flmin (real->double-flonum -inf.f) (real->double-flonum +inf.0)))
(sub1 (/ (make-rectangular -inf.0 2) 7/10))
(/ (make-rectangular +inf.f -27.89912133921971))
(round
 (bitwise-and
  (exact-round -1)
  (exact-round -1.7976931348623143e+308)
  (exact-round -1)))
(/
 (make-rectangular 1 -3.307846703506794e-41)
 4.9406564584125e-324
 (- (real->double-flonum -6.579934f0) (real->double-flonum 3.0113332f0)))
(*
 (make-rectangular -inf.0 -inf.f)
 (* (real->double-flonum 1) (real->double-flonum -6.307696920933362e-113))
 (max -1 -1.5933586f+34 -1/5))
(/ (flcos (real->double-flonum 0)) (make-rectangular 4 +inf.0))
(/ 0.1758998f0 (make-rectangular 1.4373878f0 -inf.0))
(/ (make-rectangular 2 -inf.0) (cos (real->double-flonum 1)) -8/5)
(round
 (bitwise-and (+ (exact-round -5.2906824815254385e+300)) (exact-round -7)))
(sinh (truncate (remainder (exact-round 3) (exact-round 1))))
(tanh (- 0))
(tanh (floor 1/2))
(sinh (round (truncate (exact-round 1/5))))
(sinh (- (sqr (exact-round -2.2115127f-07))))
(tanh (max (lcm (exact-round 0.16265454825991324))))
(*
 (unsafe-fl- (real->double-flonum 1) (flfloor (real->double-flonum 4)))
 -21219.082f0
 (-
  (make-polar +inf.0 4.660871016258741e+149)
  (gcd (exact-round -4/31))
  (truncate -18)))
(tanh (round (integer-length (exact-round 0))))
(/
 (make-rectangular
  (max 1)
  (*
   (real->double-flonum -9.68355657230973e-272)
   (real->double-flonum 9.721236f0)))
 (+ (flexpt (real->double-flonum 0) (real->double-flonum 3.940896f-15)))
 -0.17994110264724025)
(*
 (make-rectangular 1.7976931348623151e+308 +nan.f)
 -4.170927f+08
 (* (real->double-flonum 1.4846453277988332) (real->double-flonum 4)))
(floor (bitwise-and (exact-round -5) (exact-round 6.29660329082445e+147)))
(/
 (make-polar
  0.0
  (max (real->double-flonum 2) (real->double-flonum 30.317604f0))))
(max (bitwise-and (exact-round 1.7976931348623157e+308) (exact-round -29)))
(/
 (make-rectangular +nan.0 (max 10 2 0))
 (fl*
  (+ (real->double-flonum -1.4821969375237e-323))
  (sin (real->double-flonum -1.2086458f0)))
 (-
  (* (real->double-flonum -1.7976931348623153e+308))
  (min (exact-round 3/4) (exact-round 1.9591119f0))))
(/ (floor -6) (make-rectangular +inf.0 -1.3982029f+20) -2)
(+
 (sin -14.41533f0)
 (make-rectangular 2 1.976262583365e-323)
 (* -3.0656253f-24 -7.1026087f0 +nan.0))
(*
 (min -25.716513f0 (min -17/3 2))
 -1.7976931348623157e+308
 (make-rectangular (abs +inf.0) (max (real->double-flonum 1))))
(/
 (make-rectangular
  (log 1.0697606688604438e+256)
  (* -inf.0 -1.6213031f-11 -0.41536245f0))
 3)
(*
 (+ (real->double-flonum 1.2021859f0) (fltruncate (real->double-flonum -0.0)))
 4
 (make-rectangular
  (+ (real->double-flonum +inf.f) (real->double-flonum 0.0))
  -inf.0))
(*
 (make-rectangular (abs (real->double-flonum +inf.0)) 5)
 (* (* (real->double-flonum 3/2)) 4.3061695f0))
(/
 -8.302019f-06
 (make-polar 0.0 5.560084769914483)
 (abs (real->double-flonum 0)))
(tanh
 (/
  (make-rectangular -inf.0 -0.30725425f0)
  3
  (sub1 (real->double-flonum 7.148287f+31))))
(/ (sqrt (make-rectangular +inf.0 -5/3)) 5)
(/ (make-rectangular -inf.0 1/2))
(+ (make-polar -1.901861929168266e+59 3.8788676f0) -7.957793388226727e-60)
(+
 (make-rectangular
  (min 9 4.9406564584125e-324)
  (sin (real->double-flonum -1.486133178603394)))
 (exp (sqr 15.821825f0))
 (max
  (real->double-flonum -4.6717093f-17)
  (*
   (real->double-flonum 1)
   (real->double-flonum 5/28)
   (real->double-flonum -2.032631686775097))
  (- (real->double-flonum -2.4329875f0))))
(/
 (* 0.0f0 (make-rectangular -7 5.928787750095e-323) -1.7976931348623155e+308))
(/
 8.3991159793012e-323
 (make-rectangular
  (cos (real->double-flonum -3))
  (add1 (real->double-flonum +inf.0))))
(/
 (+
  (make-rectangular 2.4703282292062e-323 -1/2)
  (* (exact-round 0) (exact-round -6.85372f0) (exact-round -1/2))))
(/
 (flabs
  (min
   (real->double-flonum 3)
   (real->double-flonum 7.089624754923938)
   (real->double-flonum 2)))
 (make-rectangular
  (flmin (real->double-flonum 3) (real->double-flonum -inf.0))
  -3))
(+ (make-polar -3.0679216f+17 35810724.0f0) (- 1 0.0 -2.2785807f-28))
(+
 (+ (real->double-flonum 0.0) (real->double-flonum 0.0f0))
 (make-polar -62.260838f0 0.110367715f0))
(/
 (make-rectangular -inf.f -1.7976931348623157e+308)
 (flceiling (real->double-flonum 4)))
(+ (sinh (tanh (make-polar 0.368395f0 +nan.f))) -2.2694893972124115)
(/ (make-rectangular 2.1481019965316337 9.8813129168249e-324))
(/
 (+ (real->double-flonum -9.6051934f-29) (real->double-flonum 0))
 (make-rectangular 1.4821969375237e-323 4))
(+
 (flceiling (real->double-flonum 4.9406564584125e-323))
 (make-polar -1.3426063f0 6.686688f0))
(/ (make-rectangular 8.667342f-16 +inf.0))
(/
 (make-rectangular -0.13919233954185614 -2.305314f0)
 (tanh (+ -0.0f0 0))
 (sub1 1.976262583365e-323))
(* (min 3/4) 0.9845773f0 (make-rectangular 3 0.0))
(/ (make-rectangular 6.019296f+12 9.8813129168249e-324))
(/
 (fllog (real->double-flonum 1.976262583365e-323))
 (make-rectangular 3.526483f-38 -inf.0))
(/ -5 2/7 (make-polar -0.0 (fltan (real->double-flonum -3.833043f+21))))
(floor (bitwise-and (exact-round 2.713926459459902e+100) (exact-round -1)))
(log (make-rectangular 2.275169f+11 (ceiling -inf.f)))
(max (bitwise-and (- (exact-round 1)) (exact-round 1.7976931348623151e+308)))
(/
 (make-rectangular
  (round (exact-round 3))
  (flround (real->double-flonum -inf.0))))
(max
 0
 (bitwise-and
  (bitwise-ior (exact-round -1) (exact-round 3) (exact-round 3))
  (exact-round 2.967380117744804e+112))
 7)
(/ (make-rectangular +inf.0 1.7976931348623155e+308) 1.1622358f+24)
(/
 (min
  (real->double-flonum 3)
  (real->double-flonum -1.7976931348623157e+308)
  (real->double-flonum 3/2))
 (make-rectangular 1.976262583365e-323 1.9796724097581277e+19))
(/
 (tan (real->double-flonum 9))
 3.3626845f-27
 (make-rectangular 1 3.95252516673e-323))
(log (make-rectangular +inf.f +inf.f))
(/ (make-rectangular +inf.0 1))
(/
 (/
  9.8813129168249e-324
  (make-rectangular 4 -1.7976931348623145e+308)
  (add1 (exact-round -0.0f0))))
(/ (make-polar -0.0 -0.0))
(+
 (- 6.4228533959362e-323 2)
 (make-polar -1.7976931348623157e+308 8.055967f+24))
(/
 (sinh (sqr 2))
 (make-rectangular
  (max
   (real->double-flonum 21/40)
   (real->double-flonum 2)
   (real->double-flonum -1))
  (/ 5.928787750095e-323 -15/13))
 (unsafe-fl*
  (real->double-flonum 1)
  (flatan (real->double-flonum 7.9906573f+31))))
(min
 (integer-length (exact-round 6))
 (flexpt (real->double-flonum -0.0f0) (real->double-flonum -1)))
(/ 0.0 (make-rectangular +inf.0 -7/2))
(/
 -4.138431f+16
 (+
  (make-rectangular -1.350358540579664e-118 +inf.0)
  (flcos (real->double-flonum 1))
  (tan 15)))
(max 0 (bitwise-and (exact-round 9.574622f+23) (exact-round -1)))
(/
 (make-rectangular
  (/ (real->double-flonum 3.9599206f-16))
  (unsafe-flabs (real->double-flonum 9.8813129168249e-324))))
(/ (make-rectangular (floor 4.9406564584125e-324) -0.0))
(/ (make-rectangular 3 -inf.0))
(/ (* 1/4 (make-rectangular 3/4 -inf.0)))
(/ (make-rectangular -inf.0 1.5261126408157696e+164))
(log (make-rectangular +inf.f -0.908023f0))
(/
 (* (make-rectangular +inf.0 1) (+ 4))
 (fl-
  (real->double-flonum 6.578783783780891e+298)
  (real->double-flonum -2.06074474904966e+82))
 3)
(min (flexpt (real->double-flonum -0.0) (real->double-flonum -3)) (add1 0))
(/ (make-rectangular 1.0118036f0 4.9406564584125e-324))
(*
 1.4229359f-35
 (truncate (exact-round 4))
 (make-rectangular
  31/5
  (*
   (real->double-flonum 2)
   (real->double-flonum -inf.0)
   (real->double-flonum +nan.f))))
(+
 (make-polar 1.7976931348623157e+308 -128012.13f0)
 (unsafe-fl-
  (real->double-flonum 4.9406564584125e-324)
  (real->double-flonum -1.003056287745491)))
(/
 (+
  (max (real->double-flonum 23.939516f0))
  (make-polar -1.1404732f-16 -4474.371f0))
 1
 (tan (real->double-flonum 3.703682f0)))
(/
 (make-rectangular
  (*
   (real->double-flonum 0.0f0)
   (real->double-flonum 3)
   (real->double-flonum +nan.f))
  (/ (real->double-flonum 1.5019045589416252e-161)))
 (round (real->double-flonum 1))
 (abs (real->double-flonum 1.797693134862315e+308)))
(/ (make-rectangular -inf.0 0.0) (tanh (add1 0)) -inf.0)
(/ (make-rectangular 6.182307412485337e+191 +nan.f) 4 1)
(* (exp +inf.0) 9.8813129168249e-324 (make-rectangular 0.0 2))
(/
 (make-rectangular 1 +inf.0)
 (unsafe-fl* (real->double-flonum -0.08482114f0) (real->double-flonum 0)))
(- (make-rectangular +inf.0 -1) (* 0 +inf.f))
(/
 (make-polar +inf.0 0.0)
 -1.6486595f-17
 (max (real->double-flonum -1.10778723613903e+146)))
(/ (min 4 -1) (make-rectangular -1.7976931348623157e+308 -0.48362154f0))
(/
 (*
  (make-rectangular +nan.f -1.6753775871463176e-193)
  (ceiling -1.7976931348623155e+308))
 (fl* (real->double-flonum 2) (real->double-flonum 0)))
(/
 (unsafe-flmin (real->double-flonum 1) (real->double-flonum 5/8))
 (/ 1 -32/3)
 (make-rectangular 1.4821969375237e-323 3))
(/ (make-rectangular 4 +inf.0))
(/
 (cosh (make-rectangular 2.7238666f+38 -2.7298811032891e-78))
 (unsafe-flsqrt (real->double-flonum 1.7976931348623157e+308)))
(/
 (make-polar
  (max (real->double-flonum 0.0) (real->double-flonum -240026434170169.5))
  (- (real->double-flonum 3) (real->double-flonum 1/2))))
(+
 (exp -7.7010645f-06)
 2.3587394f-23
 (make-rectangular 8.450573606704089e-141 1))
(/
 (make-rectangular (abs +inf.0) 1)
 (flround (flfloor (real->double-flonum 9.063922f+14)))
 (-
  (flatan (real->double-flonum 3.4584595208887e-323))
  (real->double-flonum 2)
  (real->double-flonum 1.0189711f0)))
(/ (make-rectangular -1.8358519442157564 (floor +nan.0)) 9)
(/ (make-rectangular 2 +nan.0) 2.330923f-33 -0.3978028959794585)
(+
 (bitwise-and (exact-round -1) (exact-round 8.017544067804913e+303))
 (truncate (exact-round 0.0f0)))
(/
 (make-rectangular 3 -0.0)
 2.9643938750475e-323
 (fl- (real->double-flonum 4) (real->double-flonum -4)))
(floor
 (bitwise-and
  (bitwise-xor (exact-round 1) (exact-round 4.3507125f+19))
  (+ (exact-round -4))))
(/ (make-rectangular -5.919837783850634e+144 +inf.0))
(floor
 (bitwise-and (floor (exact-round -3)) (exact-round -1.6259864356593537e+109)))
(/ 7.8307104f0 (make-rectangular 2602856.5f0 +inf.0))
(log (make-rectangular 0.60501945f0 -inf.f))
(-
 (flfloor (flfloor (real->double-flonum +inf.f)))
 (* (ceiling (exact-round -27/8)) 0 (fllog (real->double-flonum -2/3)))
 (make-rectangular 9.797624f0 (- (real->double-flonum -0.0f0))))
(*
 5
 +inf.0
 (cosh (make-rectangular -1.7976931348623153e+308 -1.7976931348623157e+308)))
(/
 (make-rectangular +inf.0 -5.1303835f0)
 (max (exact-round 1.4821969375237e-323) (exact-round 4)))
(/
 (cosh (make-rectangular -inf.0 -1.685847f-33))
 (flsin (real->double-flonum -3)))
(/
 (make-polar -inf.0 -8.389017008941602e+214)
 (+ -9.911437636709605e+41)
 (max (exact-round 2.8164606531844925) (exact-round 3)))
(/
 (add1 (make-rectangular +nan.f 2.4703282292062e-323))
 (min
  (*
   (real->double-flonum 16)
   (real->double-flonum -6.5616974f+10)
   (real->double-flonum -3.1764786f0))
  (- 4 6.4228533959362e-323 8/15)
  -17.72646330884845))
(/ (make-rectangular 1.7665469642760404e+304 -inf.0))
(* (bitwise-and (exact-round 5.9043345f+30) (sub1 (exact-round 0))))
(+
 (make-polar 8.70392880401687 0.0047191866f0)
 (tan (real->double-flonum 4.9406564584125e-324)))
(/ (make-rectangular -2.581724777672763 +inf.0) (* +inf.0 -1/7))
(/ (make-rectangular +nan.0 9.8813129168249e-324) (floor -3.5503556f-28))
(* (flsqrt (real->double-flonum 0)) 2 (make-rectangular -inf.0 7))
(/ (make-rectangular +inf.0 2.7017850815907883e+25) (sub1 -inf.f))
(log (make-polar -inf.f 8.599268f-22))
(/ (make-rectangular 4/5 1.797693134862315e+308))
(/
 (make-rectangular -1.266485546972744e-185 -inf.f)
 (+
  (real->double-flonum 4.5108434f-10)
  (real->double-flonum 1.1956617400792416e+205))
 -inf.f)
(+
 (floor (+ (exact-round -25.263502f0) (exact-round -1/2)))
 (- (min (real->double-flonum 0)) 16 (make-rectangular -inf.0 0.0))
 +nan.0)
(/
 (sqr 3)
 (make-rectangular 1.7976931348623151e+308 (cos 1.7976931348623147e+308)))
(ceiling (flexpt (real->double-flonum -0.0f0) (real->double-flonum -11)))
(/ (make-rectangular -65578.32f0 -inf.0) 1 -5.065710475407155e+64)
(/
 (sub1 0.8550388f0)
 (+
  (+ 5.5293337f-15 1.6585703248349453)
  (gcd (exact-round 0) (exact-round 1))
  (+ (real->double-flonum 2) (real->double-flonum -1/2)))
 (sin (make-rectangular 0.0 8.3991159793012e-323)))
(/
 3.2993203f+37
 (floor -2.2441852f0)
 (make-polar 0.42484267570553375 4.940078147009648))
(ceiling (bitwise-and (exact-round 8.48463f+10) (exact-round -1)))
(/
 (make-rectangular
  (- (real->double-flonum 2.3104787099047715e+119))
  (+ 2.2895064f0 0.5689070788001137 -1.7976931348623131e+308))
 (fllog (fl+ (real->double-flonum 1) (real->double-flonum 9.2728725f-10)))
 5)
(-
 (+ 1 -0.17853218f0)
 (bitwise-ior (max (exact-round 3)) (exact-round 0.0f0))
 (make-rectangular (log 7.4109846876187e-323) (abs -inf.0)))
(/
 (flcos (real->double-flonum 0))
 (make-rectangular -1.7976931348623157e+308 1.4821969375237e-323))
(/ (make-rectangular -inf.0 -1.7976931348623153e+308))
(* (bitwise-and (exact-round -3) (exact-round -1.7976931348623157e+308)))
(ceiling (bitwise-and (exact-round 1.0015169f+14) (exact-round -1)))
(/ (make-rectangular 1.7976931348623157e+308 +inf.0))
(/
 (make-rectangular
  (fllog (real->double-flonum 0))
  (min 1.797693134862312e+308)))
(/
 (* -5/4 -1 -1/2)
 (flmax (real->double-flonum -14/59) (real->double-flonum +inf.0))
 (make-rectangular -inf.0 -6.6653183907507665e+125))
(/ (make-rectangular 5.2141701976361275e+241 +inf.f))
(min
 (flexpt
  (real->double-flonum -0.0f0)
  (min
   (real->double-flonum -3)
   (real->double-flonum 24)
   (real->double-flonum 1.0947034325260547e-254))))
(/
 (/ (real->double-flonum -1.4275423f0))
 (make-rectangular -2.85069689139348e-54 -1.7976931348623151e+308))
(truncate
 (bitwise-and
  (- (exact-round 0) (exact-round 4))
  (ceiling (exact-round 7.27468f+19))))
(/
 (* (make-rectangular +nan.0 3/2))
 (unsafe-flsqrt (real->double-flonum 9))
 (unsafe-fl/
  (real->double-flonum +inf.0)
  (real->double-flonum -1.7976931348623153e+308)))
(/
 (cosh (make-rectangular -4.9474984f+19 -4.76630879186772e-155))
 (+ 2.6386206f-06))
(/
 (round -1)
 (flmin
  (real->double-flonum -1.7976931348623151e+308)
  (real->double-flonum -1/5))
 (make-rectangular -inf.f 7.165422052747097e+265))
(/
 (+ (real->double-flonum 0.0f0))
 (unsafe-fl+ (real->double-flonum -2/3) (real->double-flonum 10))
 (make-rectangular 5.4347221042537e-323 -inf.0))
(*
 2
 (make-rectangular
  (unsafe-fl* (real->double-flonum 1) (real->double-flonum 1))
  (min
   (real->double-flonum -inf.0)
   (real->double-flonum -26.347204f0)
   (real->double-flonum 3))))
(floor (bitwise-and (exact-round -1) (exact-round -1.7976931348623147e+308)))
(*
 (make-rectangular (* +inf.0 -13/10 3) 23)
 (- (real->double-flonum 0))
 (cosh (max 5 0 0)))
(*
 (make-rectangular
  (flsin (real->double-flonum -inf.f))
  (bitwise-ior (exact-round 0) (exact-round -1)))
 2
 (+ 0.18365704f0))
(*
 (+ (exact-round 5))
 (make-polar
  (unsafe-fl* (real->double-flonum -inf.f) (real->double-flonum -5/2))
  (sin (real->double-flonum 2))))
(*
 (cosh (cosh (make-rectangular 17.431166f0 1.0615060122404165)))
 (fltruncate (unsafe-fl* (real->double-flonum 1) (real->double-flonum -3))))
(*
 (fltan (flsqrt (real->double-flonum 0)))
 (make-rectangular (- -5.9265747f0 +nan.0) 1.0339303925070463e-232))
(/
 (make-rectangular +nan.0 1)
 (-
  (real->double-flonum 1)
  (real->double-flonum 0.06738663f0)
  (real->double-flonum 7.9050503334599e-323)))
(* (make-rectangular 4 +inf.0) 2 (add1 (exact-round 0.8167418f0)))
(*
 (make-rectangular 1.7976931348623151e+308 14/3)
 (+
  (real->double-flonum 3)
  (real->double-flonum -6.994152f0)
  (real->double-flonum 1))
 1)
(/
 (make-rectangular 9.5256185f-35 -inf.0)
 (+
  (real->double-flonum 7)
  (real->double-flonum 3)
  (real->double-flonum 1.976262583365e-323))
 (unsafe-fl+ (real->double-flonum 0) (real->double-flonum 3)))
(ceiling (bitwise-and (exact-round 7.949443893444532e+177) (exact-round -1)))
(*
 (make-rectangular 0.0 +inf.0)
 (ceiling (real->double-flonum -4.1434605f+37))
 (min (real->double-flonum 0) (real->double-flonum 2)))
(*
 -0.0
 (/ (make-rectangular -4.9406564584125e-324 0.02826022f0))
 -0.03135514535167072)
(*
 (make-rectangular 1.7976931348623155e+308 1.7976931348623157e+308)
 (- 1 -inf.0)
 6)
(/
 (min -1.4811229f-36 0.0f0)
 (+ (real->double-flonum -5/2))
 (make-rectangular +inf.0 0.5895755447157006))
(truncate (bitwise-and (exact-round -2) (exact-round -1.7021951f+12)))
(* (floor 6) (make-rectangular 8.3991159793012e-323 +inf.f))
(*
 (fl+
  (real->double-flonum -1.302416064314125e-293)
  (real->double-flonum 4.9406564584125e-324))
 (make-rectangular -9.038323f+37 -inf.0))
(*
 (make-rectangular 2.644920489625189 +nan.f)
 (unsafe-flsqrt (real->double-flonum 5)))
(/
 2
 (make-rectangular 1.7976931348623155e+308 1)
 (sub1 (real->double-flonum 0.034460585393126276)))
(/
 (flmin (real->double-flonum -15) (real->double-flonum -51.424126f0))
 (add1 4)
 (make-rectangular 1.976262583365e-323 2))
(-
 (cos (min (exact-round -9/5) (exact-round 7)))
 (/
  3.9495411046506046
  (flceiling (real->double-flonum 3))
  (make-rectangular -1/4 1.4821969375237e-323)))
(-
 (/
  -0.3713432f0
  (make-rectangular 4.9406564584125e-324 -1.797693134862315e+308))
 (flexpt (max (real->double-flonum 1.4994744f-21)) (real->double-flonum -3)))
(/ (+ 0.058817342f0) (* (make-rectangular 3 -inf.0) (* 1/2 2/7 0.86800987f0)))
(max
 (bitwise-ior
  (bitwise-and (exact-round -2) (exact-round 7.669307791808228e+258))
  (bitwise-xor (exact-round 0))
  (exact-round 0.0f0)))
(+
 (* (bitwise-ior (exact-round 1) (exact-round -1.3416489f0) (exact-round 1)))
 (min 4/3 (+ (exact-round 1.7976931348623105e+308)))
 (make-rectangular
  (max (real->double-flonum 0.0))
  (min 3.966231810699627 -1.9801397340231746 -1/4)))
(/ (make-rectangular -0.47108743f0 -inf.0) (max 1.7976931348623155e+308 -2 0))
(ceiling (flexpt (real->double-flonum -0.0f0) (real->double-flonum -5)))
(min (flexpt (real->double-flonum -0.0) (real->double-flonum -3)) 0)
(*
 (unsafe-fl/ (real->double-flonum 0) (real->double-flonum -8.045241f+20))
 (make-rectangular -inf.0 -5)
 (max (exact-round 3) (exact-round 14)))
(*
 (/
  (max (real->double-flonum -1/3))
  (/ (real->double-flonum -1.7976931348623153e+308) (real->double-flonum 3)))
 (min
  (min -1.7976931348623155e+308 5)
  (min (real->double-flonum 9) (real->double-flonum -3.562195657145329e-280))
  (unsafe-fl*
   (real->double-flonum -1)
   (real->double-flonum 1.9587062593936307e-295)))
 (make-rectangular
  (flcos (real->double-flonum +nan.0))
  (/ (real->double-flonum 13) (real->double-flonum -1))))
(/
 (unsafe-flsqrt (real->double-flonum 7))
 (make-polar
  (unsafe-fl* (real->double-flonum 0) (real->double-flonum 1.3238051f-30))
  (ceiling (real->double-flonum 3))))
(*
 (make-rectangular -12.170922722958942 -1.7976931348623145e+308)
 -10.408284f0
 (unsafe-fl+ (real->double-flonum 4) (real->double-flonum 2.648344f+07)))
(* (tanh (sub1 2.2812761f+24)) (make-rectangular +nan.0 8.968231276783684e+62))
(/
 (max (exact-round -3) (exact-round 1))
 2
 (make-polar 0.0 (fltan (real->double-flonum 0))))
(*
 (make-rectangular
  (+ (real->double-flonum -2.139004f+35))
  (- 0.5136155697966388 7.609600636085995 -inf.0))
 (+
  (flsin (real->double-flonum 0))
  (+ -17/2 -1.505352392179396e-214 4.9406564584125e-324)))
(*
 (round (min (real->double-flonum 3/2)))
 (make-polar
  (min
   (real->double-flonum 1.7485642f+27)
   (real->double-flonum -1.7976931348623155e+308)
   (real->double-flonum 1.9078022f0))
  (sub1 (real->double-flonum 1.5208878098163798e+223)))
 (sin (real->double-flonum -5/13)))
(*
 (min (max -0.0f0 +inf.0 -7/11))
 -4/13
 (make-rectangular -1.7976931348623157e+308 +inf.0))
(+ (bitwise-and (exact-round -1) (exact-round -8.831564405434159e+135)))
(* (make-rectangular -inf.0 -3) (sin (real->double-flonum 8)))
(max (bitwise-and (exact-round 2.5832200494955715e+82) (exact-round -4)) -6)
(/
 (make-rectangular -9.8813129168249e-324 2)
 (sqr (real->double-flonum 0.0f0))
 -1/2)
(log (log (make-rectangular -inf.f +inf.f)))
(/
 (cos (real->double-flonum 2))
 (make-rectangular
  (flmin (real->double-flonum -7.4815276f-19) (real->double-flonum 3/2))
  (ceiling -1.7976931348623157e+308)))
(+ (flexpt (real->double-flonum -0.0f0) (flfloor (real->double-flonum -1))) 0)
(sinh (flexpt (real->double-flonum -0.0f0) (real->double-flonum -1)))
(*
 (lcm (bitwise-not (exact-round 8)))
 (make-rectangular 1.9982868116199189e+71 (add1 (real->double-flonum +inf.0))))
(sqrt
 (flexpt
  (real->double-flonum -0.0)
  (floor (real->double-flonum -2.8978367843122523))))
(/
 (real->single-flonum 0)
 (make-rectangular
  (sub1 -0.013669635f0)
  (unsafe-flsqrt (real->double-flonum +inf.f)))
 (/ (real->single-flonum 0)))
(*
 (sqr (make-rectangular +inf.0 -0.0))
 5
 (fl+
  (unsafe-flsqrt (real->double-flonum 0.17498136f0))
  (real->double-flonum 6.5786315f+16)))
(log (make-rectangular (real->single-flonum +inf.f) 5335.7827f0))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(expt
 (real->single-flonum -0.0)
 (flmin (real->double-flonum -3) (ceiling (real->double-flonum -1/10))))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(truncate (round (abs (real->double-flonum -0.0f0))))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs -0.0f0)
(abs (flround (ceiling (real->double-flonum -0.0))))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (abs (truncate (real->double-flonum -0.0f0))))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0)
(abs (truncate (unsafe-flsqrt (real->double-flonum -0.0))))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (sqrt (round -0.0)))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs (abs (floor (real->double-flonum -0.0))))
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(expt
 (real->single-flonum -8.665778974912815e+107)
 (bitwise-not (exact-round 6.774601151951068e+128)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(floor (abs (real->double-flonum -0.0)))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(truncate (abs (sqrt -0.0f0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(expt
 (sin +inf.f)
 (make-polar
  (fltruncate (real->double-flonum 2.531945015125194e+76))
  (flceiling (real->double-flonum +inf.0))))
(abs -0.0f0)
(abs -0.0)
(add1 (fl/ (real->double-flonum 1/4) (real->double-flonum -0.0f0)))
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (flceiling (flsqrt (real->double-flonum -0.0f0))))
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->double-flonum (real->single-flonum -0.0))))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->double-flonum (real->single-flonum -0.0f0))))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(expt (real->single-flonum -0.0) (bitwise-not (bitwise-ior)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(/
 (gcd (sqr (exact-round -1)) (integer-length (exact-round 0)))
 (real->single-flonum -0.0)
 (flexp (flfloor (real->double-flonum 1))))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (round (real->double-flonum -0.0f0)))
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->single-flonum -0.0))
(sqrt
 (fl/ (real->double-flonum 1.3207776839674341) (real->double-flonum -0.0)))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(/
 (real->single-flonum -2.0240654f+33)
 (+
  (fl/ (real->double-flonum 0) (real->double-flonum 2))
  (truncate (real->double-flonum -0.0))
  (flasin (real->double-flonum 0.0f0)))
 (make-rectangular (flasin (real->double-flonum 0)) 5)
 (*)
 (ceiling (real->double-flonum -1.0104585998150004e+126)))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs
 (sqrt
  (unsafe-fl/
   (real->double-flonum 1.554261f-24)
   (real->double-flonum -0.0f0))))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(truncate (bitwise-and (bitwise-and) (exact-round -1.7976931348623155e+308)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0)
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs -0.0)
(abs -0.0)
(abs (real->double-flonum (real->single-flonum -0.0f0)))
(abs -0.0)
(abs -0.0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs (real->single-flonum -0.0f0))
(abs (real->single-flonum -0.0))
(abs (real->double-flonum (real->single-flonum -0.0)))
(abs -0.0f0)
(abs (real->single-flonum -0.0))
(abs -0.0f0)
(abs -0.0)
(abs (real->single-flonum -0.0f0))
(abs -0.0)
(abs -0.0f0)
(add1
 (fltruncate
  (unsafe-fl/ (real->double-flonum 2) (real->double-flonum -0.0f0))))
(expt
 (make-polar (floor 6.468476f+31) (tanh +nan.f))
 (flexpt
  (sub1 (real->double-flonum 0))
  (real->double-flonum 7.155251486063204)))
(tanh (fl/ (real->double-flonum 2) (real->double-flonum -0.0f0)))
(tanh (unsafe-fl/ (real->double-flonum +inf.0) (real->double-flonum -0.0f0)))
(add1 (fl/ (real->double-flonum 2) (real->double-flonum -0.0)))
(log
 (make-rectangular
  (real->single-flonum 5/4)
  (real->single-flonum -1.7976931348623157e+308)))
(round
 (bitwise-and (exact-round -1.7976931348623155e+308) (round (exact-round -1))))
(expt (real->single-flonum -0.0f0) (bitwise-and))
(round (expt -0.0f0 -1))
(expt (sinh -0.0f0) (floor (real->double-flonum -1/9)))
(add1 (fl/ (ceiling (real->double-flonum 1)) (real->double-flonum -0.0)))
(add1
 (unsafe-fl-
  (fl/ (real->double-flonum 2.27068f-20) (real->double-flonum -0.0f0))
  (real->double-flonum 0)))
(expt (tanh (real->single-flonum -0.0f0)) (bitwise-not (exact-round 0)))
(expt
 (sub1 (real->single-flonum 0))
 (ceiling (exact-round 1.7976931348623157e+308)))
(expt
 (real->double-flonum (real->single-flonum -9/5))
 (sqr (exact-round 1.797693134862314e+308)))
(expt (real->single-flonum -0.0f0) (bitwise-and))
(tanh (unsafe-fl/ (real->double-flonum 3) (real->double-flonum -0.0)))
(expt -0.0f0 (floor -2/3))
(sqrt
 (fl/
  (real->double-flonum 2.2660296f0)
  (flceiling (real->double-flonum -0.0f0))))
(/ (inexact->exact (real->single-flonum 1/2)) -0.0)
(expt
 (real->single-flonum -5.6356673f0)
 (truncate (exact-round -8.2161651820900135e+242)))
(* (flround (real->double-flonum -21742.229f0)) (make-rectangular +nan.0 3))
(expt
 (unsafe-flsqrt (real->double-flonum -1))
 (make-polar (real->single-flonum 6.821443f+27) (real->single-flonum +nan.0)))
(tanh (fl/ (real->double-flonum 0.12224905f0) (real->double-flonum -0.0f0)))
(sinh
 (expt (real->single-flonum -1) (sqr (exact-round -1.360011780194363e+298))))
(ceiling (fl/ (real->double-flonum 5) (real->double-flonum -0.0f0)))
(sinh
 (fl/ (real->double-flonum 5.928787750095e-323) (real->double-flonum -0.0f0)))
(sqrt
 (fl+
  (real->double-flonum 15)
  (unsafe-fl/ (real->double-flonum 9) (real->double-flonum -0.0f0))))
(sqrt (* (fl/ (real->double-flonum +inf.0) (real->double-flonum -0.0))))
(max
 (unsafe-fl/
  (real->double-flonum 0.006261224681179065)
  (real->double-flonum -0.0f0)))
(expt
 (make-polar
  (unsafe-flabs (real->double-flonum -0.0f0))
  (round (real->double-flonum +inf.f)))
 (flsqrt (unsafe-flsqrt (real->double-flonum -1.7976931348623141e+308))))
(expt
 (ceiling (real->single-flonum -1650923.9f0))
 (lcm (exact-round -1.7976931348623143e+308)))
(sinh
 (unsafe-fl/
  (real->double-flonum 1.976262583365e-323)
  (real->double-flonum -0.0f0)))
(ceiling
 (unsafe-fl/ (abs (real->double-flonum 2)) (real->double-flonum -0.0f0)))
(* (make-rectangular -inf.f -1.0392582850499196e-255) (sinh -3018.1985f0))
(/
 (make-rectangular 8 +inf.0)
 -0.784267146771867
 (ceiling (sqr (real->double-flonum -9/11))))
(min
 (real->double-flonum (real->double-flonum (real->single-flonum 7)))
 4.4015314f-31
 (sinh 2.3485355f+11)
 (fl/ (sqr (real->double-flonum 5)) (real->double-flonum -0.0f0))
 (lcm (integer-length (exact-round 0.0))))
(*
 -5/4
 (*
  (real->single-flonum -1.1593805f+36)
  (make-rectangular 4395013039504391.5 +nan.0)
  1)
 (real->single-flonum 21/34)
 (exp (floor (exact-round 12))))
(expt
 (sub1 (real->double-flonum +nan.f))
 (make-polar (real->single-flonum 3991870.8f0) (sin -1.1223053f-17)))
(max
 (unsafe-fl/
  (real->double-flonum 7.020644089360399e-65)
  (real->double-flonum -0.0)))
(/
 (gcd (exact-round 2) (exact-round -4))
 (real->single-flonum -0.0f0)
 (round 2))
(ceiling
 (unsafe-fl/
  (fl+ (real->double-flonum 4) (real->double-flonum 2))
  (real->double-flonum -0.0f0)))
(*
 (flceiling (real->double-flonum 0))
 (make-rectangular -2.006870542357445 +nan.0)
 (real->single-flonum -3/11))
(expt
 (min -11.741571f0 (ceiling 9.8813129168249e-324))
 (sqr (lcm (exact-round -1.7976931348623141e+308) (exact-round -8))))
(expt
 (make-rectangular
  (add1 (real->double-flonum -178940.67079864573))
  (abs 9.8813129168249e-324))
 (flacos (real->double-flonum 2.1266366f+27)))
(expt (make-rectangular -1/2 1.7976931348623141e+308) (cosh +nan.f))
(log
 (flmin
  (real->double-flonum 3)
  (unsafe-fl/ (real->double-flonum 1/8) (real->double-flonum -0.0f0))))
(tanh (unsafe-fl/ (real->double-flonum 2) (real->double-flonum -0.0f0)))
(ceiling
 (fl/
  (real->double-flonum 1.2520394961614763e-248)
  (real->double-flonum -0.0)))
(/ (make-rectangular 4.580968352558739 -inf.0) 3/5)
(expt
 -2.1172982f-30
 (+ (exact-round -1.444196394736778e+279) (exact-round 3/2)))
(add1
 (unsafe-fl/ (real->double-flonum 9.195241f+30) (real->double-flonum -0.0)))
(expt
 (real->single-flonum -7.694171f0)
 (round (exact-round 2.905017610887334e+78)))
(add1 (fl/ (real->double-flonum 0.49066553f0) (real->double-flonum -0.0f0)))
(ceiling (fl/ (real->double-flonum 5) (real->double-flonum -0.0f0)))
(expt
 (fl/ (real->double-flonum 10/11) (real->double-flonum -0.0))
 -2.9323564f0)
(expt -0.0f0 -1)
(/
 (arithmetic-shift (exact-round 3.5562148232740016e+47) (exact-round 1/2))
 (real->double-flonum (real->single-flonum -0.0f0)))
(expt (real->single-flonum -0.0f0) (bitwise-and))
(log
 (make-rectangular
  (real->single-flonum -inf.0)
  (real->single-flonum -3.9914962571825976e+240)))
(expt -4.8165456f+29 (lcm (exact-round -1.7976931348623151e+308)))
(/
 (round (exact-round -2.7393196f0))
 (real->double-flonum (inexact->exact (real->single-flonum -0.0))))
(log (make-rectangular (real->single-flonum 3.8130515f0)
                       (real->single-flonum -2.8845456304823365e+289)))
(/ (abs (exact-round 1.8327790416478524))
   (unsafe-flsqrt (real->double-flonum -0.0)) 1)
(- (real->single-flonum +nan.0)
   (bitwise-xor)
   (make-polar (flatan (real->double-flonum 0.0))
               (flsin (real->double-flonum 0)))
   (flacos (sin (real->double-flonum 4)))
   (real->double-flonum (real->single-flonum -1.5582163f-23))
   (flsqrt (real->double-flonum 0))
   (make-polar (tan (real->double-flonum +inf.0)) (real->single-flonum 0.0))
   (flmin (real->double-flonum 0)
          (fl+ (real->double-flonum 5.2413393729292645e-161)
               (real->double-flonum 1)))
   (real->single-flonum -7.370759632814253e+46))
(expt -3.8097968f0
      (lcm (exact-round 4)
           (exact-round 1.7976931348623151e+308)
           (exact-round 3311118.8f0)))
(+ (sqrt (make-polar -1.3828875524272957e+166 -0.12420556f0))
   -7.977045912134898e+298)
    ))

(parameterize ([current-output-port (current-error-port)])
  (for ([c counterexamples])
    (unless (check-all-reals c)
      (displayln c)
      (newline))))
