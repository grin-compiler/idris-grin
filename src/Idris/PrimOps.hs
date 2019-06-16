{-# LANGUAGE QuasiQuotes #-}
module Idris.PrimOps where

import Grin.Grin
import Grin.TH
import Grin.PrimOpsPrelude


idrisPrimOps = withPrimPrelude [prog|
  idris_int_eq idris_int_eq0 idris_int_eq1 =
    (CGrInt idris_int_eq0_1) <- fetch idris_int_eq0
    (CGrInt idris_int_eq1_1) <- fetch idris_int_eq1
    idris_int_eq2 <- _prim_int_eq idris_int_eq0_1 idris_int_eq1_1
    case idris_int_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_double_eq idris_double_eq0 idris_double_eq1 =
    (CGrDouble idris_double_eq0_1) <- fetch idris_double_eq0
    (CGrDouble idris_double_eq1_1) <- fetch idris_double_eq1
    idris_double_eq2 <- _prim_double_eq idris_double_eq0_1 idris_double_eq1_1
    case idris_double_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_lt idris_int_lt0 idris_int_lt1 =
    (CGrInt idris_int_lt0_1) <- fetch idris_int_lt0
    (CGrInt idris_int_lt1_1) <- fetch idris_int_lt1
    idris_int_lt2 <- _prim_int_lt idris_int_lt0_1 idris_int_lt1_1
    case idris_int_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_word_lt idris_word_lt0 idris_word_lt1 =
    (CGrWord idris_word_lt0_1) <- fetch idris_word_lt0
    (CGrWord idris_word_lt1_1) <- fetch idris_word_lt1
    idris_word_lt2 <- _prim_word_lt idris_word_lt0_1 idris_word_lt1_1
    case idris_word_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_float_lt idris_float_lt0 idris_float_lt1 =
    (CGrFloat idris_float_lt0_1) <- fetch idris_float_lt0
    (CGrFloat idris_float_lt1_1) <- fetch idris_float_lt1
    idris_float_lt2 <- _prim_float_lt idris_float_lt0_1 idris_float_lt1_1
    case idris_float_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_double_lt idris_double_lt0 idris_double_lt1 =
    (CGrDouble idris_double_lt0_1) <- fetch idris_double_lt0
    (CGrDouble idris_double_lt1_1) <- fetch idris_double_lt1
    idris_double_lt2 <- _prim_double_lt idris_double_lt0_1 idris_double_lt1_1
    case idris_double_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_double_log idris_double_log1 =
    (CGrDouble idris_double_log1_0) <- fetch idris_double_log1
    idris_double_log2 <- _prim_double_log idris_double_log1_0
    pure (CGrDouble idris_double_log2)

  idris_double_sin idris_double_sin1 =
    (CGrDouble idris_double_sin1_0) <- fetch idris_double_sin1
    idris_double_sin2 <- _prim_double_sin idris_double_sin1_0
    pure (CGrDouble idris_double_sin2)

  idris_double_cos idris_double_cos1 =
    (CGrDouble idris_double_cos1_0) <- fetch idris_double_cos1
    idris_double_cos2 <- _prim_double_cos idris_double_cos1_0
    pure (CGrDouble idris_double_cos2)

  idris_double_tan idris_double_tan1 =
    (CGrDouble idris_double_tan1_0) <- fetch idris_double_tan1
    idris_double_tan2 <- _prim_double_tan idris_double_tan1_0
    pure (CGrDouble idris_double_tan2)

  idris_double_asin idris_double_asin1 =
    (CGrDouble idris_double_asin1_0) <- fetch idris_double_asin1
    idris_double_asin2 <- _prim_double_asin idris_double_asin1_0
    pure (CGrDouble idris_double_asin2)

  idris_double_acos idris_double_acos1 =
    (CGrDouble idris_double_acos1_0) <- fetch idris_double_acos1
    idris_double_acos2 <- _prim_double_acos idris_double_acos1_0
    pure (CGrDouble idris_double_acos2)

  idris_double_atan idris_double_atan1 =
    (CGrDouble idris_double_atan1_0) <- fetch idris_double_atan1
    idris_double_atan2 <- _prim_double_atan idris_double_atan1_0
    pure (CGrDouble idris_double_atan2)

  idris_double_atan_two idris_double_atan_two1 idris_double_atan_two2 =
    (CGrDouble idris_double_atan_two1_0) <- fetch idris_double_atan_two1
    (CGrDouble idris_double_atan_two2_0) <- fetch idris_double_atan_two2
    idris_double_atan_two3 <- _prim_double_atan2 idris_double_atan_two1_0 idris_double_atan_two2_0
    pure (CGrDouble idris_double_atan_two3)

  idris_double_sqrt idris_double_sqrt1 =
    (CGrDouble idris_double_sqrt1_0) <- fetch idris_double_sqrt1
    idris_double_sqrt2 <- _prim_double_sqrt idris_double_sqrt1_0
    pure (CGrDouble idris_double_sqrt2)

  idris_double_floor idris_double_floor1 =
    (CGrDouble idris_double_floor1_0) <- fetch idris_double_floor1
    idris_double_floor2 <- _prim_double_floor idris_double_floor1_0
    pure (CGrDouble idris_double_floor2)

  idris_double_ceil idris_double_ceil1 =
    (CGrDouble idris_double_ceil1_0) <- fetch idris_double_ceil1
    idris_double_ceil2 <- _prim_double_ceil idris_double_ceil1_0
    pure (CGrDouble idris_double_ceil2)

  idris_double_negate idris_double_negate1 =
    (CGrDouble idris_double_negate1_0) <- fetch idris_double_negate1
    idris_double_negate2 <- _prim_double_negate idris_double_negate1_0
    pure (CGrDouble idris_double_negate2)

  idris_int_le idris_int_le0 idris_int_le1 =
    (CGrInt idris_int_le0_1) <- fetch idris_int_le0
    (CGrInt idris_int_le1_1) <- fetch idris_int_le1
    idris_int_le2 <- _prim_int_le idris_int_le0_1 idris_int_le1_1
    case idris_int_le2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_word_le idris_word_le0 idris_word_le1 =
    (CGrWord idris_word_le0_1) <- fetch idris_word_le0
    (CGrWord idris_word_le1_1) <- fetch idris_word_le1
    idris_word_le2 <- _prim_word_le idris_word_le0_1 idris_word_le1_1
    case idris_word_le2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_int_gt idris_int_gt0 idris_int_gt1 =
    (CGrInt idris_int_gt0_1) <- fetch idris_int_gt0
    (CGrInt idris_int_gt1_1) <- fetch idris_int_gt1
    idris_int_gt2 <- _prim_int_gt idris_int_gt0_1 idris_int_gt1_1
    case idris_int_gt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_word_gt idris_word_gt0 idris_word_gt1 =
    (CGrWord idris_word_gt0_1) <- fetch idris_word_gt0
    (CGrWord idris_word_gt1_1) <- fetch idris_word_gt1
    idris_word_gt2 <- _prim_word_gt idris_word_gt0_1 idris_word_gt1_1
    case idris_word_gt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_ge idris_int_ge0 idris_int_ge1 =
    (CGrInt idris_int_ge0_1) <- fetch idris_int_ge0
    (CGrInt idris_int_ge1_1) <- fetch idris_int_ge1
    idris_int_ge2 <- _prim_int_ge idris_int_ge0_1 idris_int_ge1_1
    case idris_int_ge2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_word_ge idris_word_ge0 idris_word_ge1 =
    (CGrWord idris_word_ge0_1) <- fetch idris_word_ge0
    (CGrWord idris_word_ge1_1) <- fetch idris_word_ge1
    idris_word_ge2 <- _prim_word_ge idris_word_ge0_1 idris_word_ge1_1
    case idris_word_ge2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_lashr_int idris_lashr_int1 idris_lashr_int2 =
    (CGrInt idris_lashr_int1_0) <- fetch idris_lashr_int1
    (CGrInt idris_lashr_int2_0) <- fetch idris_lashr_int2
    idris_lashr_int3 <- _prim_int_ashr idris_lashr_int1_0 idris_lashr_int2_0
    pure (CGrInt idris_lashr_int3)

  idris_int_print idris_int_print0 =
    (CGrInt idris_int_print0_1) <- fetch idris_int_print0
    _prim_int_print idris_int_print0_1
    pure (CUnit)

  idris_double_add idris_double_add0 idris_double_add1 =
    (CGrDouble idris_double_add0_1) <- fetch idris_double_add0
    (CGrDouble idris_double_add1_1) <- fetch idris_double_add1
    idris_double_add3 <- _prim_double_add idris_double_add0_1 idris_double_add1_1
    pure (CGrDouble idris_double_add3)

  idris_double_sub idris_double_sub0 idris_double_sub1 =
    (CGrDouble idris_double_sub0_1) <- fetch idris_double_sub0
    (CGrDouble idris_double_sub1_1) <- fetch idris_double_sub1
    idris_double_sub3 <- _prim_double_sub idris_double_sub0_1 idris_double_sub1_1
    pure (CGrDouble idris_double_sub3)

  idris_double_mul idris_double_mul0 idris_double_mul1 =
    (CGrDouble idris_double_mul0_1) <- fetch idris_double_mul0
    (CGrDouble idris_double_mul1_1) <- fetch idris_double_mul1
    idris_double_mul3 <- _prim_double_mul idris_double_mul0_1 idris_double_mul1_1
    pure (CGrDouble idris_double_mul3)

  idris_int_add idris_int_add0 idris_int_add1 =
    (CGrInt idris_int_add0_1) <- fetch idris_int_add0
    (CGrInt idris_int_add1_1) <- fetch idris_int_add1
    idris_int_add2 <- _prim_int_add idris_int_add0_1 idris_int_add1_1
    pure (CGrInt idris_int_add2)

  idris_int_sub idris_int_sub0 idris_int_sub1 =
    (CGrInt idris_int_sub0_1) <- fetch idris_int_sub0
    (CGrInt idris_int_sub1_1) <- fetch idris_int_sub1
    idris_int_sub2 <- _prim_int_sub idris_int_sub0_1 idris_int_sub1_1
    pure (CGrInt idris_int_sub2)

  idris_int_mul idris_int_mul0 idris_int_mul1 =
    (CGrInt idris_int_mul0_1) <- fetch idris_int_mul0
    (CGrInt idris_int_mul1_1) <- fetch idris_int_mul1
    idris_int_mul2 <- _prim_int_mul idris_int_mul0_1 idris_int_mul1_1
    pure (CGrInt idris_int_mul2)

  idris_int_div idris_int_div0 idris_int_div1 =
    (CGrInt idris_int_div0_1) <- fetch idris_int_div0
    (CGrInt idris_int_div1_1) <- fetch idris_int_div1
    idris_int_div2 <- _prim_int_div idris_int_div0_1 idris_int_div1_1
    pure (CGrInt idris_int_div2)

  idris_double_div idris_double_div0 idris_double_div1 =
    (CGrDouble idris_double_div0_1) <- fetch idris_double_div0
    (CGrDouble idris_double_div1_1) <- fetch idris_double_div1
    idris_double_div2 <- _prim_double_div idris_double_div0_1 idris_double_div1_1
    pure (CGrDouble idris_double_div2)

  idris_write_str idris_write_str1 idris_write_str2 =
    (CGrString idris_write_str2_0) <- fetch idris_write_str2
    _prim_string_print idris_write_str2_0
    pure (CUnit)

  idris_read_str idris_read_str0 =
    -- This parameter will be an int that addresses the stream...
    idris_read_str1 <- _prim_read_string
    pure (CGrString idris_read_str1)

  idris_str_concat idris_str_concat1 idris_str_concat2 =
    (CGrString idris_str_concat1_0) <- fetch idris_str_concat1
    (CGrString idris_str_concat2_0) <- fetch idris_str_concat2
    idris_str_concat3 <- _prim_string_concat idris_str_concat1_0 idris_str_concat2_0
    pure (CGrString idris_str_concat3)

  idris_str_eq idris_str_eq1 idris_str_eq2 =
    (CGrString idris_str_eq1_0) <- fetch idris_str_eq1
    (CGrString idris_str_eq2_0) <- fetch idris_str_eq2
    idris_str_eq3 <- _prim_string_eq idris_str_eq1_0 idris_str_eq2_0
    pure (CGrInt idris_str_eq3)

  idris_str_lt idris_str_lt1 idris_str_lt2 =
    (CGrString idris_str_lt1_0) <- fetch idris_str_lt1
    (CGrString idris_str_lt2_0) <- fetch idris_str_lt2
    idris_str_lt3 <- _prim_string_lt idris_str_lt1_0 idris_str_lt2_0
    pure (CGrInt idris_str_lt3)

  idris_str_len idris_str_len1 =
    (CGrString idris_str_len2) <- fetch idris_str_len1
    idris_str_len3 <- _prim_string_len idris_str_len2
    pure (CGrInt idris_str_len3)

  idris_str_rev idris_str_rev1 =
    (CGrString idris_str_rev1_0) <- fetch idris_str_rev1
    idris_str_rev2 <- _prim_string_reverse idris_str_rev1_0
    pure (CGrString idris_str_rev2)

  idris_str_head idris_str_head1 =
    (CGrString idris_str_head1_0) <- fetch idris_str_head1
    idris_str_head2 <- _prim_string_head idris_str_head1_0
    pure (CGrInt idris_str_head2)

  idris_str_tail idris_str_tail1 =
    (CGrString idris_str_tail1_0) <- fetch idris_str_tail1
    idris_str_tail2 <- _prim_string_tail idris_str_tail1_0
    pure (CGrString idris_str_tail2)

  idris_str_cons idris_str_cons1 idris_str_cons2 =
    (CGrInt idris_str_cons1_0)    <- fetch idris_str_cons1
    (CGrString idris_str_cons2_0) <- fetch idris_str_cons2
    idris_str_cons3 <- _prim_string_cons idris_str_cons1_0 idris_str_cons2_0
    pure (CGrString idris_str_cons3)

  idris_int_str idris_int_str1 =
    (CGrInt idris_int_str1_0) <- fetch idris_int_str1
    idris_int_str2 <- _prim_int_str idris_int_str1_0
    pure (CGrString idris_int_str2)

  idris_str_int idris_str_int1 =
    (CGrString idris_str_int1_0) <- fetch idris_str_int1
    idris_str_int2 <- _prim_str_int idris_str_int1_0
    pure (CGrInt idris_str_int2)

  idris_int_double idris_int_double1 =
    (CGrInt idris_int_double1_0) <- fetch idris_int_double1
    idris_int_double2 <- _prim_int_double idris_int_double1_0
    pure (CGrDouble idris_int_double2)

  idris_double_int idris_double_int1 =
    (CGrDouble idris_double_int1_0) <- fetch idris_double_int1
    idris_double_int2 <- _prim_double_int idris_double_int1_0
    pure (CGrInt idris_double_int2)

  idris_double_str idris_double_str1 =
    (CGrDouble idris_double_str1_0) <- fetch idris_double_str1
    idris_double_str2 <- _prim_double_string idris_double_str1_0
    pure (CGrString idris_double_str2)

  idris_str_double idris_str_double1 =
    (CGrString idris_str_double1_0) <- fetch idris_str_double1
    idris_str_double2 <- _prim_string_double idris_str_double1_0
    pure (CGrDouble idris_str_double2)

  idris_ffi_file_eof idris_ffi_file_eof1 =
    (CGrInt idris_ffi_file_eof1_0) <- fetch idris_ffi_file_eof1
    idris_ffi_file_eof2 <- _prim_ffi_file_eof idris_ffi_file_eof1_0
    pure (CGrInt idris_ffi_file_eof2)

  idris_lz_ext idris_lz_ext1 =
    (CGrInt idris_lz_ext2) <- fetch idris_lz_ext1
    idris_lz_ext3 <- _prim_int_add idris_lz_ext2 0
    pure (CGrInt idris_lz_ext3)

  idris_ls_ext idris_ls_ext1 =
    (CGrInt idris_ls_ext2) <- fetch idris_ls_ext1
    idris_ls_ext3 <- _prim_int_add idris_ls_ext2 0
    pure (CGrInt idris_ls_ext3)

  -- TODO: the implementation here is wrong, needs to be fixed.
  idris_l_trunc idris_l_trunc1 =
    (CGrInt idris_l_trunc2) <- fetch idris_l_trunc1
    idris_l_trunc3 <- _prim_int_add idris_l_trunc2 0
    pure (CGrInt idris_l_trunc3)

  idris_ch_int idris_ch_int1 =
    (CGrInt idris_ch_int2) <- fetch idris_ch_int1
    pure (CGrInt idris_ch_int2)

  idris_int_ch idris_int_ch1 =
    (CGrInt idris_int_ch2) <- fetch idris_int_ch1
    pure (CGrInt idris_int_ch2)

  idris_usleep idris_usleep1 =
    (CGrInt idris_usleep1_0) <- fetch idris_usleep1
    _prim_usleep idris_usleep1_0
    pure () -- Maybe it needs another void like type?

  idris_error idris_error1 =
    _prim_error idris_error1

  prim__stdin =
    pure (CGrInt 0)

  prim__stdout =
    pure (CGrInt 1)

  prim__stderr =
    pure (CGrInt 2)

  grinMain =
    r <- "idr_{runMain_0}"
    pure ()
|]

