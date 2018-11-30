{-# LANGUAGE QuasiQuotes #-}
module Idris.PrimOps where

import Grin.Grin
import Grin.TH


idrisPrimOps = [prog|
  idris_int_eq idris_int_eq0 idris_int_eq1 =
    (CGrInt idris_int_eq0_1) <- fetch idris_int_eq0
    (CGrInt idris_int_eq1_1) <- fetch idris_int_eq1
    idris_int_eq2 <- _prim_int_eq idris_int_eq0_1 idris_int_eq1_1
    case idris_int_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_lt idris_int_lt0 idris_int_lt1 =
    (CGrInt idris_int_lt0_1) <- fetch idris_int_lt0
    (CGrInt idris_int_lt1_1) <- fetch idris_int_lt1
    idris_int_lt2 <- _prim_int_lt idris_int_lt0_1 idris_int_lt1_1
    case idris_int_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_le idris_int_le0 idris_int_le1 =
    (CGrInt idris_int_le0_1) <- fetch idris_int_le0
    (CGrInt idris_int_le1_1) <- fetch idris_int_le1
    idris_int_le2 <- _prim_int_le idris_int_le0_1 idris_int_le1_1
    case idris_int_le2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_int_gt idris_int_gt0 idris_int_gt1 =
    (CGrInt idris_int_gt0_1) <- fetch idris_int_gt0
    (CGrInt idris_int_gt1_1) <- fetch idris_int_gt1
    idris_int_gt2 <- _prim_int_gt idris_int_gt0_1 idris_int_gt1_1
    case idris_int_gt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_ge idris_int_ge0 idris_int_ge1 =
    (CGrInt idris_int_ge0_1) <- fetch idris_int_ge0
    (CGrInt idris_int_ge1_1) <- fetch idris_int_ge1
    idris_int_ge2 <- _prim_int_ge idris_int_ge0_1 idris_int_ge1_1
    case idris_int_ge2 of
      #False -> pure (CGrInt 0)
      #True  -> pure (CGrInt 1)

  idris_int_print idris_int_print0 =
    (CGrInt idris_int_print0_1) <- fetch idris_int_print0
    _prim_int_print idris_int_print0_1
    pure (CUnit)

  idris_float_add idris_float_add0 idris_float_add1 =
    (CGrFloat idris_float_add0_1) <- fetch idris_float_add0
    (CGrFloat idris_float_add1_1) <- fetch idris_float_add1
    idris_float_add3 <- _prim_float_add idris_float_add0_1 idris_float_add1_1
    pure (CGrFloat idris_float_add3)

  idris_float_mul idris_float_mul0 idris_float_mul1 =
    (CGrFloat idris_float_mul0_1) <- fetch idris_float_mul0
    (CGrFloat idris_float_mul1_1) <- fetch idris_float_mul1
    idris_float_mul3 <- _prim_float_mul idris_float_mul0_1 idris_float_mul1_1
    pure (CGrFloat idris_float_mul3)

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

  idris_float_div idris_float_div0 idris_float_div1 =
    (CGrFloat idris_float_div0_1) <- fetch idris_float_div0
    (CGrFloat idris_float_div1_1) <- fetch idris_float_div1
    idris_float_div2 <- _prim_float_div idris_float_div0_1 idris_float_div1_1
    pure (CGrFloat idris_float_div2)

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
    case idris_str_eq3 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

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

  idris_int_float idris_int_float1 =
    (CGrInt idris_int_float1_0) <- fetch idris_int_float1
    idris_int_float2 <- _prim_int_float idris_int_float1_0
    pure (CGrFloat idris_int_float2)

  idris_float_str idris_float_str1 =
    (CGrFloat idris_float_str1_0) <- fetch idris_float_str1
    idris_float_str2 <- _prim_float_string idris_float_str1_0
    pure (CGrString idris_float_str2)

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

  idris_ch_int idris_ch_int1 =
    (CGrInt idris_ch_int2) <- fetch idris_ch_int1
    pure (CGrInt idris_ch_int2)

  prim__stdin =
    pure (CGrInt 0)

  prim__stdout =
    pure (CGrInt 1)

  prim__stderr =
    pure (CGrInt 2)

  grinMain =
    r <- idr_{runMain_0}
    pure ()
|]

