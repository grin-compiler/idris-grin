{-# LANGUAGE QuasiQuotes #-}
module Idris.PrimOps where

import Grin.Grin
import Grin.TH
import Grin.PrimOpsPrelude


idrisPrimOps = withPrimPrelude [progConst|
  ffi effectful
    _prim_int_print     :: T_Int64  -> T_Unit
    _prim_string_print  :: T_String -> T_Unit
    _prim_read_string   :: T_String
    _prim_error         :: T_String -> T_Unit
    _prim_ffi_file_eof  :: T_Int64  -> T_Int64
    _prim_time          :: T_Int64
    _prim_usleep        :: T_Int64  -> T_Unit
    _prim_crash         :: T_String -> T_Unit

  -- Everything that handles Strings are FFI implemented now.
  ffi pure
    -- String
    _prim_string_concat  :: T_String -> T_String -> T_String
    _prim_string_reverse :: T_String -> T_String
    _prim_string_lt      :: T_String -> T_String -> T_Int64
    _prim_string_eq      :: T_String -> T_String -> T_Int64
    _prim_string_tail    :: T_String -> T_String
    _prim_string_len     :: T_String -> T_Int64

    -- TODO: Represent Characters as UTF8Chars instead of Int64  in the following functions
    _prim_string_cons    :: T_Int64  -> T_String -> T_String -- TODO
    _prim_string_index   :: T_String -> T_Int64
    _prim_string_head    :: T_String -> T_Int64

  ffi pure
    -- Conversion
    _prim_int_str       :: T_Int64  -> T_String
    _prim_str_int       :: T_String -> T_Int64
    _prim_int_float     :: T_Int64  -> T_Float
    _prim_float_int     :: T_Float  -> T_Int64
    _prim_float_string  :: T_Float  -> T_String
    _prim_float_floor   :: T_Float  -> T_Float
    _prim_float_ceil    :: T_Float  -> T_Float
    _prim_float_atan2   :: T_Float  -> T_Float -> T_Float
    _prim_string_float  :: T_String -> T_Float
    _prim_char_int      :: T_Char   -> T_Int64
    _prim_int_word      :: T_Int64  -> T_Word64
    _prim_int_int       :: T_Int64  -> T_Int64
    _prim_word_word     :: T_Word64 -> T_Word64
    _prim_word_int      :: T_Word64 -> T_Int64

  primop pure
    -- Int
    _prim_int_shr   :: T_Int64 -> T_Int64 -- TODO: Remove?
    _prim_int_add   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_sub   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_mul   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_div   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_udiv  :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_rem   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_ashr  :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_shl   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_lshr  :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_and   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_eq    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_ne    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_gt    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_ge    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_lt    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_le    :: T_Int64 -> T_Int64 -> T_Bool

    -- Word
    _prim_word_add  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_sub  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_mul  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_div  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_shl  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_and  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_lshr :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_udiv :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_eq   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_ne   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_gt   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_ge   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_lt   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_le   :: T_Word64 -> T_Word64 -> T_Bool

    -- Float
    _prim_float_add :: T_Float -> T_Float -> T_Float
    _prim_float_sub :: T_Float -> T_Float -> T_Float
    _prim_float_mul :: T_Float -> T_Float -> T_Float
    _prim_float_div :: T_Float -> T_Float -> T_Float
    _prim_float_eq  :: T_Float -> T_Float -> T_Bool
    _prim_float_ne  :: T_Float -> T_Float -> T_Bool
    _prim_float_gt  :: T_Float -> T_Float -> T_Bool
    _prim_float_ge  :: T_Float -> T_Float -> T_Bool
    _prim_float_lt  :: T_Float -> T_Float -> T_Bool
    _prim_float_le  :: T_Float -> T_Float -> T_Bool

    -- Bool
    _prim_bool_eq   :: T_Bool -> T_Bool -> T_Bool
    _prim_bool_ne   :: T_Bool -> T_Bool -> T_Bool

  idris_bit8_eq idris_bit8_eq0 idris_bit8_eq1 =
    (CGrBit8 idris_bit8_eq0_1) <- fetch idris_bit8_eq0
    (CGrBit8 idris_bit8_eq1_1) <- fetch idris_bit8_eq1
    idris_bit8_eq2 <- _prim_word_eq idris_bit8_eq0_1 idris_bit8_eq1_1
    case idris_bit8_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_bit16_eq idris_bit16_eq0 idris_bit16_eq1 =
    (CGrBit16 idris_bit16_eq0_1) <- fetch idris_bit16_eq0
    (CGrBit16 idris_bit16_eq1_1) <- fetch idris_bit16_eq1
    idris_bit16_eq2 <- _prim_word_eq idris_bit16_eq0_1 idris_bit16_eq1_1
    case idris_bit16_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_bit32_eq idris_bit32_eq0 idris_bit32_eq1 =
    (CGrBit32 idris_bit32_eq0_1) <- fetch idris_bit32_eq0
    (CGrBit32 idris_bit32_eq1_1) <- fetch idris_bit32_eq1
    idris_bit32_eq2 <- _prim_word_eq idris_bit32_eq0_1 idris_bit32_eq1_1
    case idris_bit32_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_bit64_eq idris_bit64_eq0 idris_bit64_eq1 =
    (CGrBit64 idris_bit64_eq0_1) <- fetch idris_bit64_eq0
    (CGrBit64 idris_bit64_eq1_1) <- fetch idris_bit64_eq1
    idris_bit64_eq2 <- _prim_word_eq idris_bit64_eq0_1 idris_bit64_eq1_1
    case idris_bit64_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_eq idris_int_eq0 idris_int_eq1 =
    (CGrInt idris_int_eq0_1) <- fetch idris_int_eq0
    (CGrInt idris_int_eq1_1) <- fetch idris_int_eq1
    idris_int_eq2 <- _prim_int_eq idris_int_eq0_1 idris_int_eq1_1
    case idris_int_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_float_eq idris_float_eq0 idris_float_eq1 =
    (CGrFloat idris_float_eq0_1) <- fetch idris_float_eq0
    (CGrFloat idris_float_eq1_1) <- fetch idris_float_eq1
    idris_float_eq2 <- _prim_float_eq idris_float_eq0_1 idris_float_eq1_1
    case idris_float_eq2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_int_lt idris_int_lt0 idris_int_lt1 =
    (CGrInt idris_int_lt0_1) <- fetch idris_int_lt0
    (CGrInt idris_int_lt1_1) <- fetch idris_int_lt1
    idris_int_lt2 <- _prim_int_lt idris_int_lt0_1 idris_int_lt1_1
    case idris_int_lt2 of
      #False  -> pure (CGrInt 0)
      #True   -> pure (CGrInt 1)

  idris_float_lt idris_float_lt0 idris_float_lt1 =
    (CGrFloat idris_float_lt0_1) <- fetch idris_float_lt0
    (CGrFloat idris_float_lt1_1) <- fetch idris_float_lt1
    idris_float_lt2 <- _prim_float_lt idris_float_lt0_1 idris_float_lt1_1
    case idris_float_lt2 of
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

  idris_lashr_int idris_lashr_int1 idris_lashr_int2 =
    (CGrInt idris_lashr_int1_0) <- fetch idris_lashr_int1
    (CGrInt idris_lashr_int2_0) <- fetch idris_lashr_int2
    idris_lashr_int3 <- _prim_int_ashr idris_lashr_int1_0 idris_lashr_int2_0
    pure (CGrInt idris_lashr_int3)

  idris_bit64_shl idris_bit64_shl1 idris_bit64_shl2 =
    (CGrBit64 idris_bit64_shl3) <- fetch idris_bit64_shl1
    (CGrBit64 idris_bit64_shl4) <- fetch idris_bit64_shl2
    idris_bit64_shl5 <- _prim_word_shl idris_bit64_shl3 idris_bit64_shl4
    pure (CGrBit64 idris_bit64_shl5)

  idris_int_shl idris_int_shl1 idris_int_shl2 =
    (CGrInt idris_int_shl3) <- fetch idris_int_shl1
    (CGrInt idris_int_shl4) <- fetch idris_int_shl2
    idris_int_shl5 <- _prim_int_shl idris_int_shl3 idris_int_shl4
    pure (CGrInt idris_int_shl5)

  idris_bit8_lshr idris_bit8_lshr1 idris_bit8_lshr2 =
    (CGrBit8 idris_bit8_lshr3) <- fetch idris_bit8_lshr1
    (CGrBit8 idris_bit8_lshr4) <- fetch idris_bit8_lshr2
    idris_bit8_lshr5 <- _prim_word_lshr idris_bit8_lshr3 idris_bit8_lshr4
    pure (CGrBit8 idris_bit8_lshr5)

  idris_bit16_lshr idris_bit16_lshr1 idris_bit16_lshr2 =
    (CGrBit16 idris_bit16_lshr3) <- fetch idris_bit16_lshr1
    (CGrBit16 idris_bit16_lshr4) <- fetch idris_bit16_lshr2
    idris_bit16_lshr5 <- _prim_word_lshr idris_bit16_lshr3 idris_bit16_lshr4
    pure (CGrBit16 idris_bit16_lshr5)

  idris_bit32_lshr idris_bit32_lshr1 idris_bit32_lshr2 =
    (CGrBit32 idris_bit32_lshr3) <- fetch idris_bit32_lshr1
    (CGrBit32 idris_bit32_lshr4) <- fetch idris_bit32_lshr2
    idris_bit32_lshr5 <- _prim_word_lshr idris_bit32_lshr3 idris_bit32_lshr4
    pure (CGrBit32 idris_bit32_lshr5)

  idris_bit64_lshr idris_bit64_lshr1 idris_bit64_lshr2 =
    (CGrBit64 idris_bit64_lshr3) <- fetch idris_bit64_lshr1
    (CGrBit64 idris_bit64_lshr4) <- fetch idris_bit64_lshr2
    idris_bit64_lshr5 <- _prim_word_lshr idris_bit64_lshr3 idris_bit64_lshr4
    pure (CGrBit64 idris_bit64_lshr5)

  idris_int_lshr idris_int_lshr1 idris_int_lshr2 =
    (CGrInt idris_int_lshr3) <- fetch idris_int_lshr1
    (CGrInt idris_int_lshr4) <- fetch idris_int_lshr2
    idris_int_lshr5 <- _prim_int_lshr idris_int_lshr3 idris_int_lshr4
    pure (CGrInt idris_int_lshr5)

  idris_bit8_and idris_bit8_and1 idris_bit8_and2 =
    (CGrBit8 idris_bit8_and3) <- fetch idris_bit8_and1
    (CGrBit8 idris_bit8_and4) <- fetch idris_bit8_and2
    idris_bit8_and5 <- _prim_word_and idris_bit8_and3 idris_bit8_and4
    pure (CGrBit8 idris_bit8_and5)

  idris_bit64_and idris_bit64_and1 idris_bit64_and2 =
    (CGrBit64 idris_bit64_and3) <- fetch idris_bit64_and1
    (CGrBit64 idris_bit64_and4) <- fetch idris_bit64_and2
    idris_bit64_and5 <- _prim_word_and idris_bit64_and3 idris_bit64_and4
    pure (CGrBit64 idris_bit64_and5)

  idris_int_and idris_int_and1 idris_int_and2 =
    (CGrInt idris_int_and3) <- fetch idris_int_and1
    (CGrInt idris_int_and4) <- fetch idris_int_and2
    idris_int_and5 <- _prim_int_and idris_int_and3 idris_int_and4
    pure (CGrInt idris_int_and5)

  idris_int_print idris_int_print0 =
    (CGrInt idris_int_print0_1) <- fetch idris_int_print0
    _prim_int_print idris_int_print0_1
    pure (CUnit)

  idris_float_add idris_float_add0 idris_float_add1 =
    (CGrFloat idris_float_add0_1) <- fetch idris_float_add0
    (CGrFloat idris_float_add1_1) <- fetch idris_float_add1
    idris_float_add3 <- _prim_float_add idris_float_add0_1 idris_float_add1_1
    pure (CGrFloat idris_float_add3)

  idris_float_sub idris_float_sub0 idris_float_sub1 =
    (CGrFloat idris_float_sub0_1) <- fetch idris_float_sub0
    (CGrFloat idris_float_sub1_1) <- fetch idris_float_sub1
    idris_float_sub3 <- _prim_float_sub idris_float_sub0_1 idris_float_sub1_1
    pure (CGrFloat idris_float_sub3)

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

  idris_bit64_add idris_bit64_add0 idris_bit64_add1 =
    (CGrBit64 idris_bit64_add0_1) <- fetch idris_bit64_add0
    (CGrBit64 idris_bit64_add1_1) <- fetch idris_bit64_add1
    idris_bit64_add2 <- _prim_word_add idris_bit64_add0_1 idris_bit64_add1_1
    pure (CGrBit64 idris_bit64_add2)

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

  idris_bit8_udiv idris_bit8_udiv1 idris_bit8_udiv2 =
    (CGrBit8 idris_bit8_udiv3) <- fetch idris_bit8_udiv1
    (CGrBit8 idris_bit8_udiv4) <- fetch idris_bit8_udiv2
    idris_bit8_udiv5 <- _prim_word_udiv idris_bit8_udiv3 idris_bit8_udiv4
    pure (CGrBit8 idris_bit8_udiv5)

  idris_bit16_udiv idris_bit16_udiv1 idris_bit16_udiv2 =
    (CGrBit16 idris_bit16_udiv3) <- fetch idris_bit16_udiv1
    (CGrBit16 idris_bit16_udiv4) <- fetch idris_bit16_udiv2
    idris_bit16_udiv5 <- _prim_word_udiv idris_bit16_udiv3 idris_bit16_udiv4
    pure (CGrBit16 idris_bit16_udiv5)

  idris_bit32_udiv idris_bit32_udiv1 idris_bit32_udiv2 =
    (CGrBit32 idris_bit32_udiv3) <- fetch idris_bit32_udiv1
    (CGrBit32 idris_bit32_udiv4) <- fetch idris_bit32_udiv2
    idris_bit32_udiv5 <- _prim_word_udiv idris_bit32_udiv3 idris_bit32_udiv4
    pure (CGrBit32 idris_bit32_udiv5)

  idris_bit64_udiv idris_bit64_udiv1 idris_bit64_udiv2 =
    (CGrBit64 idris_bit64_udiv3) <- fetch idris_bit64_udiv1
    (CGrBit64 idris_bit64_udiv4) <- fetch idris_bit64_udiv2
    idris_bit64_udiv5 <- _prim_word_udiv idris_bit64_udiv3 idris_bit64_udiv4
    pure (CGrBit64 idris_bit64_udiv5)

  idris_int_udiv idris_int_udiv1 idris_int_udiv2 =
    (CGrInt idris_int_udiv3) <- fetch idris_int_udiv1
    (CGrInt idris_int_udiv4) <- fetch idris_int_udiv2
    idris_int_udiv5 <- _prim_int_udiv idris_int_udiv3 idris_int_udiv4
    pure (CGrInt idris_int_udiv5)

  idris_int_rem idris_int_rem1 idris_int_rem2 =
    (CGrInt idris_int_rem3) <- fetch idris_int_rem1
    (CGrInt idris_int_rem4) <- fetch idris_int_rem2
    idris_int_rem5 <- _prim_int_rem idris_int_rem3 idris_int_rem4
    pure (CGrInt idris_int_rem5)

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

  idris_str_idx idris_str_idx1 idris_str_idx2 =
    (CGrString idris_str_idx3)  <- fetch idris_str_idx1
    (CGrInt idris_str_idx4)     <- fetch idris_str_idx2
    idris_str_idx5 <- _prim_string_index idris_str_idx3 idris_str_idx4
    pure (CGrInt idris_str_idx5)

  idris_int_str idris_int_str1 =
    (CGrInt idris_int_str1_0) <- fetch idris_int_str1
    idris_int_str2 <- _prim_int_str idris_int_str1_0
    pure (CGrString idris_int_str2)

  idris_str_int idris_str_int1 =
    (CGrString idris_str_int1_0) <- fetch idris_str_int1
    idris_str_int2 <- _prim_str_int idris_str_int1_0
    pure (CGrInt idris_str_int2)

  idris_str_float idris_str_float1 =
    (CGrString idris_str_float2) <- fetch idris_str_float1
    idris_str_float3 <- _prim_string_float idris_str_float2
    pure (CGrFloat idris_str_float3)

  idris_int_float idris_int_float1 =
    (CGrInt idris_int_float1_0) <- fetch idris_int_float1
    idris_int_float2 <- _prim_int_float idris_int_float1_0
    pure (CGrFloat idris_int_float2)

  idris_float_int idris_float_int1 =
    (CGrFloat idris_float_int2) <- fetch idris_float_int1
    idris_float_int3 <- _prim_float_int idris_float_int2
    pure (CGrInt idris_float_int3)

  idris_float_str idris_float_str1 =
    (CGrFloat idris_float_str1_0) <- fetch idris_float_str1
    idris_float_str2 <- _prim_float_string idris_float_str1_0
    pure (CGrString idris_float_str2)

  idris_float_floor idris_float_floor1 =
    (CGrFloat idris_float_floor2) <- fetch idris_float_floor1
    idris_float_floor3 <- _prim_float_floor idris_float_floor2
    pure (CGrFloat idris_float_floor3)

  idris_float_ceil idris_float_ceil1 =
    (CGrFloat idris_float_ceil2) <- fetch idris_float_ceil1
    idris_float_ceil3 <- _prim_float_ceil idris_float_ceil2
    pure (CGrFloat idris_float_ceil3)

  idris_float_atan2 idris_float_atan2_1 idris_float_atan2_2 =
    (CGrFloat idris_float_atan2_3) <- fetch idris_float_atan2_1
    (CGrFloat idris_float_atan2_4) <- fetch idris_float_atan2_2
    idris_float_atan2_5 <- _prim_float_atan2 idris_float_atan2_3 idris_float_atan2_4
    pure (CGrFloat idris_float_atan2_5)

  idris_ffi_file_eof idris_ffi_file_eof1 =
    (CGrInt idris_ffi_file_eof1_0) <- fetch idris_ffi_file_eof1
    idris_ffi_file_eof2 <- _prim_ffi_file_eof idris_ffi_file_eof1_0
    pure (CGrInt idris_ffi_file_eof2)

  idris_lz_ext_int_bit64 idris_lz_ext_int_bit64_1 =
    (CGrInt idris_lz_ext_int_bit64_2) <- fetch idris_lz_ext_int_bit64_1
    idris_lz_ext_int_bit64_3 <- _prim_int_word idris_lz_ext_int_bit64_2
    pure (CGrBit64 idris_lz_ext_int_bit64_3)

  idris_lz_ext_bit8_int idris_lz_ext_bit8_int1 =
    (CGrBit8 idris_lz_ext_bit8_int2) <- fetch idris_lz_ext_bit8_int1
    idris_lz_ext_bit8_int3 <- _prim_word_int idris_lz_ext_bit8_int2
    pure (CGrInt idris_lz_ext_bit8_int3)

  idris_lz_ext_bit8_bit64 idris_lz_ext_bit8_bit64_1 =
    (CGrBit8 idris_lz_ext_bit8_bit64_2) <- fetch idris_lz_ext_bit8_bit64_1
    idris_lz_ext_bit8_bit64_3 <- _prim_word_word idris_lz_ext_bit8_bit64_2
    pure (CGrBit64 idris_lz_ext_bit8_bit64_3)

  idris_lz_ext idris_lz_ext1 =
    (CGrInt idris_lz_ext2) <- fetch idris_lz_ext1
    idris_lz_ext3 <- _prim_int_int idris_lz_ext2
    pure (CGrInt idris_lz_ext3)

  idris_ls_ext idris_ls_ext1 =
    (CGrInt idris_ls_ext2) <- fetch idris_ls_ext1
    idris_ls_ext3 <- _prim_int_add idris_ls_ext2 0
    pure (CGrInt idris_ls_ext3)

  idris_ch_int idris_ch_int1 =
    (CGrInt idris_ch_int2) <- fetch idris_ch_int1
    pure (CGrInt idris_ch_int2)

  idris_int_ch idris_int_ch1 =
    (CGrInt idris_int_ch2) <- fetch idris_int_ch1
    pure (CGrInt idris_int_ch2)

  idris_usleep idris_usleep1 =
    (CGrInt idris_usleep1_0) <- fetch idris_usleep1
    _prim_usleep idris_usleep1_0
    pure (CGrVoid) -- Maybe it needs another void like type?

  idris_crash idris_crash1 =
    (CGrString idris_crash2) <- fetch idris_crash1
    _prim_crash idris_crash2
    pure (CGrVoid)

  idris_ltrunc_big_int idris_ltrunc_big_int1 =
    (CGrInt idris_ltrunc_big_int2) <- fetch idris_ltrunc_big_int1
    -- TODO: Big vs Native Int
    idris_ltrunc_big_int3 <- _prim_int_int idris_ltrunc_big_int2
    pure (CGrInt idris_ltrunc_big_int3)

  idris_ltrunc_int_bit8 idris_ltrunc_int_bit8_1 =
    (CGrInt idris_ltrunc_int_bit8_2) <- fetch idris_ltrunc_int_bit8_1
    idris_ltrunc_int_bit8_3 <- _prim_int_word
    pure (CGrBit8 idris_ltrunc_int_bit8_3)

  idris_ltrunc_big_bit64 idris_ltrunc_big_bit64_1 =
    (CGrInt idris_ltrunc_big_bit64_2) <- fetch idris_ltrunc_big_bit64_1
    idris_ltrunc_big_bit64_3 <- _prim_int_word idris_ltrunc_big_bit64_2
    pure (CGrBit64 idris_ltrunc_big_bit64_3)

  idris_ltrunc_bit64_bit8 idris_ltrunc_bit64_bit8_1 =
    (CGrBit64 idris_ltrunc_bit64_bit8_2) <- fetch idris_ltrunc_bit64_bit8_1
    -- TODO: use Word8 for Bit8
    pure (CGrBit8 idris_ltrunc_bit64_bit8_2)

  idris_ltrunc_bit32_bit8 idris_ltrunc_bit32_bit8_1 =
    -- TODO: use Word32 for Bit32 and Word8 for Bit8
    (CGrBit32 idris_ltrunc_bit32_bit8_2) <- fetch idris_ltrunc_bit32_bit8_1
    pure (CGrBit8 idris_ltrunc_bit32_bit8_2)

  idris_ltrunc_bit16_bit8 idris_ltrunc_bit16_bit8_1 =
    -- TODO: use Word16 for Bit16 and Word8 for Bit8
    (CGrBit16 idris_ltrunc_bit16_bit8_2) <- fetch idris_ltrunc_bit16_bit8_1
    pure (CGrBit8 idris_ltrunc_bit16_bit8_2)

  idris_ltrunc idris_ltrunc1 =
    (CGrInt idris_ltrunc1_0) <- fetch idris_ltrunc1
    pure (CGrInt idris_ltrunc1_0)

  idris_error idris_error1 =
    _prim_error idris_error1
    pure (CGrError idris_error1)

  idris_time =
    idris_time1 <- _prim_time
    pure (CGrInt idris_time1)

  idris_fileSize idris_fileSize1 =
    idris_fileSize2 <- pure 1024 -- TODO: Handle C file pointers
    pure (CGrInt idris_fileSize2)

  idris_fileOpen idris_fileOpen1 idris_fileOpen2 =
    idris_fileOpen3 <- pure 42 -- TODO: File handler converted to (Void *) ptr
    pure (CGrPtr idris_fileOpen3)

  idris_fileError idris_fileError1 =
    idris_fileError2 <- pure 0 -- TODO: Call ferror
    pure (CGrPtr idris_fileError2)

  idris_fileClose idris_fileClose1 =
    idris_fileClose2 <- pure 0 -- TODO: Call fclose
    pure (CGrVoid)

  idris_addToString idris_addToString1 idris_addToString2 =
    (CGrPtr idris_addToString2_0) <- fetch idris_addToString2
    (CGrString idris_addToString1_0) <- fetch idris_addToString1 -- TODO: add C String to non-String
    pure (CGrVoid)

  idris_mkFileError idris_mkFileError0 =
    pure (CGrFileError) -- TODO: Turns the errno into a FileError constructor from the Prelude.File

  idris_getString idris_getString1 idris_getString2 =
    idris_getString3 <- pure #"TODO" -- TODO: Handle StringBuffer
    pure (CGrString idris_getString3)

  idris_makeStringBuffer idris_makeStringBuffer1 =
    -- TODO: Create StringBuffer
    -- TODO: Handle CPtr
    idris_makeStringBuffer2 <- pure 42
    pure (CGrPtr idris_makeStringBuffer2)

  isNull isNull1 =
    -- TODO: Implement isNull check
    (CGrPtr isNull1_0) <- fetch isNull1
    isNull2 <- pure 0
    pure (CGrInt isNull2)

  idris_bit64_mul idris_bit64_mul1 idris_bit64_mul2 =
    (CGrBit64 idris_bit64_mul3) <- fetch idris_bit64_mul1
    (CGrBit64 idris_bit64_mul4) <- fetch idris_bit64_mul2
    idris_bit64_mul5 <- _prim_word_mul idris_bit64_mul3 idris_bit64_mul4
    pure (CGrBit64 idris_bit64_mul5)

  idris_errno =
    pure (CErrorNo)

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
