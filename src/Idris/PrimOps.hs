{-# LANGUAGE QuasiQuotes #-}
module Idris.PrimOps where

import Grin.Grin
import Grin.TH
import Grin.PrimOpsPrelude


idrisPrimOps = withPrimPrelude [progConst|
  ffi effectful
    -- Solve HPT for the %ref variables, variables should refer to the
    -- same values, context dependent or independent
    _prim_new_ref   :: %ref -> T_Word64           -- TODO: Ptr
    _prim_write_ref :: T_Word64 -> %ref -> T_Unit -- TODO: Ptr
    _prim_read_ref  :: T_Word64 -> %ref           -- TODO: Ptr
    _prim_malloc    :: T_Int64 -> T_Word64 -- Ptr
    _prim_poke      :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit -- Ptr, Word8
    _prim_peek      :: T_Word64 -> T_Int64 -> T_Word64 -- Ptr, Word8
    _prim_memset    :: T_Word64 -> T_Int64 -> T_Word64 -> T_Int64 -> T_Unit -- Ptr, Word8
    _prim_memmove   :: T_Word64 -> T_Word64 -> T_Int64 -> T_Int64 -> T_Int64 -> T_Unit -- Ptr, Ptr
    _prim_free      :: T_Word64 -> T_Unit -- Ptr

  ffi effectful
    _prim_int_print     :: T_Int64  -> T_Unit
    _prim_string_print  :: T_String -> T_Unit
    _prim_read_string   :: T_String
    _prim_error         :: T_String -> T_Unit
    _prim_time          :: T_Int64
    _prim_usleep        :: T_Int64  -> T_Unit
    _prim_crash         :: T_String -> T_Unit
    _prim_new_buffer    :: T_Int64  -> T_Word64 -- TODO: Ptr
    _prim_set_buffer_byte   :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit -- TODO: Ptr
    _prim_set_buffer_string :: T_Word64 -> T_Int64 -> T_String -> T_Unit -- TODO: Ptr
    _prim_set_buffer_int    :: T_Word64 -> T_Int64 -> T_Int64  -> T_Unit -- TODO: Ptr
    _prim_set_buffer_double :: T_Word64 -> T_Int64 -> T_Float  -> T_Unit -- TODO: Ptr
    _prim_copy_buffer   :: T_Word64 -> T_Int64 -> T_Int64 -> T_Word64 -> T_Int64 -> T_Unit -- TODO: Ptr
    _prim_write_buffer  :: T_Word64 -> T_Word64 -> T_Int64 -> T_Int64 -> T_Unit -- TODO: Ptr
    _prim_file_close    :: T_Word64 -> T_Unit -- TODO: Ptr
    _prim_read_buffer   :: T_Word64 -> T_Word64 -> T_Int64 -> T_Int64 -> T_Int64 -- TODO: Ptr
    _prim_file_eof      :: T_Word64 -> T_Int64
    _prim_stdin         :: T_Word64 -- TODO: Ptr
    _prim_stdout        :: T_Word64 -- TODO: Ptr
    _prim_stderr        :: T_Word64 -- TODO: Ptr
    _prim_putchar       :: T_Char -> T_Unit

  -- These are effectful primitives, but we can optimise them away, if nothing
  -- depends on them
  ffi pure
    _prim_get_buffer_byte   :: T_Word64 -> T_Int64 -> T_Word64 -- TODO: Ptr
    _prim_get_buffer_int    :: T_Word64 -> T_Int64 -> T_Int64  -- TODO: Ptr
    _prim_get_buffer_double :: T_Word64 -> T_Int64 -> T_Float  -- TODO: Ptr
    _prim_get_buffer_string :: T_Word64 -> T_Int64 -> T_Int64 -> T_String
    _prim_file_open         :: T_String -> T_String -> T_Word64 -- TODO: Ptr

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
    _prim_int_char      :: T_Int64  -> T_Char

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

  prim__null =
    -- TODO: Word constants, or constants with primitive type annotations
    prim__null1 <- _prim_int_word 0
    pure (CGrPtr prim__null1)

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
    (CGrBit64 idris_ffi_file_eof1_0) <- fetch idris_ffi_file_eof1
    idris_ffi_file_eof2 <- _prim_file_eof idris_ffi_file_eof1_0
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
    idris_ltrunc_int_bit8_3 <- _prim_int_word idris_ltrunc_int_bit8_2
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
    (CGrString idris_fileOpen3) <- fetch idris_fileOpen1
    (CGrString idris_fileOpen4) <- fetch idris_fileOpen2
    idris_fileOpen5 <- _prim_file_open idris_fileOpen3 idris_fileOpen4
    pure (CGrPtr idris_fileOpen5)

  idris_fileError idris_fileError1 =
    idris_fileError2 <- _prim_int_word 0 -- TODO: Call ferror
    pure (CGrPtr idris_fileError2)

  idris_fileClose idris_fileClose1 =
    (CGrPtr idris_fileClose2) <- fetch idris_fileClose1
    idris_fileClose3 <- _prim_file_close idris_fileClose2
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
    (CGrPtr isNull2) <- fetch isNull1
    isNull6 <- _prim_int_word 0
    isNull3 <- _prim_word_eq isNull2 isNull6
    isNull4 <- case isNull3 of
      #False -> pure 0
      #True  -> pure 1
    pure (CGrInt isNull4)

  idris_bit64_mul idris_bit64_mul1 idris_bit64_mul2 =
    (CGrBit64 idris_bit64_mul3) <- fetch idris_bit64_mul1
    (CGrBit64 idris_bit64_mul4) <- fetch idris_bit64_mul2
    idris_bit64_mul5 <- _prim_word_mul idris_bit64_mul3 idris_bit64_mul4
    pure (CGrBit64 idris_bit64_mul5)

  idris_errno =
    pure (CErrorNo)

  prim__stdin =
    prim__stdin1 <- _prim_stdin
    pure (CGrBit64 prim__stdin1)

  prim__stdout =
    prim__stdout1 <- _prim_stdout
    pure (CGrBit64 prim__stdout1)

  prim__stderr =
    prim__stderr1 <- _prim_stderr
    pure (CGrBit64 prim__stderr1)

  prim__vm prim__vm1 =
    -- TODO: Figure this out properly
    prim__vm2 <- fetch prim__vm1
    -- Pointer of VM
    pure (CGrVM)

  prim__writeFile prim__writeFile1 prim__writeFile2 prim__writeFile3 =
    (CGrUndefined1) <- fetch prim__writeFile1
    (CGrUndefined2) <- fetch prim__writeFile2
    (CGrUndefined3) <- fetch prim__writeFile3
    pure (CGrUndefined4)

  idris_newBuffer idris_newBuffer1 idris_newBuffer2 =
    (CGrVM) <- fetch idris_newBuffer1
    (CGrInt idris_newBuffer3) <- fetch idris_newBuffer2
    idris_newBuffer4 <- _prim_new_buffer idris_newBuffer3
    pure (CGrPtr idris_newBuffer4)

  idris_newRef idris_newRef1 =
    idris_newRef2 <- fetch idris_newRef1
    idris_newRef3 <- _prim_new_ref idris_newRef2
    pure (CGrRef idris_newRef3)

  idris_readRef idris_readRef1 =
    (CGrRef idris_readRef2) <- fetch idris_readRef1
    idris_readRef3 <- _prim_read_ref idris_readRef2
    pure idris_readRef3

  idris_writeRef idris_writeRef1 idris_writeRef2 =
    (CGrRef idris_writeRef3) <- fetch idris_writeRef1
    idris_writeRef4 <- fetch idris_writeRef2
    idris_writeRef5 <- _prim_write_ref idris_writeRef3 idris_writeRef4
    pure (CGrUnit)

  idris_copyBuffer idris_copyBuffer1 idris_copyBuffer2 idris_copyBuffer3 idris_copyBuffer4 idris_copyBuffer5 =
    (CGrPtr idris_copyBuffer6)  <- fetch idris_copyBuffer1
    (CGrInt idris_copyBuffer7)  <- fetch idris_copyBuffer2
    (CGrInt idris_copyBuffer8)  <- fetch idris_copyBuffer3
    (CGrPtr idris_copyBuffer9)  <- fetch idris_copyBuffer4
    (CGrInt idris_copyBuffer10) <- fetch idris_copyBuffer5
    idris_copyBuffer11 <- _prim_copy_buffer idris_copyBuffer6 idris_copyBuffer7 idris_copyBuffer8 idris_copyBuffer9 idris_copyBuffer10
    pure (CGrUnit)

  idris_getBufferByte idris_getBufferByte1 idris_getBufferByte2 =
    (CGrPtr idris_getBufferByte3) <- fetch idris_getBufferByte1
    (CGrInt idris_getBufferByte4) <- fetch idris_getBufferByte2
    idris_getBufferByte5 <- _prim_get_buffer_byte idris_getBufferByte3 idris_getBufferByte4
    pure (CGrBit8 idris_getBufferByte5)

  idris_getBufferDouble idris_getBufferDouble1 idris_getBufferDouble2 =
    (CGrPtr idris_getBufferDouble3) <- fetch idris_getBufferDouble1
    (CGrInt idris_getBufferDouble4) <- fetch idris_getBufferDouble2
    idris_getBufferDouble5 <- _prim_get_buffer_double idris_getBufferDouble3 idris_getBufferDouble4
    pure (CGrFloat idris_getBufferDouble5)

  fflush fflush1 fflush2 =
    (CGrUndefined25) <- fetch fflush1
    (CGrUndefined26) <- fetch fflush2
    pure (CGrUndefined27)

  exit exit1 =
    (CGrUndefined28) <- fetch exit1
    pure (CGrUndefined29)

  idris_freeMsg idris_freeMsg1 =
    (CGrUndefined30) <- fetch idris_freeMsg1
    pure (CGrUndefined31)

  free free1 =
    (CGrPtr free2) <- fetch free1
    free3 <- _prim_free free2
    pure (CGrUnit)

  idris_showerror idris_showerror1 =
    (CGrUndefined34) <- fetch idris_showerror1
    pure (CGrUndefined35)

  idris_numArgs =
    pure (CGrUndefined36)

  fgetc fgetc1 =
    (CGrUndefined37) <- fetch fgetc1
    pure (CGrUndefined38)

  getchar =
    pure (CGrUndefined39)

  idris_memmove idris_memmove1 idris_memmove2 idris_memmove3 idris_memmove4 idris_memmove5 =
    (CGrPtr idris_memmove6) <- fetch idris_memmove1 -- Destination
    (CGrPtr idris_memmove7) <- fetch idris_memmove2 -- Source
    (CGrInt idris_memmove8) <- fetch idris_memmove3 -- Dest Offset
    (CGrInt idris_memmove9) <- fetch idris_memmove4 -- Src Offset
    (CGrInt idris_memmove10) <- fetch idris_memmove5 -- Size
    idris_memmove11 <- _prim_memmove idris_memmove6 idris_memmove7 idris_memmove8 idris_memmove9 idris_memmove10
    pure (CGrUnit)

  calloc calloc1 calloc2 =
    (CGrUndefined46) <- fetch calloc1
    (CGrUndefined47) <- fetch calloc2
    pure (CGrUndefined48)

  idris_getSender idris_getSender1 =
    (CGrUndefined49) <- fetch idris_getSender1
    pure (CGrUndefined50)

  idris_getArg idris_getArg1 =
    (CGrUndefined51) <- fetch idris_getArg1
    pure (CGrUndefined52)

  idris_readBuffer idris_readBuffer1 idris_readBuffer2 idris_readBuffer3 idris_readBuffer4 =
    (CGrPtr idris_readBuffer5) <- fetch idris_readBuffer1
    (CGrPtr idris_readBuffer6) <- fetch idris_readBuffer2
    (CGrInt idris_readBuffer7) <- fetch idris_readBuffer3
    (CGrInt idris_readBuffer8) <- fetch idris_readBuffer4
    idris_readBuffer9 <- _prim_read_buffer idris_readBuffer5 idris_readBuffer6 idris_readBuffer7 idris_readBuffer8
    pure (CGrInt idris_readBuffer9)

  idris_getBufferInt idris_getBufferInt1 idris_getBufferInt2 =
    (CGrPtr idris_getBufferInt3) <- fetch idris_getBufferInt1
    (CGrInt idris_getBufferInt4) <- fetch idris_getBufferInt2
    idris_getBufferInt5 <- _prim_get_buffer_int idris_getBufferInt3 idris_getBufferInt4
    pure (CGrInt idris_getBufferInt5)

  putchar putchar1 =
    (CGrInt putchar2) <- fetch putchar1
    putchar3 <- _prim_int_char putchar2
    putchar4 <- _prim_putchar putchar3
    pure (CGrUnit)

  idris_memset idris_memset1 idris_memset2 idris_memset3 idris_memset4 =
    (CGrPtr  idris_memset5) <- fetch idris_memset1
    (CGrInt  idris_memset6) <- fetch idris_memset2 -- Offset
    (CGrBit8 idris_memset7) <- fetch idris_memset3 -- Char
    (CGrInt  idris_memset8) <- fetch idris_memset4 -- Size
    idris_memset9 <- _prim_memset idris_memset5 idris_memset6 idris_memset7 idris_memset8
    pure (CGrUnit)

  idris_getChannel idris_getChannel1 =
    (CGrUndefined63) <- fetch idris_getChannel1
    pure (CGrUndefined64)

  idris_setBufferByte idris_setBufferByte1 idris_setBufferByte2 idris_setBufferByte3 =
    -- TODO: Convert Ptr to ManagedPtr?
    (CGrPtr   idris_setBufferByte4) <- fetch idris_setBufferByte1
    (CGrInt   idris_setBufferByte5) <- fetch idris_setBufferByte2
    (CGrBit8  idris_setBufferByte6) <- fetch idris_setBufferByte3
    idris_setBufferByte7 <- _prim_set_buffer_byte idris_setBufferByte4 idris_setBufferByte5 idris_setBufferByte6
    pure (CGrUnit)

  idris_getBufferString idris_getBufferString1 idris_getBufferString2 idris_getBufferString3 =
    (CGrPtr idris_getBufferString4) <- fetch idris_getBufferString1
    (CGrInt idris_getBufferString5) <- fetch idris_getBufferString2
    (CGrInt idris_getBufferString6) <- fetch idris_getBufferString3
    idris_getBufferString7 <- _prim_get_buffer_string idris_getBufferString4 idris_getBufferString5 idris_getBufferString6
    pure (CGrString idris_getBufferString7)

  idris_peek idris_peek1 idris_peek2 =
    (CGrPtr idris_peek3) <- fetch idris_peek1
    (CGrInt idris_peek4) <- fetch idris_peek2 -- Offset
    idris_peek5 <- _prim_peek idris_peek3 idris_peek4
    pure (CGrBit8 idris_peek5)

  idris_recvMessage idris_recvMessage1 =
    (CGrUndefined76) <- fetch idris_recvMessage1
    pure (CGrUndefined77)

  idris_checkMessages idris_checkMessages1 =
    (CGrUndefined78) <- fetch idris_checkMessages1
    pure (CGrUndefined79)

  idris_setBufferString idris_setBufferString1 idris_setBufferString2 idris_setBufferString3 =
    (CGrPtr     idris_setBufferString4) <- fetch idris_setBufferString1 -- ManagedPtr
    (CGrInt     idris_setBufferString5) <- fetch idris_setBufferString2
    (CGrString  idris_setBufferString6) <- fetch idris_setBufferString3
    idris_setBufferString7 <- _prim_set_buffer_string idris_setBufferString4 idris_setBufferString5 idris_setBufferString6
    pure (CGrUnit)

  idris_setBufferDouble idris_setBufferDouble1 idris_setBufferDouble2 idris_setBufferDouble3 =
    (CGrPtr idris_setBufferDouble4) <- fetch idris_setBufferDouble1
    (CGrInt idris_setBufferDouble5) <- fetch idris_setBufferDouble2
    (CGrFloat idris_setBufferDouble6) <- fetch idris_setBufferDouble3
    idris_setBufferDouble7 <- _prim_set_buffer_double idris_setBufferDouble4 idris_setBufferDouble5 idris_setBufferDouble6
    pure (CGrUnit)

  idris_poke idris_poke1 idris_poke2 idris_poke3 =
    (CGrPtr   idris_poke4) <- fetch idris_poke1
    (CGrInt   idris_poke5) <- fetch idris_poke2 -- Offset
    (CGrBit8  idris_poke6) <- fetch idris_poke3
    idris_poke7 <- _prim_poke idris_poke4 idris_poke5 idris_poke6
    pure (CGrUnit)

  idris_checkMessagesTimeout idris_checkMessagesTimeout1 idris_checkMessagesTimeout2 =
    (CGrUndefined92) <- fetch idris_checkMessagesTimeout1
    (CGrUndefined93) <- fetch idris_checkMessagesTimeout2
    pure (CGrUndefined94)

  idris_getMsg idris_getMsg1 =
    (CGrUndefined95) <- fetch idris_getMsg1
    pure (CGrUndefined96)

  idris_sendMessage idris_sendMessage1 idris_sendMessage2 idris_sendMessage3 idris_sendMessage4 =
    (CGrUndefined97)  <- fetch idris_sendMessage1
    (CGrUndefined98)  <- fetch idris_sendMessage2
    (CGrUndefined99)  <- fetch idris_sendMessage3
    (CGrUndefined100) <- fetch idris_sendMessage4
    pure (CGrUndefined101)

  idris_writeBuffer idris_writeBuffer1 idris_writeBuffer2 idris_writeBuffer3 idris_writeBuffer4 =
    (CGrPtr idris_writeBuffer5) <- fetch idris_writeBuffer1
    (CGrPtr idris_writeBuffer6) <- fetch idris_writeBuffer2
    (CGrInt idris_writeBuffer7) <- fetch idris_writeBuffer3
    (CGrInt idris_writeBuffer8) <- fetch idris_writeBuffer4
    idris_writeBuffer9 <- _prim_write_buffer idris_writeBuffer5 idris_writeBuffer6 idris_writeBuffer7 idris_writeBuffer8
    pure (CGrUnit)

  idris_setBufferInt idris_setBufferInt1 idris_setBufferInt2 idris_setBufferInt3 =
    (CGrPtr idris_setBufferInt4) <- fetch idris_setBufferInt1
    (CGrInt idris_setBufferInt5) <- fetch idris_setBufferInt2
    (CGrInt idris_setBufferInt6) <- fetch idris_setBufferInt3
    idris_setBufferInt7 <- _prim_set_buffer_int idris_setBufferInt4 idris_setBufferInt5 idris_setBufferInt6
    pure (CGrUnit)

  malloc malloc1 =
    (CGrInt malloc2) <- fetch malloc1
    malloc3 <- _prim_malloc malloc2
    pure (CGrPtr malloc3)

  idris_fork idris_fork1 =
    (CGrUndefined113) <- fetch idris_fork1
    pure (CGrUndefined113)

  grinMain =
    r <- "idr_{runMain_0}"
    pure ()
|]


{-
Idris Builtin primitives

-- Pointers as external primitive; there's no literals for these, so no
-- need for them to be part of the compiler.

export data Ptr : Type
export data ManagedPtr : Type
export data CData : Type

%extern prim__readFile : prim__WorldType -> Ptr -> String
%extern prim__readChars : prim__WorldType -> Int -> Ptr -> String
%extern prim__writeFile : prim__WorldType -> Ptr -> String -> Int

%extern prim__vm : prim__WorldType -> Ptr
%extern prim__stdin : Ptr
%extern prim__stdout : Ptr
%extern prim__stderr : Ptr

%extern prim__null : Ptr
%extern prim__managedNull : ManagedPtr
%extern prim__eqPtr : Ptr -> Ptr -> Int
%extern prim__eqManagedPtr : ManagedPtr -> ManagedPtr -> Int
%extern prim__registerPtr : Ptr -> Int -> ManagedPtr

-- primitives for accessing memory.
%extern prim__asPtr : ManagedPtr -> Ptr
%extern prim__sizeofPtr : Int
%extern prim__ptrOffset : Ptr -> Int -> Ptr
%extern prim__peek8 : prim__WorldType -> Ptr -> Int -> Bits8
%extern prim__peek16 : prim__WorldType -> Ptr -> Int -> Bits16
%extern prim__peek32 : prim__WorldType -> Ptr -> Int -> Bits32
%extern prim__peek64 : prim__WorldType -> Ptr -> Int -> Bits64

%extern prim__poke8 : prim__WorldType -> Ptr -> Int -> Bits8 -> Int
%extern prim__poke16 : prim__WorldType -> Ptr -> Int -> Bits16 -> Int
%extern prim__poke32 : prim__WorldType -> Ptr -> Int -> Bits32 -> Int
%extern prim__poke64 : prim__WorldType -> Ptr -> Int -> Bits64 -> Int

%extern prim__peekPtr : prim__WorldType -> Ptr -> Int -> Ptr
%extern prim__pokePtr : prim__WorldType -> Ptr -> Int -> Ptr -> Int

%extern pri__peekDouble : prim__WorldType -> Ptr -> Int -> Double
%extern prim__pokeDouble : prim__WorldType -> Ptr -> Int -> Double -> Int
%extern prim__peekSingle : prim__WorldType -> Ptr -> Int -> Double
%extern prim__pokeSingle : prim__WorldType -> Ptr -> Int -> Double -> Int
-}
