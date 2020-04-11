{-# LANGUAGE QuasiQuotes #-}
module Idris.PrimOps where

import Grin.ExtendedSyntax.TH
import Transformations.ExtendedSyntax.Conversion (convertToNew)
import qualified Grin.ExtendedSyntax.Syntax (Exp)
import Grin.ExtendedSyntax.Pretty (printGrinToFile)

renderNewPrimOps :: IO ()
renderNewPrimOps = do
  printGrinToFile "new-prim-ops.grin" $ idrisPrimOps

idrisPrimOps :: Grin.ExtendedSyntax.Syntax.Exp
idrisPrimOps = [progConst|
ffi effectful
  _prim_int_print :: T_Int64 -> T_Unit
  _prim_usleep :: T_Int64 -> T_Unit
  _prim_string_print :: T_String -> T_Unit
  _prim_read_string :: T_String
  _prim_error :: T_String -> T_Unit
  _prim_ffi_file_eof :: T_Int64 -> T_Int64

ffi pure
  _prim_string_concat :: T_String -> T_String -> T_String
  _prim_string_reverse :: T_String -> T_String
  _prim_string_lt :: T_String -> T_String -> T_Int64
  _prim_string_eq :: T_String -> T_String -> T_Int64
  _prim_string_head :: T_String -> T_Int64
  _prim_string_tail :: T_String -> T_String
  _prim_string_cons :: T_Int64 -> T_String -> T_String
  _prim_string_len :: T_String -> T_Int64
  _prim_int_str :: T_Int64 -> T_String
  _prim_str_int :: T_String -> T_Int64
  _prim_int_float :: T_Int64 -> T_Float
  _prim_float_string :: T_Float -> T_String
  _prim_char_int :: T_Char -> T_Int64

primop pure
  _prim_int_shr :: T_Int64 -> T_Int64
  _prim_int_add :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_sub :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_mul :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_div :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_ashr :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_eq :: T_Int64 -> T_Int64 -> T_Bool
  _prim_int_ne :: T_Int64 -> T_Int64 -> T_Bool
  _prim_int_gt :: T_Int64 -> T_Int64 -> T_Bool
  _prim_int_ge :: T_Int64 -> T_Int64 -> T_Bool
  _prim_int_lt :: T_Int64 -> T_Int64 -> T_Bool
  _prim_int_le :: T_Int64 -> T_Int64 -> T_Bool
  _prim_word_add :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_sub :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_mul :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_div :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_eq :: T_Word64 -> T_Word64 -> T_Bool
  _prim_word_ne :: T_Word64 -> T_Word64 -> T_Bool
  _prim_word_gt :: T_Word64 -> T_Word64 -> T_Bool
  _prim_word_ge :: T_Word64 -> T_Word64 -> T_Bool
  _prim_word_lt :: T_Word64 -> T_Word64 -> T_Bool
  _prim_word_le :: T_Word64 -> T_Word64 -> T_Bool
  _prim_float_add :: T_Float -> T_Float -> T_Float
  _prim_float_sub :: T_Float -> T_Float -> T_Float
  _prim_float_mul :: T_Float -> T_Float -> T_Float
  _prim_float_div :: T_Float -> T_Float -> T_Float
  _prim_float_eq :: T_Float -> T_Float -> T_Bool
  _prim_float_ne :: T_Float -> T_Float -> T_Bool
  _prim_float_gt :: T_Float -> T_Float -> T_Bool
  _prim_float_ge :: T_Float -> T_Float -> T_Bool
  _prim_float_lt :: T_Float -> T_Float -> T_Bool
  _prim_float_le :: T_Float -> T_Float -> T_Bool
  _prim_bool_eq :: T_Bool -> T_Bool -> T_Bool
  _prim_bool_ne :: T_Bool -> T_Bool -> T_Bool

ffi effectful
  _prim_new_ref :: %ref -> T_Word64
  _prim_write_ref :: T_Word64 -> %ref -> T_Unit
  _prim_read_ref :: T_Word64 -> %ref
  _prim_malloc :: T_Int64 -> T_Word64
  _prim_poke :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit
  _prim_peek :: T_Word64 -> T_Int64 -> T_Word64
  _prim_memset :: T_Word64
               -> T_Int64
               -> T_Word64
               -> T_Int64
               -> T_Unit
  _prim_memmove :: T_Word64
                -> T_Word64
                -> T_Int64
                -> T_Int64
                -> T_Int64
                -> T_Unit
  _prim_free :: T_Word64 -> T_Unit
  _prim_write_file :: T_Word64 -> T_String -> T_Int64
  _prim_read_chars :: T_Word64 -> T_Int64 -> T_String
  _prim_create_vm :: T_Word64
  _prim_get_vm :: %ref -> T_Word64
  _prim_poke8 :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit
  _prim_poke16 :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit
  _prim_poke32 :: T_Word64 -> T_Int64 -> T_Word64 -> T_Unit
  _prim_peek8 :: T_Word64 -> T_Int64 -> T_Word64
  _prim_peek16 :: T_Word64 -> T_Int64 -> T_Word64
  _prim_peek32 :: T_Word64 -> T_Int64 -> T_Word64
  _prim_peek64 :: T_Word64 -> T_Int64 -> T_Word64
  _prim_num_args :: T_Int64
  _prim_get_arg :: T_Int64 -> T_String
  _prim_check_messages_timeout :: T_Word64
                               -> T_Int64
                               -> T_Word64
  _prim_time :: T_Int64
  _prim_crash :: T_String -> T_Unit
  _prim_new_buffer :: T_Int64 -> T_Word64
  _prim_set_buffer_byte :: T_Word64
                        -> T_Int64
                        -> T_Word64
                        -> T_Unit
  _prim_set_buffer_string :: T_Word64
                          -> T_Int64
                          -> T_String
                          -> T_Unit
  _prim_set_buffer_int :: T_Word64
                       -> T_Int64
                       -> T_Int64
                       -> T_Unit
  _prim_set_buffer_double :: T_Word64
                          -> T_Int64
                          -> T_Float
                          -> T_Unit
  _prim_copy_buffer :: T_Word64
                    -> T_Int64
                    -> T_Int64
                    -> T_Word64
                    -> T_Int64
                    -> T_Unit
  _prim_write_buffer :: T_Word64
                     -> T_Word64
                     -> T_Int64
                     -> T_Int64
                     -> T_Unit
  _prim_file_close :: T_Word64 -> T_Unit
  _prim_read_buffer :: T_Word64
                    -> T_Word64
                    -> T_Int64
                    -> T_Int64
                    -> T_Int64
  _prim_file_eof :: T_Word64 -> T_Int64
  _prim_stdin :: T_Word64
  _prim_stdout :: T_Word64
  _prim_stderr :: T_Word64
  _prim_putchar :: T_Char -> T_Unit
  _prim_send_message :: T_Word64
                     -> T_Word64
                     -> T_Int64
                     -> %msg
                     -> T_Int64
  _prim_recv_message :: T_Word64 -> %msg

ffi pure
  _prim_get_buffer_byte :: T_Word64 -> T_Int64 -> T_Word64
  _prim_get_buffer_int :: T_Word64 -> T_Int64 -> T_Int64
  _prim_get_buffer_double :: T_Word64 -> T_Int64 -> T_Float
  _prim_get_buffer_string :: T_Word64
                          -> T_Int64
                          -> T_Int64
                          -> T_String
  _prim_file_open :: T_String -> T_String -> T_Word64
  _prim_file_error :: T_Word64 -> T_Int64
  _prim_null :: T_Word64
  _prim_eq_ptr :: T_Word64 -> T_Word64 -> T_Int64
  _prim_calloc :: T_Int64 -> T_Int64 -> T_Word64
  _prim_string_sub :: T_Int64 -> T_Int64 -> T_String -> T_String
  _prim_string_index :: T_String -> T_Int64
  _prim_float_int :: T_Float -> T_Int64
  _prim_float_floor :: T_Float -> T_Float
  _prim_float_ceil :: T_Float -> T_Float
  _prim_float_atan2 :: T_Float -> T_Float -> T_Float
  _prim_string_float :: T_String -> T_Float
  _prim_int_word :: T_Int64 -> T_Word64
  _prim_int_int :: T_Int64 -> T_Int64
  _prim_word_word :: T_Word64 -> T_Word64
  _prim_word_int :: T_Word64 -> T_Int64
  _prim_int_char :: T_Int64 -> T_Char
  _prim_int_bigint :: T_Int64 -> T_Int64

primop pure
  _prim_int_udiv :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_rem :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_shl :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_lshr :: T_Int64 -> T_Int64 -> T_Int64
  _prim_int_and :: T_Int64 -> T_Int64 -> T_Int64
  _prim_word_shl :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_and :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_lshr :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_udiv :: T_Word64 -> T_Word64 -> T_Word64
  _prim_word_urem :: T_Word64 -> T_Word64 -> T_Word64

prim__null =
  prim__null1 <- _prim_null $
  v.0 <- pure (CGrPtr prim__null1)
  pure v.0

prim__eqPtr prim__eqPtr1 prim__eqPtr2 =
  (CGrPtr prim__eqPtr3) @ p.1 <- fetch prim__eqPtr1
  (CGrPtr prim__eqPtr4) @ p.0 <- fetch prim__eqPtr2
  prim__eqPtr5 <- _prim_eq_ptr $ prim__eqPtr3 prim__eqPtr4
  v.1 <- pure (CGrInt prim__eqPtr5)
  pure v.1

idris_bit8_eq idris_bit8_eq0 idris_bit8_eq1 =
  (CGrBit8 idris_bit8_eq0_1) @ p.3 <- fetch idris_bit8_eq0
  (CGrBit8 idris_bit8_eq1_1) @ p.2 <- fetch idris_bit8_eq1
  idris_bit8_eq2 <- _prim_word_eq $ idris_bit8_eq0_1 idris_bit8_eq1_1
  case idris_bit8_eq2 of
    #False @ alt.0 ->
      y.2 <- pure 0
      v.2 <- pure (CGrInt y.2)
      pure v.2
    #True @ alt.1 ->
      y.3 <- pure 1
      v.3 <- pure (CGrInt y.3)
      pure v.3

idris_bit16_eq idris_bit16_eq0 idris_bit16_eq1 =
  (CGrBit16 idris_bit16_eq0_1) @ p.5 <- fetch idris_bit16_eq0
  (CGrBit16 idris_bit16_eq1_1) @ p.4 <- fetch idris_bit16_eq1
  idris_bit16_eq2 <- _prim_word_eq $ idris_bit16_eq0_1 idris_bit16_eq1_1
  case idris_bit16_eq2 of
    #False @ alt.2 ->
      y.4 <- pure 0
      v.4 <- pure (CGrInt y.4)
      pure v.4
    #True @ alt.3 ->
      y.5 <- pure 1
      v.5 <- pure (CGrInt y.5)
      pure v.5

idris_bit32_eq idris_bit32_eq0 idris_bit32_eq1 =
  (CGrBit32 idris_bit32_eq0_1) @ p.7 <- fetch idris_bit32_eq0
  (CGrBit32 idris_bit32_eq1_1) @ p.6 <- fetch idris_bit32_eq1
  idris_bit32_eq2 <- _prim_word_eq $ idris_bit32_eq0_1 idris_bit32_eq1_1
  case idris_bit32_eq2 of
    #False @ alt.4 ->
      y.6 <- pure 0
      v.6 <- pure (CGrInt y.6)
      pure v.6
    #True @ alt.5 ->
      y.7 <- pure 1
      v.7 <- pure (CGrInt y.7)
      pure v.7

idris_bit64_eq idris_bit64_eq0 idris_bit64_eq1 =
  (CGrBit64 idris_bit64_eq0_1) @ p.9 <- fetch idris_bit64_eq0
  (CGrBit64 idris_bit64_eq1_1) @ p.8 <- fetch idris_bit64_eq1
  idris_bit64_eq2 <- _prim_word_eq $ idris_bit64_eq0_1 idris_bit64_eq1_1
  case idris_bit64_eq2 of
    #False @ alt.6 ->
      y.8 <- pure 0
      v.8 <- pure (CGrInt y.8)
      pure v.8
    #True @ alt.7 ->
      y.9 <- pure 1
      v.9 <- pure (CGrInt y.9)
      pure v.9

idris_int_eq idris_int_eq0 idris_int_eq1 =
  (CGrInt idris_int_eq0_1) @ p.11 <- fetch idris_int_eq0
  (CGrInt idris_int_eq1_1) @ p.10 <- fetch idris_int_eq1
  idris_int_eq2 <- _prim_int_eq $ idris_int_eq0_1 idris_int_eq1_1
  case idris_int_eq2 of
    #False @ alt.8 ->
      y.10 <- pure 0
      v.10 <- pure (CGrInt y.10)
      pure v.10
    #True @ alt.9 ->
      y.11 <- pure 1
      v.11 <- pure (CGrInt y.11)
      pure v.11

idris_float_eq idris_float_eq0 idris_float_eq1 =
  (CGrFloat idris_float_eq0_1) @ p.13 <- fetch idris_float_eq0
  (CGrFloat idris_float_eq1_1) @ p.12 <- fetch idris_float_eq1
  idris_float_eq2 <- _prim_float_eq $ idris_float_eq0_1 idris_float_eq1_1
  case idris_float_eq2 of
    #False @ alt.10 ->
      y.12 <- pure 0
      v.12 <- pure (CGrInt y.12)
      pure v.12
    #True @ alt.11 ->
      y.13 <- pure 1
      v.13 <- pure (CGrInt y.13)
      pure v.13

idris_int_lt idris_int_lt0 idris_int_lt1 =
  (CGrInt idris_int_lt0_1) @ p.15 <- fetch idris_int_lt0
  (CGrInt idris_int_lt1_1) @ p.14 <- fetch idris_int_lt1
  idris_int_lt2 <- _prim_int_lt $ idris_int_lt0_1 idris_int_lt1_1
  case idris_int_lt2 of
    #False @ alt.12 ->
      y.14 <- pure 0
      v.14 <- pure (CGrInt y.14)
      pure v.14
    #True @ alt.13 ->
      y.15 <- pure 1
      v.15 <- pure (CGrInt y.15)
      pure v.15

idris_float_lt idris_float_lt0 idris_float_lt1 =
  (CGrFloat idris_float_lt0_1) @ p.17 <- fetch idris_float_lt0
  (CGrFloat idris_float_lt1_1) @ p.16 <- fetch idris_float_lt1
  idris_float_lt2 <- _prim_float_lt $ idris_float_lt0_1 idris_float_lt1_1
  case idris_float_lt2 of
    #False @ alt.14 ->
      y.16 <- pure 0
      v.16 <- pure (CGrInt y.16)
      pure v.16
    #True @ alt.15 ->
      y.17 <- pure 1
      v.17 <- pure (CGrInt y.17)
      pure v.17

idris_int_le idris_int_le0 idris_int_le1 =
  (CGrInt idris_int_le0_1) @ p.19 <- fetch idris_int_le0
  (CGrInt idris_int_le1_1) @ p.18 <- fetch idris_int_le1
  idris_int_le2 <- _prim_int_le $ idris_int_le0_1 idris_int_le1_1
  case idris_int_le2 of
    #False @ alt.16 ->
      y.18 <- pure 0
      v.18 <- pure (CGrInt y.18)
      pure v.18
    #True @ alt.17 ->
      y.19 <- pure 1
      v.19 <- pure (CGrInt y.19)
      pure v.19

idris_int_gt idris_int_gt0 idris_int_gt1 =
  (CGrInt idris_int_gt0_1) @ p.21 <- fetch idris_int_gt0
  (CGrInt idris_int_gt1_1) @ p.20 <- fetch idris_int_gt1
  idris_int_gt2 <- _prim_int_gt $ idris_int_gt0_1 idris_int_gt1_1
  case idris_int_gt2 of
    #False @ alt.18 ->
      y.20 <- pure 0
      v.20 <- pure (CGrInt y.20)
      pure v.20
    #True @ alt.19 ->
      y.21 <- pure 1
      v.21 <- pure (CGrInt y.21)
      pure v.21

idris_int_ge idris_int_ge0 idris_int_ge1 =
  (CGrInt idris_int_ge0_1) @ p.23 <- fetch idris_int_ge0
  (CGrInt idris_int_ge1_1) @ p.22 <- fetch idris_int_ge1
  idris_int_ge2 <- _prim_int_ge $ idris_int_ge0_1 idris_int_ge1_1
  case idris_int_ge2 of
    #False @ alt.20 ->
      y.22 <- pure 0
      v.22 <- pure (CGrInt y.22)
      pure v.22
    #True @ alt.21 ->
      y.23 <- pure 1
      v.23 <- pure (CGrInt y.23)
      pure v.23

idris_lashr_int idris_lashr_int1 idris_lashr_int2 =
  (CGrInt idris_lashr_int1_0) @ p.25 <- fetch idris_lashr_int1
  (CGrInt idris_lashr_int2_0) @ p.24 <- fetch idris_lashr_int2
  idris_lashr_int3 <- _prim_int_ashr $ idris_lashr_int1_0 idris_lashr_int2_0
  v.24 <- pure (CGrInt idris_lashr_int3)
  pure v.24

idris_bit64_shl idris_bit64_shl1 idris_bit64_shl2 =
  (CGrBit64 idris_bit64_shl3) @ p.27 <- fetch idris_bit64_shl1
  (CGrBit64 idris_bit64_shl4) @ p.26 <- fetch idris_bit64_shl2
  idris_bit64_shl5 <- _prim_word_shl $ idris_bit64_shl3 idris_bit64_shl4
  v.25 <- pure (CGrBit64 idris_bit64_shl5)
  pure v.25

idris_int_shl idris_int_shl1 idris_int_shl2 =
  (CGrInt idris_int_shl3) @ p.29 <- fetch idris_int_shl1
  (CGrInt idris_int_shl4) @ p.28 <- fetch idris_int_shl2
  idris_int_shl5 <- _prim_int_shl $ idris_int_shl3 idris_int_shl4
  v.26 <- pure (CGrInt idris_int_shl5)
  pure v.26

idris_bit8_lshr idris_bit8_lshr1 idris_bit8_lshr2 =
  (CGrBit8 idris_bit8_lshr3) @ p.31 <- fetch idris_bit8_lshr1
  (CGrBit8 idris_bit8_lshr4) @ p.30 <- fetch idris_bit8_lshr2
  idris_bit8_lshr5 <- _prim_word_lshr $ idris_bit8_lshr3 idris_bit8_lshr4
  v.27 <- pure (CGrBit8 idris_bit8_lshr5)
  pure v.27

idris_bit16_lshr idris_bit16_lshr1 idris_bit16_lshr2 =
  (CGrBit16 idris_bit16_lshr3) @ p.33 <- fetch idris_bit16_lshr1
  (CGrBit16 idris_bit16_lshr4) @ p.32 <- fetch idris_bit16_lshr2
  idris_bit16_lshr5 <- _prim_word_lshr $ idris_bit16_lshr3 idris_bit16_lshr4
  v.28 <- pure (CGrBit16 idris_bit16_lshr5)
  pure v.28

idris_bit32_lshr idris_bit32_lshr1 idris_bit32_lshr2 =
  (CGrBit32 idris_bit32_lshr3) @ p.35 <- fetch idris_bit32_lshr1
  (CGrBit32 idris_bit32_lshr4) @ p.34 <- fetch idris_bit32_lshr2
  idris_bit32_lshr5 <- _prim_word_lshr $ idris_bit32_lshr3 idris_bit32_lshr4
  v.29 <- pure (CGrBit32 idris_bit32_lshr5)
  pure v.29

idris_bit64_lshr idris_bit64_lshr1 idris_bit64_lshr2 =
  (CGrBit64 idris_bit64_lshr3) @ p.37 <- fetch idris_bit64_lshr1
  (CGrBit64 idris_bit64_lshr4) @ p.36 <- fetch idris_bit64_lshr2
  idris_bit64_lshr5 <- _prim_word_lshr $ idris_bit64_lshr3 idris_bit64_lshr4
  v.30 <- pure (CGrBit64 idris_bit64_lshr5)
  pure v.30

idris_int_lshr idris_int_lshr1 idris_int_lshr2 =
  (CGrInt idris_int_lshr3) @ p.39 <- fetch idris_int_lshr1
  (CGrInt idris_int_lshr4) @ p.38 <- fetch idris_int_lshr2
  idris_int_lshr5 <- _prim_int_lshr $ idris_int_lshr3 idris_int_lshr4
  v.31 <- pure (CGrInt idris_int_lshr5)
  pure v.31

idris_bit8_and idris_bit8_and1 idris_bit8_and2 =
  (CGrBit8 idris_bit8_and3) @ p.41 <- fetch idris_bit8_and1
  (CGrBit8 idris_bit8_and4) @ p.40 <- fetch idris_bit8_and2
  idris_bit8_and5 <- _prim_word_and $ idris_bit8_and3 idris_bit8_and4
  v.32 <- pure (CGrBit8 idris_bit8_and5)
  pure v.32

idris_bit64_and idris_bit64_and1 idris_bit64_and2 =
  (CGrBit64 idris_bit64_and3) @ p.43 <- fetch idris_bit64_and1
  (CGrBit64 idris_bit64_and4) @ p.42 <- fetch idris_bit64_and2
  idris_bit64_and5 <- _prim_word_and $ idris_bit64_and3 idris_bit64_and4
  v.33 <- pure (CGrBit64 idris_bit64_and5)
  pure v.33

idris_int_and idris_int_and1 idris_int_and2 =
  (CGrInt idris_int_and3) @ p.45 <- fetch idris_int_and1
  (CGrInt idris_int_and4) @ p.44 <- fetch idris_int_and2
  idris_int_and5 <- _prim_int_and $ idris_int_and3 idris_int_and4
  v.34 <- pure (CGrInt idris_int_and5)
  pure v.34

idris_int_print idris_int_print0 =
  (CGrInt idris_int_print0_1) @ p.47 <- fetch idris_int_print0
  p.46 <- _prim_int_print $ idris_int_print0_1
  v.35 <- pure (CUnit)
  pure v.35

idris_float_add idris_float_add0 idris_float_add1 =
  (CGrFloat idris_float_add0_1) @ p.49 <- fetch idris_float_add0
  (CGrFloat idris_float_add1_1) @ p.48 <- fetch idris_float_add1
  idris_float_add3 <- _prim_float_add $ idris_float_add0_1 idris_float_add1_1
  v.36 <- pure (CGrFloat idris_float_add3)
  pure v.36

idris_float_sub idris_float_sub0 idris_float_sub1 =
  (CGrFloat idris_float_sub0_1) @ p.51 <- fetch idris_float_sub0
  (CGrFloat idris_float_sub1_1) @ p.50 <- fetch idris_float_sub1
  idris_float_sub3 <- _prim_float_sub $ idris_float_sub0_1 idris_float_sub1_1
  v.37 <- pure (CGrFloat idris_float_sub3)
  pure v.37

idris_float_mul idris_float_mul0 idris_float_mul1 =
  (CGrFloat idris_float_mul0_1) @ p.53 <- fetch idris_float_mul0
  (CGrFloat idris_float_mul1_1) @ p.52 <- fetch idris_float_mul1
  idris_float_mul3 <- _prim_float_mul $ idris_float_mul0_1 idris_float_mul1_1
  v.38 <- pure (CGrFloat idris_float_mul3)
  pure v.38

idris_int_add idris_int_add0 idris_int_add1 =
  (CGrInt idris_int_add0_1) @ p.55 <- fetch idris_int_add0
  (CGrInt idris_int_add1_1) @ p.54 <- fetch idris_int_add1
  idris_int_add2 <- _prim_int_add $ idris_int_add0_1 idris_int_add1_1
  v.39 <- pure (CGrInt idris_int_add2)
  pure v.39

idris_bit64_add idris_bit64_add0 idris_bit64_add1 =
  (CGrBit64 idris_bit64_add0_1) @ p.57 <- fetch idris_bit64_add0
  (CGrBit64 idris_bit64_add1_1) @ p.56 <- fetch idris_bit64_add1
  idris_bit64_add2 <- _prim_word_add $ idris_bit64_add0_1 idris_bit64_add1_1
  v.40 <- pure (CGrBit64 idris_bit64_add2)
  pure v.40

idris_int_sub idris_int_sub0 idris_int_sub1 =
  (CGrInt idris_int_sub0_1) @ p.59 <- fetch idris_int_sub0
  (CGrInt idris_int_sub1_1) @ p.58 <- fetch idris_int_sub1
  idris_int_sub2 <- _prim_int_sub $ idris_int_sub0_1 idris_int_sub1_1
  v.41 <- pure (CGrInt idris_int_sub2)
  pure v.41

idris_int_mul idris_int_mul0 idris_int_mul1 =
  (CGrInt idris_int_mul0_1) @ p.61 <- fetch idris_int_mul0
  (CGrInt idris_int_mul1_1) @ p.60 <- fetch idris_int_mul1
  idris_int_mul2 <- _prim_int_mul $ idris_int_mul0_1 idris_int_mul1_1
  v.42 <- pure (CGrInt idris_int_mul2)
  pure v.42

idris_int_div idris_int_div0 idris_int_div1 =
  (CGrInt idris_int_div0_1) @ p.63 <- fetch idris_int_div0
  (CGrInt idris_int_div1_1) @ p.62 <- fetch idris_int_div1
  idris_int_div2 <- _prim_int_div $ idris_int_div0_1 idris_int_div1_1
  v.43 <- pure (CGrInt idris_int_div2)
  pure v.43

idris_bit8_udiv idris_bit8_udiv1 idris_bit8_udiv2 =
  (CGrBit8 idris_bit8_udiv3) @ p.65 <- fetch idris_bit8_udiv1
  (CGrBit8 idris_bit8_udiv4) @ p.64 <- fetch idris_bit8_udiv2
  idris_bit8_udiv5 <- _prim_word_udiv $ idris_bit8_udiv3 idris_bit8_udiv4
  v.44 <- pure (CGrBit8 idris_bit8_udiv5)
  pure v.44

idris_bit16_udiv idris_bit16_udiv1 idris_bit16_udiv2 =
  (CGrBit16 idris_bit16_udiv3) @ p.67 <- fetch idris_bit16_udiv1
  (CGrBit16 idris_bit16_udiv4) @ p.66 <- fetch idris_bit16_udiv2
  idris_bit16_udiv5 <- _prim_word_udiv $ idris_bit16_udiv3 idris_bit16_udiv4
  v.45 <- pure (CGrBit16 idris_bit16_udiv5)
  pure v.45

idris_bit32_udiv idris_bit32_udiv1 idris_bit32_udiv2 =
  (CGrBit32 idris_bit32_udiv3) @ p.69 <- fetch idris_bit32_udiv1
  (CGrBit32 idris_bit32_udiv4) @ p.68 <- fetch idris_bit32_udiv2
  idris_bit32_udiv5 <- _prim_word_udiv $ idris_bit32_udiv3 idris_bit32_udiv4
  v.46 <- pure (CGrBit32 idris_bit32_udiv5)
  pure v.46

idris_bit64_udiv idris_bit64_udiv1 idris_bit64_udiv2 =
  (CGrBit64 idris_bit64_udiv3) @ p.71 <- fetch idris_bit64_udiv1
  (CGrBit64 idris_bit64_udiv4) @ p.70 <- fetch idris_bit64_udiv2
  idris_bit64_udiv5 <- _prim_word_udiv $ idris_bit64_udiv3 idris_bit64_udiv4
  v.47 <- pure (CGrBit64 idris_bit64_udiv5)
  pure v.47

idris_int_udiv idris_int_udiv1 idris_int_udiv2 =
  (CGrInt idris_int_udiv3) @ p.73 <- fetch idris_int_udiv1
  (CGrInt idris_int_udiv4) @ p.72 <- fetch idris_int_udiv2
  idris_int_udiv5 <- _prim_int_udiv $ idris_int_udiv3 idris_int_udiv4
  v.48 <- pure (CGrInt idris_int_udiv5)
  pure v.48

idris_int_rem idris_int_rem1 idris_int_rem2 =
  (CGrInt idris_int_rem3) @ p.75 <- fetch idris_int_rem1
  (CGrInt idris_int_rem4) @ p.74 <- fetch idris_int_rem2
  idris_int_rem5 <- _prim_int_rem $ idris_int_rem3 idris_int_rem4
  v.49 <- pure (CGrInt idris_int_rem5)
  pure v.49

idris_int_urem idris_int_urem1 idris_int_urem2 =
  (CGrInt idris_int_urem3) @ p.77 <- fetch idris_int_urem1
  (CGrInt idris_int_urem4) @ p.76 <- fetch idris_int_urem2
  idris_int_urem5 <- _prim_int_urem $ idris_int_urem3 idris_int_urem4
  v.50 <- pure (CGrInt idris_int_urem5)
  pure v.50

idris_bit8_urem idris_bit8_urem1 idris_bit8_urem2 =
  (CGrBit8 idris_bit8_urem3) @ p.79 <- fetch idris_bit8_urem1
  (CGrBit8 idris_bit8_urem4) @ p.78 <- fetch idris_bit8_urem2
  idris_bit8_urem5 <- _prim_word_urem $ idris_bit8_urem3 idris_bit8_urem4
  v.51 <- pure (CGrBit8 idris_bit8_urem5)
  pure v.51

idris_bit16_urem idris_bit16_urem1 idris_bit16_urem2 =
  (CGrBit16 idris_bit16_urem3) @ p.81 <- fetch idris_bit16_urem1
  (CGrBit16 idris_bit16_urem4) @ p.80 <- fetch idris_bit16_urem2
  idris_bit16_urem5 <- _prim_word_urem $ idris_bit16_urem3 idris_bit16_urem4
  v.52 <- pure (CGrBit16 idris_bit16_urem5)
  pure v.52

idris_bit32_urem idris_bit32_urem1 idris_bit32_urem2 =
  (CGrBit32 idris_bit32_urem3) @ p.83 <- fetch idris_bit32_urem1
  (CGrBit32 idris_bit32_urem4) @ p.82 <- fetch idris_bit32_urem2
  idris_bit32_urem5 <- _prim_word_urem $ idris_bit32_urem3 idris_bit32_urem4
  v.53 <- pure (CGrBit32 idris_bit32_urem5)
  pure v.53

idris_bit64_urem idris_bit64_urem1 idris_bit64_urem2 =
  (CGrBit64 idris_bit64_urem3) @ p.85 <- fetch idris_bit64_urem1
  (CGrBit64 idris_bit64_urem4) @ p.84 <- fetch idris_bit64_urem2
  idris_bit64_urem5 <- _prim_word_urem $ idris_bit64_urem3 idris_bit64_urem4
  v.54 <- pure (CGrBit64 idris_bit64_urem5)
  pure v.54

idris_float_div idris_float_div0 idris_float_div1 =
  (CGrFloat idris_float_div0_1) @ p.87 <- fetch idris_float_div0
  (CGrFloat idris_float_div1_1) @ p.86 <- fetch idris_float_div1
  idris_float_div2 <- _prim_float_div $ idris_float_div0_1 idris_float_div1_1
  v.55 <- pure (CGrFloat idris_float_div2)
  pure v.55

idris_write_str idris_write_str1 idris_write_str2 =
  (CGrString idris_write_str2_0) @ p.89 <- fetch idris_write_str2
  p.88 <- _prim_string_print $ idris_write_str2_0
  v.56 <- pure (CUnit)
  pure v.56

idris_read_str idris_read_str0 =
  idris_read_str1 <- _prim_read_string $
  v.57 <- pure (CGrString idris_read_str1)
  pure v.57

idris_str_concat idris_str_concat1 idris_str_concat2 =
  (CGrString idris_str_concat1_0) @ p.91 <- fetch idris_str_concat1
  (CGrString idris_str_concat2_0) @ p.90 <- fetch idris_str_concat2
  idris_str_concat3 <- _prim_string_concat $ idris_str_concat1_0 idris_str_concat2_0
  v.58 <- pure (CGrString idris_str_concat3)
  pure v.58

idris_str_eq idris_str_eq1 idris_str_eq2 =
  (CGrString idris_str_eq1_0) @ p.93 <- fetch idris_str_eq1
  (CGrString idris_str_eq2_0) @ p.92 <- fetch idris_str_eq2
  idris_str_eq3 <- _prim_string_eq $ idris_str_eq1_0 idris_str_eq2_0
  v.59 <- pure (CGrInt idris_str_eq3)
  pure v.59

idris_str_lt idris_str_lt1 idris_str_lt2 =
  (CGrString idris_str_lt1_0) @ p.95 <- fetch idris_str_lt1
  (CGrString idris_str_lt2_0) @ p.94 <- fetch idris_str_lt2
  idris_str_lt3 <- _prim_string_lt $ idris_str_lt1_0 idris_str_lt2_0
  v.60 <- pure (CGrInt idris_str_lt3)
  pure v.60

idris_str_len idris_str_len1 =
  (CGrString idris_str_len2) @ p.96 <- fetch idris_str_len1
  idris_str_len3 <- _prim_string_len $ idris_str_len2
  v.61 <- pure (CGrInt idris_str_len3)
  pure v.61

idris_str_sub idris_str_sub1 idris_str_sub2 idris_str_sub3 =
  (CGrInt idris_str_sub4) @ p.99 <- fetch idris_str_sub1
  (CGrInt idris_str_sub5) @ p.98 <- fetch idris_str_sub2
  (CGrString idris_str_sub6) @ p.97 <- fetch idris_str_sub3
  idris_str_sub7 <- _prim_string_sub $ idris_str_sub4 idris_str_sub5 idris_str_sub6
  v.62 <- pure (CGrString idris_str_sub7)
  pure v.62

idris_str_rev idris_str_rev1 =
  (CGrString idris_str_rev1_0) @ p.100 <- fetch idris_str_rev1
  idris_str_rev2 <- _prim_string_reverse $ idris_str_rev1_0
  v.63 <- pure (CGrString idris_str_rev2)
  pure v.63

idris_str_head idris_str_head1 =
  (CGrString idris_str_head1_0) @ p.101 <- fetch idris_str_head1
  idris_str_head2 <- _prim_string_head $ idris_str_head1_0
  v.64 <- pure (CGrInt idris_str_head2)
  pure v.64

idris_str_tail idris_str_tail1 =
  (CGrString idris_str_tail1_0) @ p.102 <- fetch idris_str_tail1
  idris_str_tail2 <- _prim_string_tail $ idris_str_tail1_0
  v.65 <- pure (CGrString idris_str_tail2)
  pure v.65

idris_str_cons idris_str_cons1 idris_str_cons2 =
  (CGrInt idris_str_cons1_0) @ p.104 <- fetch idris_str_cons1
  (CGrString idris_str_cons2_0) @ p.103 <- fetch idris_str_cons2
  idris_str_cons3 <- _prim_string_cons $ idris_str_cons1_0 idris_str_cons2_0
  v.66 <- pure (CGrString idris_str_cons3)
  pure v.66

idris_str_idx idris_str_idx1 idris_str_idx2 =
  (CGrString idris_str_idx3) @ p.106 <- fetch idris_str_idx1
  (CGrInt idris_str_idx4) @ p.105 <- fetch idris_str_idx2
  idris_str_idx5 <- _prim_string_index $ idris_str_idx3 idris_str_idx4
  v.67 <- pure (CGrInt idris_str_idx5)
  pure v.67

idris_int_str idris_int_str1 =
  (CGrInt idris_int_str1_0) @ p.107 <- fetch idris_int_str1
  idris_int_str2 <- _prim_int_str $ idris_int_str1_0
  v.68 <- pure (CGrString idris_int_str2)
  pure v.68

idris_str_int idris_str_int1 =
  (CGrString idris_str_int1_0) @ p.108 <- fetch idris_str_int1
  idris_str_int2 <- _prim_str_int $ idris_str_int1_0
  v.69 <- pure (CGrInt idris_str_int2)
  pure v.69

idris_str_float idris_str_float1 =
  (CGrString idris_str_float2) @ p.109 <- fetch idris_str_float1
  idris_str_float3 <- _prim_string_float $ idris_str_float2
  v.70 <- pure (CGrFloat idris_str_float3)
  pure v.70

idris_int_float idris_int_float1 =
  (CGrInt idris_int_float1_0) @ p.110 <- fetch idris_int_float1
  idris_int_float2 <- _prim_int_float $ idris_int_float1_0
  v.71 <- pure (CGrFloat idris_int_float2)
  pure v.71

idris_float_int idris_float_int1 =
  (CGrFloat idris_float_int2) @ p.111 <- fetch idris_float_int1
  idris_float_int3 <- _prim_float_int $ idris_float_int2
  v.72 <- pure (CGrInt idris_float_int3)
  pure v.72

idris_float_str idris_float_str1 =
  (CGrFloat idris_float_str1_0) @ p.112 <- fetch idris_float_str1
  idris_float_str2 <- _prim_float_string $ idris_float_str1_0
  v.73 <- pure (CGrString idris_float_str2)
  pure v.73

idris_float_floor idris_float_floor1 =
  (CGrFloat idris_float_floor2) @ p.113 <- fetch idris_float_floor1
  idris_float_floor3 <- _prim_float_floor $ idris_float_floor2
  v.74 <- pure (CGrFloat idris_float_floor3)
  pure v.74

idris_float_ceil idris_float_ceil1 =
  (CGrFloat idris_float_ceil2) @ p.114 <- fetch idris_float_ceil1
  idris_float_ceil3 <- _prim_float_ceil $ idris_float_ceil2
  v.75 <- pure (CGrFloat idris_float_ceil3)
  pure v.75

idris_float_atan2 idris_float_atan2_1 idris_float_atan2_2 =
  (CGrFloat idris_float_atan2_3) @ p.116 <- fetch idris_float_atan2_1
  (CGrFloat idris_float_atan2_4) @ p.115 <- fetch idris_float_atan2_2
  idris_float_atan2_5 <- _prim_float_atan2 $ idris_float_atan2_3 idris_float_atan2_4
  v.76 <- pure (CGrFloat idris_float_atan2_5)
  pure v.76

idris_ffi_file_eof idris_ffi_file_eof1 =
  (CGrPtr idris_ffi_file_eof1_0) @ p.117 <- fetch idris_ffi_file_eof1
  idris_ffi_file_eof2 <- _prim_file_eof $ idris_ffi_file_eof1_0
  v.77 <- pure (CGrInt idris_ffi_file_eof2)
  pure v.77

idris_lz_ext_int_bigint idris_lz_ext_int_bigint1 =
  (CGrInt idris_lz_ext_int_bigint2) @ p.118 <- fetch idris_lz_ext_int_bigint1
  idris_lz_ext_int_bigint3 <- _prim_int_bigint $ idris_lz_ext_int_bigint2
  v.78 <- pure (CGrInt idris_lz_ext_int_bigint3)
  pure v.78

idris_lz_ext_int_bit64 idris_lz_ext_int_bit64_1 =
  (CGrInt idris_lz_ext_int_bit64_2) @ p.119 <- fetch idris_lz_ext_int_bit64_1
  idris_lz_ext_int_bit64_3 <- _prim_int_word $ idris_lz_ext_int_bit64_2
  v.79 <- pure (CGrBit64 idris_lz_ext_int_bit64_3)
  pure v.79

idris_lz_ext_bit8_int idris_lz_ext_bit8_int1 =
  (CGrBit8 idris_lz_ext_bit8_int2) @ p.120 <- fetch idris_lz_ext_bit8_int1
  idris_lz_ext_bit8_int3 <- _prim_word_int $ idris_lz_ext_bit8_int2
  v.80 <- pure (CGrInt idris_lz_ext_bit8_int3)
  pure v.80

idris_lz_ext_bit8_bit64 idris_lz_ext_bit8_bit64_1 =
  (CGrBit8 idris_lz_ext_bit8_bit64_2) @ p.121 <- fetch idris_lz_ext_bit8_bit64_1
  idris_lz_ext_bit8_bit64_3 <- _prim_word_word $ idris_lz_ext_bit8_bit64_2
  v.81 <- pure (CGrBit64 idris_lz_ext_bit8_bit64_3)
  pure v.81

idris_lz_ext idris_lz_ext1 =
  (CGrInt idris_lz_ext2) @ p.122 <- fetch idris_lz_ext1
  idris_lz_ext3 <- _prim_int_int $ idris_lz_ext2
  v.82 <- pure (CGrInt idris_lz_ext3)
  pure v.82

idris_ls_ext idris_ls_ext1 =
  (CGrInt idris_ls_ext2) @ p.123 <- fetch idris_ls_ext1
  x.122 <- pure 0
  idris_ls_ext3 <- _prim_int_add $ idris_ls_ext2 x.122
  v.83 <- pure (CGrInt idris_ls_ext3)
  pure v.83

idris_ch_int idris_ch_int1 =
  (CGrInt idris_ch_int2) @ p.124 <- fetch idris_ch_int1
  v.84 <- pure (CGrInt idris_ch_int2)
  pure v.84

idris_int_ch idris_int_ch1 =
  (CGrInt idris_int_ch2) @ p.125 <- fetch idris_int_ch1
  v.85 <- pure (CGrInt idris_int_ch2)
  pure v.85

idris_usleep idris_usleep1 =
  (CGrInt idris_usleep1_0) @ p.127 <- fetch idris_usleep1
  p.126 <- _prim_usleep $ idris_usleep1_0
  v.86 <- pure (CGrVoid)
  pure v.86

idris_crash idris_crash1 =
  (CGrString idris_crash2) @ p.129 <- fetch idris_crash1
  p.128 <- _prim_crash $ idris_crash2
  v.87 <- pure (CGrVoid)
  pure v.87

idris_ltrunc_big_int idris_ltrunc_big_int1 =
  (CGrInt idris_ltrunc_big_int2) @ p.130 <- fetch idris_ltrunc_big_int1
  idris_ltrunc_big_int3 <- _prim_int_int $ idris_ltrunc_big_int2
  v.88 <- pure (CGrInt idris_ltrunc_big_int3)
  pure v.88

idris_ltrunc_int_bit8 idris_ltrunc_int_bit8_1 =
  (CGrInt idris_ltrunc_int_bit8_2) @ p.131 <- fetch idris_ltrunc_int_bit8_1
  idris_ltrunc_int_bit8_3 <- _prim_int_word $ idris_ltrunc_int_bit8_2
  v.89 <- pure (CGrBit8 idris_ltrunc_int_bit8_3)
  pure v.89

idris_ltrunc_big_bit64 idris_ltrunc_big_bit64_1 =
  (CGrInt idris_ltrunc_big_bit64_2) @ p.132 <- fetch idris_ltrunc_big_bit64_1
  idris_ltrunc_big_bit64_3 <- _prim_int_word $ idris_ltrunc_big_bit64_2
  v.90 <- pure (CGrBit64 idris_ltrunc_big_bit64_3)
  pure v.90

idris_ltrunc_bit64_bit8 idris_ltrunc_bit64_bit8_1 =
  (CGrBit64 idris_ltrunc_bit64_bit8_2) @ p.133 <- fetch idris_ltrunc_bit64_bit8_1
  v.91 <- pure (CGrBit8 idris_ltrunc_bit64_bit8_2)
  pure v.91

idris_ltrunc_bit32_bit8 idris_ltrunc_bit32_bit8_1 =
  (CGrBit32 idris_ltrunc_bit32_bit8_2) @ p.134 <- fetch idris_ltrunc_bit32_bit8_1
  v.92 <- pure (CGrBit8 idris_ltrunc_bit32_bit8_2)
  pure v.92

idris_ltrunc_bit16_bit8 idris_ltrunc_bit16_bit8_1 =
  (CGrBit16 idris_ltrunc_bit16_bit8_2) @ p.135 <- fetch idris_ltrunc_bit16_bit8_1
  v.93 <- pure (CGrBit8 idris_ltrunc_bit16_bit8_2)
  pure v.93

idris_ltrunc idris_ltrunc1 =
  (CGrInt idris_ltrunc1_0) @ p.136 <- fetch idris_ltrunc1
  v.94 <- pure (CGrInt idris_ltrunc1_0)
  pure v.94

idris_error idris_error1 =
  p.137 <- _prim_error $ idris_error1
  v.95 <- pure (CGrError idris_error1)
  pure v.95

idris_time =
  idris_time1 <- _prim_time $
  v.96 <- pure (CGrInt idris_time1)
  pure v.96

idris_fileSize idris_fileSize1 =
  idris_fileSize2 <- pure 1024
  v.97 <- pure (CGrInt idris_fileSize2)
  pure v.97

idris_fileOpen idris_fileOpen1 idris_fileOpen2 =
  (CGrString idris_fileOpen3) @ p.139 <- fetch idris_fileOpen1
  (CGrString idris_fileOpen4) @ p.138 <- fetch idris_fileOpen2
  idris_fileOpen5 <- _prim_file_open $ idris_fileOpen3 idris_fileOpen4
  v.98 <- pure (CGrPtr idris_fileOpen5)
  pure v.98

fileError fileError1 =
  (CGrPtr fileError2) @ p.140 <- fetch fileError1
  fileError3 <- _prim_file_error $ fileError2
  v.99 <- pure (CGrInt fileError3)
  pure v.99

idris_fileClose idris_fileClose1 =
  (CGrPtr idris_fileClose2) @ p.141 <- fetch idris_fileClose1
  idris_fileClose3 <- _prim_file_close $ idris_fileClose2
  v.100 <- pure (CGrVoid)
  pure v.100

idris_addToString idris_addToString1 idris_addToString2 =
  (CGrPtr idris_addToString2_0) @ p.143 <- fetch idris_addToString2
  (CGrString idris_addToString1_0) @ p.142 <- fetch idris_addToString1
  v.101 <- pure (CGrVoid)
  pure v.101

idris_mkFileError idris_mkFileError0 =
  v.102 <- pure (CGrFileError)
  pure v.102

idris_getString idris_getString1 idris_getString2 =
  idris_getString3 <- pure #"TODO"
  v.103 <- pure (CGrString idris_getString3)
  pure v.103

idris_makeStringBuffer idris_makeStringBuffer1 =
  idris_makeStringBuffer2 <- pure 42
  v.104 <- pure (CGrPtr idris_makeStringBuffer2)
  pure v.104

isNull isNull1 =
  (CGrPtr isNull2) @ p.144 <- fetch isNull1
  x.133 <- pure 0
  isNull6 <- _prim_int_word $ x.133
  isNull3 <- _prim_word_eq $ isNull2 isNull6
  isNull4 <- case isNull3 of
    #False @ alt.22 ->
      pure 0
    #True @ alt.23 ->
      pure 1
  v.105 <- pure (CGrInt isNull4)
  pure v.105

idris_bit64_mul idris_bit64_mul1 idris_bit64_mul2 =
  (CGrBit64 idris_bit64_mul3) @ p.146 <- fetch idris_bit64_mul1
  (CGrBit64 idris_bit64_mul4) @ p.145 <- fetch idris_bit64_mul2
  idris_bit64_mul5 <- _prim_word_mul $ idris_bit64_mul3 idris_bit64_mul4
  v.106 <- pure (CGrBit64 idris_bit64_mul5)
  pure v.106

idris_errno =
  v.107 <- pure (CErrorNo)
  pure v.107

prim__stdin =
  prim__stdin1 <- _prim_stdin $
  v.108 <- pure (CGrPtr prim__stdin1)
  pure v.108

prim__stdout =
  prim__stdout1 <- _prim_stdout $
  v.109 <- pure (CGrPtr prim__stdout1)
  pure v.109

prim__stderr =
  prim__stderr1 <- _prim_stderr $
  v.110 <- pure (CGrPt prim__stderr1)
  pure v.110

prim__vm prim__vm1 =
  prim__vm3 <- _prim_get_vm $ prim__vm1
  v.111 <- pure (CGrVM prim__vm3)
  pure v.111

prim__writeFile prim__writeFile1 prim__writeFile2 prim__writeFile3 =
  (CGrPtr prim__writeFile4) @ p.148 <- fetch prim__writeFile2
  (CGrString prim__writeFile5) @ p.147 <- fetch prim__writeFile3
  prim__writeFile6 <- _prim_write_file $ prim__writeFile4 prim__writeFile5
  v.112 <- pure (CGrInt prim__writeFile6)
  pure v.112

prim__readChars prim__readChars1 prim__readChars2 prim__readChars3 =
  (CGrInt prim__readChars4) @ p.150 <- fetch prim__readChars2
  (CGrPtr prim__readChars5) @ p.149 <- fetch prim__readChars3
  prim__readChars6 <- _prim_read_chars $ prim__readChars5 prim__readChars4
  v.113 <- pure (CGrString prim__readChars6)
  pure v.113

prim__poke8 prim__poke8_1 prim__poke8_2 prim__poke8_3 prim__poke8_4 =
  (CGrPtr prim__poke8_5) @ p.153 <- fetch prim__poke8_2
  (CGrInt prim__poke8_6) @ p.152 <- fetch prim__poke8_3
  (CGrBit8 prim__poke8_7) @ p.151 <- fetch prim__poke8_4
  prim__poke8_8 <- _prim_poke8 $ prim__poke8_5 prim__poke8_6 prim__poke8_7
  prim__poke8_9 <- pure 0
  v.114 <- pure (CGrInt prim__poke8_9)
  pure v.114

prim__poke16 prim__poke16_1 prim__poke16_2 prim__poke16_3 prim__poke16_4 =
  (CGrPtr prim__poke16_5) @ p.156 <- fetch prim__poke16_2
  (CGrInt prim__poke16_6) @ p.155 <- fetch prim__poke16_3
  (CGrBit16 prim__poke16_7) @ p.154 <- fetch prim__poke16_4
  prim__poke16_8 <- _prim_poke16 $ prim__poke16_5 prim__poke16_6 prim__poke16_7
  prim__poke16_9 <- pure 0
  v.115 <- pure (CGrInt prim__poke16_9)
  pure v.115

prim__poke32 prim__poke32_1 prim__poke32_2 prim__poke32_3 prim__poke32_4 =
  (CGrPtr prim__poke32_5) @ p.159 <- fetch prim__poke32_2
  (CGrInt prim__poke32_6) @ p.158 <- fetch prim__poke32_3
  (CGrBit32 prim__poke32_7) @ p.157 <- fetch prim__poke32_4
  prim__poke32_8 <- _prim_poke32 $ prim__poke32_5 prim__poke32_6 prim__poke32_7
  prim__poke32_9 <- pure 0
  v.116 <- pure (CGrInt prim__poke32_9)
  pure v.116

prim__peek8 prim__peek8_0 prim__peek8_1 prim__peek8_2 =
  (CGrPtr prim__peek8_3) @ p.161 <- fetch prim__peek8_1
  (CGrInt prim__peek8_4) @ p.160 <- fetch prim__peek8_2
  prim__peek8_5 <- _prim_peek8 $ prim__peek8_3 prim__peek8_4
  v.117 <- pure (CGrBit8 prim__peek8_5)
  pure v.117

prim__peek16 prim__peek16_0 prim__peek16_1 prim__peek16_2 =
  (CGrPtr prim__peek16_3) @ p.163 <- fetch prim__peek16_1
  (CGrInt prim__peek16_4) @ p.162 <- fetch prim__peek16_2
  prim__peek16_5 <- _prim_peek16 $ prim__peek16_3 prim__peek16_4
  v.118 <- pure (CGrBit16 prim__peek16_5)
  pure v.118

prim__peek32 prim__peek32_0 prim__peek32_1 prim__peek32_2 =
  (CGrPtr prim__peek32_3) @ p.165 <- fetch prim__peek32_1
  (CGrInt prim__peek32_4) @ p.164 <- fetch prim__peek32_2
  prim__peek32_5 <- _prim_peek32 $ prim__peek32_3 prim__peek32_4
  v.119 <- pure (CGrBit32 prim__peek32_5)
  pure v.119

prim__peek64 prim__peek64_0 prim__peek64_1 prim__peek64_2 =
  (CGrPtr prim__peek64_3) @ p.167 <- fetch prim__peek64_1
  (CGrInt prim__peek64_4) @ p.166 <- fetch prim__peek64_2
  prim__peek64_5 <- _prim_peek64 $ prim__peek64_3 prim__peek64_4
  v.120 <- pure (CGrBit64 prim__peek64_5)
  pure v.120

idris_newBuffer idris_newBuffer1 idris_newBuffer2 =
  (CGrVM idris_newBuffer5) @ p.169 <- fetch idris_newBuffer1
  (CGrInt idris_newBuffer3) @ p.168 <- fetch idris_newBuffer2
  idris_newBuffer4 <- _prim_new_buffer $ idris_newBuffer3
  v.121 <- pure (CGrPtr idris_newBuffer4)
  pure v.121

idris_newRef idris_newRef1 =
  idris_newRef2 <- fetch idris_newRef1
  idris_newRef3 <- _prim_new_ref $ idris_newRef2
  v.122 <- pure (CGrRef idris_newRef3)
  pure v.122

idris_readRef idris_readRef1 =
  (CGrRef idris_readRef2) @ p.170 <- fetch idris_readRef1
  idris_readRef3 <- _prim_read_ref $ idris_readRef2
  pure idris_readRef3

idris_writeRef idris_writeRef1 idris_writeRef2 =
  (CGrRef idris_writeRef3) @ p.171 <- fetch idris_writeRef1
  idris_writeRef4 <- fetch idris_writeRef2
  idris_writeRef5 <- _prim_write_ref $ idris_writeRef3 idris_writeRef4
  v.123 <- pure (CGrUnit)
  pure v.123

idris_copyBuffer idris_copyBuffer1 idris_copyBuffer2 idris_copyBuffer3 idris_copyBuffer4 idris_copyBuffer5 =
  (CGrPtr idris_copyBuffer6) @ p.176 <- fetch idris_copyBuffer1
  (CGrInt idris_copyBuffer7) @ p.175 <- fetch idris_copyBuffer2
  (CGrInt idris_copyBuffer8) @ p.174 <- fetch idris_copyBuffer3
  (CGrPtr idris_copyBuffer9) @ p.173 <- fetch idris_copyBuffer4
  (CGrInt idris_copyBuffer10) @ p.172 <- fetch idris_copyBuffer5
  idris_copyBuffer11 <- _prim_copy_buffer $ idris_copyBuffer6 idris_copyBuffer7 idris_copyBuffer8 idris_copyBuffer9 idris_copyBuffer10
  v.124 <- pure (CGrUnit)
  pure v.124

idris_getBufferByte idris_getBufferByte1 idris_getBufferByte2 =
  (CGrPtr idris_getBufferByte3) @ p.178 <- fetch idris_getBufferByte1
  (CGrInt idris_getBufferByte4) @ p.177 <- fetch idris_getBufferByte2
  idris_getBufferByte5 <- _prim_get_buffer_byte $ idris_getBufferByte3 idris_getBufferByte4
  v.125 <- pure (CGrBit8 idris_getBufferByte5)
  pure v.125

idris_getBufferDouble idris_getBufferDouble1 idris_getBufferDouble2 =
  (CGrPtr idris_getBufferDouble3) @ p.180 <- fetch idris_getBufferDouble1
  (CGrInt idris_getBufferDouble4) @ p.179 <- fetch idris_getBufferDouble2
  idris_getBufferDouble5 <- _prim_get_buffer_double $ idris_getBufferDouble3 idris_getBufferDouble4
  v.126 <- pure (CGrFloat idris_getBufferDouble5)
  pure v.126

fflush fflush1 fflush2 =
  (CGrUndefined25) @ p.182 <- fetch fflush1
  (CGrUndefined26) @ p.181 <- fetch fflush2
  v.127 <- pure (CGrUndefined27)
  pure v.127

exit exit1 =
  (CGrUndefined28) @ p.183 <- fetch exit1
  v.128 <- pure (CGrUndefined29)
  pure v.128

idris_freeMsg idris_freeMsg1 =
  (CGrUndefined30) @ p.184 <- fetch idris_freeMsg1
  v.129 <- pure (CGrUndefined31)
  pure v.129

free free1 =
  (CGrPtr free2) @ p.185 <- fetch free1
  free3 <- _prim_free $ free2
  v.130 <- pure (CGrUnit)
  pure v.130

idris_showerror idris_showerror1 =
  (CGrUndefined34) @ p.186 <- fetch idris_showerror1
  v.131 <- pure (CGrUndefined35)
  pure v.131

idris_numArgs =
  idris_numArgs1 <- _prim_num_args $
  v.132 <- pure (CGrInt idris_numArgs1)
  pure v.132

fgetc fgetc1 =
  (CGrUndefined37) @ p.187 <- fetch fgetc1
  v.133 <- pure (CGrUndefined38)
  pure v.133

getchar =
  v.134 <- pure (CGrUndefined39)
  pure v.134

idris_memmove idris_memmove1 idris_memmove2 idris_memmove3 idris_memmove4 idris_memmove5 =
  (CGrPtr idris_memmove6) @ p.192 <- fetch idris_memmove1
  (CGrPtr idris_memmove7) @ p.191 <- fetch idris_memmove2
  (CGrInt idris_memmove8) @ p.190 <- fetch idris_memmove3
  (CGrInt idris_memmove9) @ p.189 <- fetch idris_memmove4
  (CGrInt idris_memmove10) @ p.188 <- fetch idris_memmove5
  idris_memmove11 <- _prim_memmove $ idris_memmove6 idris_memmove7 idris_memmove8 idris_memmove9 idris_memmove10
  v.135 <- pure (CGrUnit)
  pure v.135

calloc calloc1 calloc2 =
  (CGrInt calloc3) @ p.194 <- fetch calloc1
  (CGrInt calloc4) @ p.193 <- fetch calloc2
  calloc5 <- _prim_calloc $ calloc3 calloc4
  v.136 <- pure (CGrPtr calloc5)
  pure v.136

idris_getSender idris_getSender1 =
  (CGrUndefined49) @ p.195 <- fetch idris_getSender1
  v.137 <- pure (CGrUndefined50)
  pure v.137

idris_getArg idris_getArg1 =
  (CGrInt idris_getArg2) @ p.196 <- fetch idris_getArg1
  idris_getArg3 <- _prim_get_arg $ idris_getArg2
  v.138 <- pure (CGrString idris_getArg3)
  pure v.138

idris_readBuffer idris_readBuffer1 idris_readBuffer2 idris_readBuffer3 idris_readBuffer4 =
  (CGrPtr idris_readBuffer5) @ p.200 <- fetch idris_readBuffer1
  (CGrPtr idris_readBuffer6) @ p.199 <- fetch idris_readBuffer2
  (CGrInt idris_readBuffer7) @ p.198 <- fetch idris_readBuffer3
  (CGrInt idris_readBuffer8) @ p.197 <- fetch idris_readBuffer4
  idris_readBuffer9 <- _prim_read_buffer $ idris_readBuffer5 idris_readBuffer6 idris_readBuffer7 idris_readBuffer8
  v.139 <- pure (CGrInt idris_readBuffer9)
  pure v.139

idris_getBufferInt idris_getBufferInt1 idris_getBufferInt2 =
  (CGrPtr idris_getBufferInt3) @ p.202 <- fetch idris_getBufferInt1
  (CGrInt idris_getBufferInt4) @ p.201 <- fetch idris_getBufferInt2
  idris_getBufferInt5 <- _prim_get_buffer_int $ idris_getBufferInt3 idris_getBufferInt4
  v.140 <- pure (CGrInt idris_getBufferInt5)
  pure v.140

putchar putchar1 =
  (CGrInt putchar2) @ p.203 <- fetch putchar1
  putchar3 <- _prim_int_char $ putchar2
  putchar4 <- _prim_putchar $ putchar3
  v.141 <- pure (CGrUnit)
  pure v.141

idris_memset idris_memset1 idris_memset2 idris_memset3 idris_memset4 =
  (CGrPtr idris_memset5) @ p.207 <- fetch idris_memset1
  (CGrInt idris_memset6) @ p.206 <- fetch idris_memset2
  (CGrBit8 idris_memset7) @ p.205 <- fetch idris_memset3
  (CGrInt idris_memset8) @ p.204 <- fetch idris_memset4
  idris_memset9 <- _prim_memset $ idris_memset5 idris_memset6 idris_memset7 idris_memset8
  v.142 <- pure (CGrUnit)
  pure v.142

idris_getChannel idris_getChannel1 =
  (CGrUndefined63) @ p.208 <- fetch idris_getChannel1
  v.143 <- pure (CGrUndefined64)
  pure v.143

idris_setBufferByte idris_setBufferByte1 idris_setBufferByte2 idris_setBufferByte3 =
  (CGrPtr idris_setBufferByte4) @ p.211 <- fetch idris_setBufferByte1
  (CGrInt idris_setBufferByte5) @ p.210 <- fetch idris_setBufferByte2
  (CGrBit8 idris_setBufferByte6) @ p.209 <- fetch idris_setBufferByte3
  idris_setBufferByte7 <- _prim_set_buffer_byte $ idris_setBufferByte4 idris_setBufferByte5 idris_setBufferByte6
  v.144 <- pure (CGrUnit)
  pure v.144

idris_getBufferString idris_getBufferString1 idris_getBufferString2 idris_getBufferString3 =
  (CGrPtr idris_getBufferString4) @ p.214 <- fetch idris_getBufferString1
  (CGrInt idris_getBufferString5) @ p.213 <- fetch idris_getBufferString2
  (CGrInt idris_getBufferString6) @ p.212 <- fetch idris_getBufferString3
  idris_getBufferString7 <- _prim_get_buffer_string $ idris_getBufferString4 idris_getBufferString5 idris_getBufferString6
  v.145 <- pure (CGrString idris_getBufferString7)
  pure v.145

idris_peek idris_peek1 idris_peek2 =
  (CGrPtr idris_peek3) @ p.216 <- fetch idris_peek1
  (CGrInt idris_peek4) @ p.215 <- fetch idris_peek2
  idris_peek5 <- _prim_peek $ idris_peek3 idris_peek4
  v.146 <- pure (CGrBit8 idris_peek5)
  pure v.146

idris_recvMessage idris_recvMessage1 =
  (CGrVM idris_recvMessage2) @ p.217 <- fetch idris_recvMessage1
  idris_recvMessage3 <- _prim_recv_message $ idris_recvMessage2
  pure idris_recvMessage3

idris_checkMessages idris_checkMessages1 =
  (CGrUndefined78) @ p.218 <- fetch idris_checkMessages1
  v.147 <- pure (CGrUndefined79)
  pure v.147

idris_setBufferString idris_setBufferString1 idris_setBufferString2 idris_setBufferString3 =
  (CGrPtr idris_setBufferString4) @ p.221 <- fetch idris_setBufferString1
  (CGrInt idris_setBufferString5) @ p.220 <- fetch idris_setBufferString2
  (CGrString idris_setBufferString6) @ p.219 <- fetch idris_setBufferString3
  idris_setBufferString7 <- _prim_set_buffer_string $ idris_setBufferString4 idris_setBufferString5 idris_setBufferString6
  v.148 <- pure (CGrUnit)
  pure v.148

idris_setBufferDouble idris_setBufferDouble1 idris_setBufferDouble2 idris_setBufferDouble3 =
  (CGrPtr idris_setBufferDouble4) @ p.224 <- fetch idris_setBufferDouble1
  (CGrInt idris_setBufferDouble5) @ p.223 <- fetch idris_setBufferDouble2
  (CGrFloat idris_setBufferDouble6) @ p.222 <- fetch idris_setBufferDouble3
  idris_setBufferDouble7 <- _prim_set_buffer_double $ idris_setBufferDouble4 idris_setBufferDouble5 idris_setBufferDouble6
  v.149 <- pure (CGrUnit)
  pure v.149

idris_poke idris_poke1 idris_poke2 idris_poke3 =
  (CGrPtr idris_poke4) @ p.227 <- fetch idris_poke1
  (CGrInt idris_poke5) @ p.226 <- fetch idris_poke2
  (CGrBit8 idris_poke6) @ p.225 <- fetch idris_poke3
  idris_poke7 <- _prim_poke $ idris_poke4 idris_poke5 idris_poke6
  v.150 <- pure (CGrUnit)
  pure v.150

idris_checkMessagesTimeout idris_checkMessagesTimeout1 idris_checkMessagesTimeout2 =
  (CGrVM idris_checkMessagesTimeout3) @ p.229 <- fetch idris_checkMessagesTimeout1
  (CGrInt idris_checkMessagesTimeout4) @ p.228 <- fetch idris_checkMessagesTimeout2
  idris_checkMessagesTimeout5 <- _prim_check_messages_timeout $ idris_checkMessagesTimeout3 idris_checkMessagesTimeout4
  v.151 <- pure (CGrPtr idris_checkMessagesTimeout5)
  pure v.151

idris_getMsg idris_getMsg1 =
  (CGrUndefined95) @ p.230 <- fetch idris_getMsg1
  v.152 <- pure (CGrUndefined96)
  pure v.152

idris_sendMessage idris_sendMessage1 idris_sendMessage2 idris_sendMessage3 idris_sendMessage4 =
  (CGrVM idris_sendMessage8) @ p.233 <- fetch idris_sendMessage1
  (CGrInt idris_sendMessage5) @ p.232 <- fetch idris_sendMessage2
  (CGrVM idris_sendMessage9) @ p.231 <- fetch idris_sendMessage3
  idris_sendMessage6 <- fetch idris_sendMessage4
  idris_sendMessage7 <- _prim_send_message $ idris_sendMessage8 idris_sendMessage9 idris_sendMessage5 idris_sendMessage6
  v.153 <- pure (CGrInt idris_sendMessage7)
  pure v.153

idris_writeBuffer idris_writeBuffer1 idris_writeBuffer2 idris_writeBuffer3 idris_writeBuffer4 =
  (CGrPtr idris_writeBuffer5) @ p.237 <- fetch idris_writeBuffer1
  (CGrPtr idris_writeBuffer6) @ p.236 <- fetch idris_writeBuffer2
  (CGrInt idris_writeBuffer7) @ p.235 <- fetch idris_writeBuffer3
  (CGrInt idris_writeBuffer8) @ p.234 <- fetch idris_writeBuffer4
  idris_writeBuffer9 <- _prim_write_buffer $ idris_writeBuffer5 idris_writeBuffer6 idris_writeBuffer7 idris_writeBuffer8
  v.154 <- pure (CGrUnit)
  pure v.154

idris_setBufferInt idris_setBufferInt1 idris_setBufferInt2 idris_setBufferInt3 =
  (CGrPtr idris_setBufferInt4) @ p.240 <- fetch idris_setBufferInt1
  (CGrInt idris_setBufferInt5) @ p.239 <- fetch idris_setBufferInt2
  (CGrInt idris_setBufferInt6) @ p.238 <- fetch idris_setBufferInt3
  idris_setBufferInt7 <- _prim_set_buffer_int $ idris_setBufferInt4 idris_setBufferInt5 idris_setBufferInt6
  v.155 <- pure (CGrUnit)
  pure v.155

malloc malloc1 =
  (CGrInt malloc2) @ p.241 <- fetch malloc1
  malloc3 <- _prim_malloc $ malloc2
  v.156 <- pure (CGrPtr malloc3)
  pure v.156

idris_fork idris_fork1 =
  idris_fork2 <- "idr_{EVAL_0}" $ idris_fork1
  idris_fork3 <- _prim_create_vm $
  v.157 <- pure (CGrVM idris_fork3)
  pure v.157

grinMain =
  r <- "idr_{runMain_0}" $
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

%extern prim__managedNull : ManagedPtr
%extern prim__eqManagedPtr : ManagedPtr -> ManagedPtr -> Int
%extern prim__registerPtr : Ptr -> Int -> ManagedPtr

-- primitives for accessing memory.
%extern prim__asPtr : ManagedPtr -> Ptr
%extern prim__sizeofPtr : Int
%extern prim__ptrOffset : Ptr -> Int -> Ptr

%extern prim__peek64 : prim__WorldType -> Ptr -> Int -> Bits64
%extern prim__poke64 : prim__WorldType -> Ptr -> Int -> Bits64 -> Int

%extern prim__peekPtr : prim__WorldType -> Ptr -> Int -> Ptr
%extern prim__pokePtr : prim__WorldType -> Ptr -> Int -> Ptr -> Int

%extern prim__peekDouble : prim__WorldType -> Ptr -> Int -> Double
%extern prim__pokeDouble : prim__WorldType -> Ptr -> Int -> Double -> Int
%extern prim__peekSingle : prim__WorldType -> Ptr -> Int -> Double
%extern prim__pokeSingle : prim__WorldType -> Ptr -> Int -> Double -> Int
-}
