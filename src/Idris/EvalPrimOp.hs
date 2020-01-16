{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Idris.EvalPrimOp
  ( evalPrimOp
  , createBuffer
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Bits
import Data.Char (chr, ord)
import Data.Functor.Infix ((<$$>))
import Data.IntMap as IntMap
import Data.IORef
import Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Grin.Grin
import Reducer.Base
import System.IO
import System.IO.Unsafe
import Data.Bits.Floating
import Data.Int
import Data.Maybe (fromMaybe)
import Control.Monad (when, mzero)

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU


C.include "<math.h>"
C.include "<stdio.h>"

data EvalReferences = EvalReferences
  { bufferRef :: IORef (IntMap (Vector Word8))
  , handleRef :: IORef (IntMap Handle)
  }

createBuffer :: IO EvalReferences
createBuffer = do
  -- The 0 index is reserved for the NULL pointer.
  bufferRef <- newIORef $ IntMap.singleton 0 Vector.empty
  handleRef <- newIORef $ IntMap.fromList
    [ (0, stdin)
    , (1, stdout)
    , (2, stderr)
    ]
  pure $ EvalReferences bufferRef handleRef

-- primitive functions
primLiteralPrint _ _ [RT_Lit (LInt64 a)] = liftIO (putStr $ show a) >> pure RT_Unit
primLiteralPrint _ _ [RT_Lit (LString a)] = liftIO (putStr (Text.unpack a)) >> pure RT_Unit
primLiteralPrint ctx ps x = error $ Prelude.unwords ["primLiteralPrint", ctx, "- invalid arguments:", show ps, " - ", show x]

evalPrimOp :: EvalReferences -> Name -> [Val] -> [RTVal] -> IO RTVal
evalPrimOp (EvalReferences bufferRef handleRef) name params args = case name of
  "_prim_int_print"    -> primLiteralPrint "int"    params args
  "_prim_string_print" -> primLiteralPrint "string" params args
  "_prim_string_index" -> primStringIndex
  "_prim_crash"        -> primCrash
  "_prim_read_string"  -> primReadString
  "_prim_usleep"       -> primUSleep
  "_prim_error"        -> primError
  "_prim_file_open"    -> primFileOpen
  "_prim_file_close"   -> primFileClose
  "_prim_file_eof"     -> primFileEOF
  "_prim_stdin"        -> primStdIn
  "_prim_stdout"       -> primStdOut
  "_prim_stderr"       -> primStdErr
  "_prim_putChar"      -> primPutchar

  -- Buffer
  "_prim_new_buffer"        -> primNewBuffer
  "_prim_set_buffer_byte"   -> primSetBufferByte
  "_prim_set_buffer_string" -> primSetBufferString
  "_prim_set_buffer_int"    -> primSetBufferInt
  "_prim_set_buffer_double" -> primSetBufferDouble
  "_prim_get_buffer_byte"   -> primGetBufferByte
  "_prim_get_buffer_int"    -> primGetBufferInt
  "_prim_get_buffer_double" -> primGetBufferDouble
  "_prim_get_buffer_string" -> primGetBufferString
  "_prim_copy_buffer"       -> primCopyBuffer
  "_prim_write_buffer"      -> primWriteBuffer
  "_prim_read_buffer"       -> primReadBuffer
  -- Conversion
  "_prim_int_str"      -> int_str
  "_prim_str_int"      -> str_int
  "_prim_int_float"    -> int_float
  "_prim_float_int"    -> float_int
  "_prim_float_string" -> float_str
  "_prim_char_int"     -> char_int
  "_prim_int_char"     -> int_char
  "_prim_int_word"     -> int_word
  "_prim_word_int"     -> word_int
  "_prim_word_word"    -> word_word
  "_prim_int_int"      -> int_int
  -- String
  "_prim_string_reverse" -> string_un_op string Text.reverse
  "_prim_string_head"    -> string_un_op int (fromIntegral . ord . Text.head)
  "_prim_string_tail"    -> string_un_op string Text.tail
  "_prim_string_len"     -> string_un_op int (fromIntegral . Text.length)
  "_prim_string_concat"  -> string_bin_op string (\t1 t2 -> Text.concat [t1, t2])
  "_prim_string_lt"      -> string_bin_op int (boolean 0 1 <$$> (<))
  "_prim_string_eq"      -> string_bin_op int (boolean 0 1 <$$> (==))
  "_prim_string_cons"    -> string_cons
  "_prim_string_float"   -> primStringFloat

  -- Int
  "_prim_int_shr"   -> int_un_op int (`shiftR` 1)
  "_prim_int_add"   -> int_bin_op int (+)
  "_prim_int_sub"   -> int_bin_op int (-)
  "_prim_int_mul"   -> int_bin_op int (*)
  "_prim_int_div"   -> int_bin_op int div
  "_prim_int_ashr"  -> int_bin_op int (\v h -> shift v ((-1) * fromIntegral h))
  "_prim_int_rem"   -> int_bin_op int rem
  "_prim_int_eq"    -> int_bin_op bool (==)
  "_prim_int_ne"    -> int_bin_op bool (/=)
  "_prim_int_gt"    -> int_bin_op bool (>)
  "_prim_int_ge"    -> int_bin_op bool (>=)
  "_prim_int_lt"    -> int_bin_op bool (<)
  "_prim_int_le"    -> int_bin_op bool (<=)
  -- Word
  "_prim_word_add"  -> word_bin_op word (+)
  "_prim_word_sub"  -> word_bin_op word (-)
  "_prim_word_mul"  -> word_bin_op word (*)
  "_prim_word_div"  -> word_bin_op word div -- ???
  "_prim_word_udiv" -> word_bin_op word div
  "_prim_word_shl"  -> word_bin_op word (\v h -> shift v (fromIntegral h))
  "_prim_word_lshr" -> word_bin_op word (\v h -> shiftR v (fromIntegral h))
  "_prim_word_and"  -> word_bin_op word (.&.)
  "_prim_word_eq"   -> word_bin_op bool (==)
  "_prim_word_ne"   -> word_bin_op bool (/=)
  "_prim_word_gt"   -> word_bin_op bool (>)
  "_prim_word_ge"   -> word_bin_op bool (>=)
  "_prim_word_lt"   -> word_bin_op bool (<)
  "_prim_word_le"   -> word_bin_op bool (<=)
  -- Float
  "_prim_float_add" -> float_bin_op float (+)
  "_prim_float_sub" -> float_bin_op float (-)
  "_prim_float_mul" -> float_bin_op float (*)
  "_prim_float_div" -> float_bin_op float (/)
  "_prim_float_eq"  -> float_bin_op bool (==)
  "_prim_float_ne"  -> float_bin_op bool (/=)
  "_prim_float_gt"  -> float_bin_op bool (>)
  "_prim_float_ge"  -> float_bin_op bool (>=)
  "_prim_float_lt"  -> float_bin_op bool (<)
  "_prim_float_le"  -> float_bin_op bool (<=)
  "_prim_float_atan2" -> float_bin_op float atan2
  "_prim_float_floor" -> float_un_op float (fromIntegral @Integer @Float . floor)
  "_prim_float_ceil"  -> float_un_op float (fromIntegral @Integer @Float . ceiling)
  -- Bool
  "_prim_bool_eq"   -> bool_bin_op bool (==)
  "_prim_bool_ne"   -> bool_bin_op bool (/=)

  _ -> error $ "unknown primitive operation: " ++ unpackName name
 where
  int   x = pure . RT_Lit . LInt64 $ x
  word  x = pure . RT_Lit . LWord64 $ x
  float x = pure . RT_Lit . LFloat $ x
  bool  x = pure . RT_Lit . LBool $ x
  string x = pure . RT_Lit . LString $ x
  char x = pure . RT_Lit . LChar $ x

  int_un_op retTy fn = case args of
    [RT_Lit (LInt64 a)] -> retTy $ fn a
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_bin_op retTy fn = case args of
    [RT_Lit (LInt64 a), RT_Lit (LInt64 b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  word_bin_op retTy fn = case args of
    [RT_Lit (LWord64 a), RT_Lit (LWord64 b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  float_un_op retTy fn = case args of
    [RT_Lit (LFloat a)] -> retTy $ fn a
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  float_bin_op retTy fn = case args of
    [RT_Lit (LFloat a), RT_Lit (LFloat b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  bool_bin_op retTy fn = case args of
    [RT_Lit (LBool a), RT_Lit (LBool b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  string_bin_op retTy fn = case args of
    [RT_Lit (LString a), RT_Lit (LString b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  string_un_op retTy fn = case args of
    [RT_Lit (LString a)] -> retTy $ fn a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  string_cons = case args of
    [RT_Lit (LInt64 a), RT_Lit (LString b)] -> string $ Text.cons (chr (fromIntegral a)) b
    _ -> error $ "invalid arguments: " ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_str = case args of
    [RT_Lit (LInt64 a)] -> string $ fromString $ show a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  str_int = case args of
    [RT_Lit (LString a)] -> int $ read $ Text.unpack a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_float = case args of
    [RT_Lit (LInt64 a)] -> float $ fromIntegral a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  float_int = case args of
    [RT_Lit (LFloat a)] -> int $ round a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  char_int = case args of
    [RT_Lit (LChar a)] -> int . fromIntegral . ord $ a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_char = case args of
    [RT_Lit (LInt64 a)] -> char . chr $ fromIntegral a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_word = case args of
    [RT_Lit (LInt64 a)] -> word $ fromIntegral a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  word_int = case args of
    [RT_Lit (LWord64 a)] -> int $ fromIntegral a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  int_int = case args of
    [RT_Lit (LInt64 a)] -> int a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  word_word = case args of
    [RT_Lit (LWord64 a)] -> word a
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  float_str = case args of
    [RT_Lit (LFloat a)] -> liftIO $ allocaBytes 32 $ \buf -> do
        let cf = CFloat a
        [C.exp| void { snprintf($(char* buf), 32, "%.16g", $(float cf)) } |]
        string . fromString =<< peekCString buf

    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primReadString = case args of
    [] -> liftIO getLine >>= (string . fromString)
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primStringIndex = case args of
    [RT_Lit (LString str), RT_Lit (LInt64 idx)] -> int $ fromIntegral $ ord $ Text.index str $ fromIntegral idx
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primStringFloat = case args of
    [RT_Lit (LString str)] -> float $ read $ Text.unpack str
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primUSleep = case args of
    [RT_Lit (LInt64 us)] -> liftIO $ threadDelay (fromIntegral us) >> pure RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primError = case args of
    [RT_Lit (LString msg)] -> liftIO (ioError $ userError $ Text.unpack msg) >> pure RT_Unit
--    [RT_Lit (LString msg)] -> liftIO (putStr $ Text.unpack msg) >> pure RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primNewBuffer = case args of
    [RT_Lit (LInt64 bytes)] -> do
      newIdx <- IntMap.size <$> readIORef bufferRef
      modifyIORef bufferRef
        $ IntMap.insert newIdx
        $ Vector.replicate (fromIntegral bytes) 0
      pure $ RT_Lit $ LWord64 $ fromIntegral newIdx
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primSetBufferByte = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LWord64 byte)] -> do
      modifyIORef bufferRef
        $ IntMap.adjust
            (\v -> v Vector.// [(fromIntegral loc, fromIntegral byte)])
            (fromIntegral idx)
      pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primSetBufferString = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LString string)] -> do
      let newChars = [(fromIntegral loc) ..] `zip` ((fromIntegral . ord) <$> (Text.unpack string))
      modifyIORef bufferRef
        $ IntMap.adjust
            (\v -> v Vector.// newChars)
            (fromIntegral idx)
      pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primSetBufferInt = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LInt64 int)] -> do
      let (b1, b2, b3, b4) = int64toBytes int
      modifyIORef bufferRef
        $ IntMap.adjust
            (\v -> v Vector.//
              [ (fromIntegral loc + 0, b1)
              , (fromIntegral loc + 1, b2)
              , (fromIntegral loc + 2, b3)
              , (fromIntegral loc + 3, b4)
              ])
            (fromIntegral idx)
      pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primSetBufferDouble = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LFloat float)] -> do
      let (b1, b2, b3, b4) = doubleToBytes $ realToFrac float
      modifyIORef bufferRef
        $ IntMap.adjust
            (\v -> v Vector.//
              [ (fromIntegral loc + 0, b1)
              , (fromIntegral loc + 1, b2)
              , (fromIntegral loc + 2, b3)
              , (fromIntegral loc + 3, b4)
              ])
            (fromIntegral idx)
      pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name


  primGetBufferByte = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc)] -> do
      buffer <- readIORef bufferRef
      pure $ RT_Lit $ LWord64 $ fromIntegral $ fromMaybe 0 $ (buffer IntMap.! (fromIntegral idx)) Vector.!? (fromIntegral loc)
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primGetBufferInt = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc)] -> do
      buffer <- readIORef bufferRef
      pure $ RT_Lit $ LInt64 $ fromMaybe 0 $ do
        buffer0 <- IntMap.lookup (fromIntegral idx) buffer
        b1 <- buffer0 Vector.!? (fromIntegral loc + 0)
        b2 <- buffer0 Vector.!? (fromIntegral loc + 1)
        b3 <- buffer0 Vector.!? (fromIntegral loc + 2)
        b4 <- buffer0 Vector.!? (fromIntegral loc + 3)
        pure $ bytesToInt64 (b1,b2,b3,b4)
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primGetBufferDouble = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc)] -> do
      buffer <- readIORef bufferRef
      pure $ RT_Lit $ LFloat $ fromMaybe 0.0 $ do
        buffer0 <- IntMap.lookup (fromIntegral idx) buffer
        b1 <- buffer0 Vector.!? (fromIntegral loc + 0)
        b2 <- buffer0 Vector.!? (fromIntegral loc + 1)
        b3 <- buffer0 Vector.!? (fromIntegral loc + 2)
        b4 <- buffer0 Vector.!? (fromIntegral loc + 3)
        pure $ realToFrac $ bytesToDouble (b1,b2,b3,b4)
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primGetBufferString = case args of
    [RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LInt64 len)] -> do
      buffer <- readIORef bufferRef
      pure $ RT_Lit $ LString $ fromString $ fromMaybe "" $ do
        buffer0 <- IntMap.lookup (fromIntegral idx) buffer
        when (Vector.length buffer0 < fromIntegral (loc + len)) mzero
        mapM (fmap (chr . fromIntegral) . (buffer0 Vector.!?) . fromIntegral) [loc .. loc + len - 1]

  primCopyBuffer = case args of
    [RT_Lit (LWord64 idxfrom), RT_Lit (LInt64 locfrom), RT_Lit (LInt64 len), RT_Lit (LWord64 idxto), RT_Lit (LInt64 locto) ]
      -> do buffer <- readIORef bufferRef
            let bytes     = Vector.toList
                          $ Vector.slice (fromIntegral locfrom) (fromIntegral len)
                          $ buffer IntMap.! (fromIntegral idxfrom)
            let newBytes  = [(fromIntegral locto) ..] `zip` bytes
            modifyIORef bufferRef
              $ IntMap.adjust
                  (\v -> v Vector.// newBytes)
                  (fromIntegral idxto)
            pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primReadBuffer = case args of
    [RT_Lit (LWord64 filePtr), RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LInt64 max0)] -> do
      handle <- fmap (\m -> m IntMap.! (fromIntegral filePtr)) $ readIORef handleRef
      buffer <- readIORef bufferRef
      let noOfBytesToRead = Vector.length
                          $ Vector.drop (fromIntegral loc)
                          $ buffer IntMap.! (fromIntegral idx)
      let readFile res _ 0 = pure res
          readFile res i n = do
            eof <- hIsEOF handle
            case eof of
              True  -> pure res
              False -> do
                c <- hGetChar handle
                readFile ((i, fromIntegral $ ord c):res) (i + 1) (n - 1)
      newBytes <- readFile [] (fromIntegral loc) (min noOfBytesToRead (fromIntegral max0))
      modifyIORef bufferRef
        $ IntMap.adjust
            (\v -> v Vector.// newBytes)
            (fromIntegral idx)
      pure $ RT_Lit $ LInt64 $ fromIntegral $ length newBytes
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primWriteBuffer = case args of
    [RT_Lit (LWord64 filePtr), RT_Lit (LWord64 idx), RT_Lit (LInt64 loc), RT_Lit (LInt64 len)] -> do
      buffer <- readIORef bufferRef
      handle <- fmap (\m -> m IntMap.! (fromIntegral filePtr)) $ readIORef handleRef
      let bytes = Vector.toList
                $ Vector.take (fromIntegral len)
                $ Vector.drop (fromIntegral loc)
                $ buffer IntMap.! (fromIntegral idx)
      mapM (hPutChar handle . chr . fromIntegral) bytes
      pure RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primStdIn = case args of
    [] -> pure $ RT_Lit $ LWord64 0
    _  -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primStdOut = case args of
    [] -> pure $ RT_Lit $ LWord64 1
    _  -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primStdErr = case args of
    [] -> pure $ RT_Lit $ LWord64 2
    _  -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primPutchar = case args of
    [RT_Lit (LChar c)] -> do
      putChar c
      pure RT_Unit
    _  -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primFileOpen = case args of
    [RT_Lit (LString fileName), RT_Lit (LString mode0)] -> do
      handles <- readIORef handleRef
      let newPtr = IntMap.size handles
      let mode = case Text.unpack mode0 of
                  "r"   -> ReadMode
                  "r+"  -> ReadWriteMode
                  "w+"  -> ReadWriteMode
                  "w"   -> WriteMode
                  "a"   -> AppendMode
                  "rb"   -> ReadMode
                  "rb+"  -> ReadWriteMode
                  "wb+"  -> ReadWriteMode
                  "wb"   -> WriteMode
                  "ab"   -> AppendMode
                  rest  -> error $ "primFileOpen: " ++ rest
      handle <- openFile (Text.unpack fileName) mode
      modifyIORef handleRef
        $ IntMap.insert newPtr handle
      pure $ RT_Lit $ LWord64 $ fromIntegral newPtr
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primFileEOF = case args of
    [RT_Lit (LWord64 filePtr)] -> do
      handle <- fmap (\m -> m IntMap.! (fromIntegral filePtr)) $ readIORef handleRef
      eof <- hIsEOF handle
      pure $ RT_Lit $ LInt64 $ case eof of { True -> 1; False -> 0 }
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primFileClose = case args of
    [RT_Lit (LWord64 filePtr)] -> do
      handle <- fmap (\m -> m IntMap.! (fromIntegral filePtr)) $ readIORef handleRef
      hClose handle
      pure $ RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  primCrash = case args of
    [RT_Lit (LString msg)] -> liftIO (ioError $ userError $ Text.unpack msg) >> pure RT_Unit
    _ -> error $ "invalid arguments:" ++ show params ++ " " ++ show args ++ " for " ++ unpackName name

  boolean f t x = if x then t else f

word64toBytes :: Word64 -> (Word8, Word8, Word8, Word8)
word64toBytes w =
  ( fromIntegral $ w .&. 0x000000FF
  , fromIntegral $ shift (w .&. 0x0000FF00) (-8)
  , fromIntegral $ shift (w .&. 0x00FF0000) (-16)
  , fromIntegral $ shift (w .&. 0xFF000000) (-24)
  )

int64toBytes :: Int64 -> (Word8, Word8, Word8, Word8)
int64toBytes w =
  ( fromIntegral $ w .&. 0x000000FF
  , fromIntegral $ shift (w .&. 0x0000FF00) (-8)
  , fromIntegral $ shift (w .&. 0x00FF0000) (-16)
  , fromIntegral $ shift (w .&. 0xFF000000) (-24)
  )

bytesToInt64 :: (Word8, Word8, Word8, Word8) -> Int64
bytesToInt64 (b1, b2, b3, b4)
  = (fromIntegral b1)
    + shift (fromIntegral b2) 8
    + shift (fromIntegral b3) 16
    + shift (fromIntegral b4) 24

doubleToBytes :: Double -> (Word8, Word8, Word8, Word8)
doubleToBytes = word64toBytes . coerceToWord

bytesToDouble :: (Word8, Word8, Word8, Word8) -> Double
bytesToDouble (b1, b2, b3, b4)
  = coerceToFloat $ zeroBits
    .|. (fromIntegral b1)
    .|. shift (fromIntegral b2) 8
    .|. shift (fromIntegral b3) 16
    .|. shift (fromIntegral b4) 24
