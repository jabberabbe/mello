{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Copyright: (c) 2021 Tito Sacchi
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tito Sacchi <tito.sakki@gmail.com>

Memory read/write operations packaged in the `MemoryMelloable` typeclass.
-}

module Mello.Memory
       ( -- * The memory-addressable handle typeclass
         -- $memoryAddressable
         MemoryMelloable(..)
         -- * Convenience re-exports
       , module Data.Word
       , module Data.Bits
       ) where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word


-- $memoryAddressable
--
-- The building blocks of the memory inspection abstraction are byte-level
-- read/write operations, 'peek' and 'poke'. Operations on commonly used word
-- sizes (16, 32, 64 bits) are provided for convenience and performance, but
-- the default implementation assumes a little-endian representation.
--
-- Example (following a linked list until @nullptr@ is reached):
--
--
-- > {-# LANGUAGE RecursiveDo #-}
-- > getLinkedList :: MemoryMelloable h m => h -> Word -> m [Word]
-- > getLinkedList h 0 = pure []
-- > getLinkedList h addr = mdo
-- >   x <- fromIntegral <$> peekW64 h addr
-- >   tail <- getLinkedList h x
-- >   pure $ addr:tail
--

-- | A typeclass for memory access operations.  The monad parameter allows pure
-- backends (e.g. 'BS.ByteString's) as well as effectful ones (e.g. in 'IO').
--
-- Convenience functions are provided for common word types. Backends that can
-- peek/poke more than one byte at a time should give custom implementations.
class Monad m => MemoryMelloable h m where
  -- | Read a single octet at the given memory address.
  peek :: h -> Word -> m Word8
  -- | Write a single octet at the given memory address.
  poke :: h -> Word -> Word8 -> m ()

  -- | Peek a word.
  -- Default implementation works for little-endian memory representations.
  peekW16 :: h -> Word -> m Word16
  peekW16 h addr = do
    x1 <- fromIntegral <$> peek h addr
    x2 <- fromIntegral <$> peek h (addr + 1)
    pure $ x1 .|. (x2 `shiftL` 8)

  -- | Peek a double word.
  -- Default implementation works for little-endian memory representations.
  peekW32 :: h -> Word -> m Word32
  peekW32 h addr = do
    x1 <- fromIntegral <$> peekW16 h addr
    x2 <- fromIntegral <$> peekW16 h (addr + 2)
    pure $ x1 .|. (x2 `shiftL` 16)

  -- | Peek a quad word.
  -- Default implementation works for little-endian memory representations.
  peekW64 :: h -> Word -> m Word64
  peekW64 h addr = do
    x1 <- fromIntegral <$> peekW32 h addr
    x2 <- fromIntegral <$> peekW32 h (addr + 4)
    pure $ x1 .|. (x2 `shiftL` 32)

  -- | Read an arbitrary number of bytes and return them as a 'BS.ByteString'.
  -- Default implementation peeks one byte at a time.
  peekN :: h              -- ^ Handle
        -> Word           -- ^ Address
        -> Word           -- ^ String size
        -> m BS.ByteString
  peekN h addr size = BS.pack <$> mapM (peek h) [addr .. (addr + size - 1)]

  -- | Poke a word.
  -- Default implementation works for little-endian memory representations.
  pokeW16 :: h -> Word -> Word16 -> m ()
  pokeW16 h addr x = do
    poke    h addr       $ fromIntegral x
    poke    h (addr + 1) $ fromIntegral (x `shiftR` 8)

  -- | Poke a double word.
  -- Default implementation works for little-endian memory representations.
  pokeW32 :: h -> Word -> Word32 -> m ()
  pokeW32 h addr x = do
    pokeW16 h addr       $ fromIntegral x
    pokeW16 h (addr + 2) $ fromIntegral (x `shiftR` 16)

  -- | Poke a quad word.
  -- Default implementation works for little-endian memory representations.
  pokeW64 :: h -> Word -> Word64 -> m ()
  pokeW64 h addr x = do
    pokeW32 h addr       $ fromIntegral x
    pokeW32 h (addr + 4) $ fromIntegral (x `shiftR` 32)

  -- | Write a buffer to the specified memory location.
  -- Default implementation pokes one byte at a time.
  pokeN :: h              -- ^ Handle
        -> Word           -- ^ Address
        -> BS.ByteString  -- ^ Buffer
        -> m ()
  pokeN h addr bs =
    mapM_ (\(addr', val) -> poke h addr' val) $ zip [addr..] (BS.unpack bs)

  {-# MINIMAL peek, poke #-}

-- | 'poke' methods are not implemented and will trigger a runtime error.
-- 'fail' is used whenever the 'BS.ByteString' is too short to perform the
-- requested read operations.
instance MonadFail m => MemoryMelloable BS.ByteString m where
  peek bs idx = maybe (fail "Out of range") return $
    bs `BS.indexMaybe` (fromIntegral idx)

  poke _ _ _ = error
    "Poke operations not implemented on referentially-transparent ByteStrings!"

