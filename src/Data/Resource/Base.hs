{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Resource.Base
    (
    -- * Files
      file
    , binaryFile
    -- * MVars
    , mvar
    -- * Foreign
    -- ** Using @ForeignPtr@s
    , foreignPtr
    -- ** Allocating memory
    , bytes
    , bytesAligned
    , ptr
    , array
    , array0
    -- ** Marshalling @Storable@ values
    , marshal
    , marshalArray
    , marshalArray0
    , marshalArrayLen
    , marshalArrayLen0
    -- ** Marshalling @String@s
    , cstring
    , castring
    , cwstring
    , cstringLen
    , castringLen
    , cwstringLen
    -- ** Memory @Pool@s
    , pool
    )
where

-- base ----------------------------------------------------------------------
import           Control.Concurrent (MVar, putMVar, takeMVar)
import           Data.Char (ord)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (mempty)
#endif
import           Foreign.C.String
                     ( CString
                     , CWString
                     , CStringLen
                     , CWStringLen
                     , castCharToCChar
                     , newCString
                     , newCStringLen
                     )
import           Foreign.C.Types (CWchar)
import           Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
#if MIN_VERSION_base(4, 4, 0)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import           Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif
import           Foreign.Marshal.Alloc (free)
import           Foreign.Marshal.Array (pokeArray, pokeArray0)
import           Foreign.Marshal.Pool (Pool, newPool, freePool)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable
                     ( Storable
                     , alignment
                     , sizeOf
                     , poke
                     , pokeElemOff
                     )
import           GHC.Exts
                     ( Int (I#)
                     , byteArrayContents#
                     , newAlignedPinnedByteArray#
                     , newPinnedByteArray#
                     , unsafeFreezeByteArray#
                     , touch#
                     )
import           GHC.IO (IO (IO))
import           GHC.Ptr (Ptr (Ptr))
import           System.IO (Handle, IOMode, hClose, openBinaryFile, openFile)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (lift)


-- resource ------------------------------------------------------------------
import           Data.Resource
                     ( Resource
                     , resource
                     , resource'
                     , onSuccess
                     )


------------------------------------------------------------------------------
file :: FilePath -> IOMode -> Resource IO Handle
file path mode = resource (openFile path mode) hClose


------------------------------------------------------------------------------
binaryFile :: FilePath -> IOMode -> Resource IO Handle
binaryFile path mode = resource (openBinaryFile path mode) hClose


------------------------------------------------------------------------------
mvar :: MVar a -> Resource IO a
mvar mv = resource (takeMVar mv) (putMVar mv)


------------------------------------------------------------------------------
foreignPtr :: ForeignPtr a -> Resource IO (Ptr a)
foreignPtr p = resource' (return (unsafeForeignPtrToPtr p))
    (const $ mempty { onSuccess = touchForeignPtr p })


------------------------------------------------------------------------------
bytes :: Int -> Resource IO (Ptr a)
bytes (I# size) = resource'
    (IO $ \s0 -> case newPinnedByteArray# size s0 of
        (# s1, mbarr# #) -> case unsafeFreezeByteArray# mbarr# s1 of
            (# s2, barr# #) -> (# s2, Ptr (byteArrayContents# barr#) #))
    (\(Ptr barr#) -> mempty { onSuccess =
        IO $ \s0 -> case touch# barr# s0 of s1 -> (# s1, () #) })


------------------------------------------------------------------------------
bytesAligned :: Int -> Int -> Resource IO (Ptr a)
bytesAligned (I# size) (I# align) = resource'
    (IO $ \s0 -> case newAlignedPinnedByteArray# size align s0 of
        (# s1, mbarr# #) -> case unsafeFreezeByteArray# mbarr# s1 of
            (# s2, barr# #) -> (# s2, Ptr (byteArrayContents# barr#) #))
    (\(Ptr barr#) -> mempty { onSuccess =
        IO $ \s0 -> case touch# barr# s0 of s1 -> (# s1, () #) })


------------------------------------------------------------------------------
ptr :: forall a. Storable a => Resource IO (Ptr a)
ptr = bytesAligned (sizeOf proxy) (alignment proxy)
  where
    proxy :: a
    proxy = undefined
{-# INLINE ptr #-}


------------------------------------------------------------------------------
array :: forall a. Storable a => Int -> Resource IO (Ptr a)
array size = bytesAligned (sizeOf proxy * size) (alignment proxy)
  where
    proxy :: a
    proxy = undefined
{-# INLINE array #-}


------------------------------------------------------------------------------
array0 :: Storable a => Int -> Resource IO (Ptr a)
array0 = array . (+1)
{-# INLINE array0 #-}


------------------------------------------------------------------------------
marshal :: Storable a => a -> Resource IO (Ptr a)
marshal a = ptr >>= \p -> lift (poke p a) >> return p


------------------------------------------------------------------------------
marshalArray :: Storable a => [a] -> Resource IO (Ptr a)
marshalArray as = fmap fst (marshalArrayLen as)


------------------------------------------------------------------------------
marshalArray0 :: Storable a => a -> [a] -> Resource IO (Ptr a)
marshalArray0 a as = fmap fst (marshalArrayLen0 a as)


------------------------------------------------------------------------------
marshalArrayLen :: Storable a => [a] -> Resource IO (Ptr a, Int)
marshalArrayLen as = do
    p <- array size
    lift $ pokeArray p as
    return (p, size)
  where
    size = length as


------------------------------------------------------------------------------
marshalArrayLen0 :: Storable a => a -> [a] -> Resource IO (Ptr a, Int)
marshalArrayLen0 a as = do
    p <- array0 size
    lift $ pokeArray0 a p as
    return (p, size)
  where
    size = length as


------------------------------------------------------------------------------
cstring :: String -> Resource IO CString
cstring s = resource (newCString s) free


------------------------------------------------------------------------------
cstringLen :: String -> Resource IO CStringLen
cstringLen s = resource (newCStringLen s) (free . fst)


------------------------------------------------------------------------------
castring :: String -> Resource IO CString
castring s = do
    p <- array (length s)
    let go [] n = pokeElemOff p n 0
        go (c:cs) !n = pokeElemOff p n (castCharToCChar c) >> go cs (n + 1)
    lift $ go s 0
    return p


------------------------------------------------------------------------------
castringLen :: String -> Resource IO CStringLen
castringLen s = do
    p <- array size
    let go [] _ = return ()
        go (c:cs) !n = pokeElemOff p n (castCharToCChar c) >> go cs (n + 1)
    lift $ go s 0
    return (p, size)
  where
    size = length s


------------------------------------------------------------------------------
cwstring :: String -> Resource IO CWString
cwstring = marshalArray0 0 . charsToCWchars


------------------------------------------------------------------------------
cwstringLen :: String -> Resource IO CWStringLen
cwstringLen = marshalArrayLen . charsToCWchars


------------------------------------------------------------------------------
charsToCWchars :: [Char] -> [CWchar]
#ifdef mingw32_HOST_OS
charsToCWchars = foldr utf16Char [] . map ord
  where
    utf16Char c wcs
        | c < 0x10000 = fromIntegral c : wcs
        | otherwise = let c' = c - 0x10000 in
            fromIntegral (c' `div` 0x400 + 0xd800) :
            fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs
#else
charsToCWchars = map $ fromIntegral . ord
#endif


------------------------------------------------------------------------------
pool :: Resource IO Pool
pool = resource newPool freePool
