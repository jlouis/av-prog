module State (State (..)) where

import Data.Word


class State s where
    {- init () generate new S -}
    empty     :: [Word32] -> s

    {- index S arr off, return val, the value stored in Array arr, Offset off -}
    lookupE  :: s -> Word32 -> Word32 -> Maybe Word32

    {- update S arr off val -> return S, st. index S arr off == val -}
    updateE  :: s -> Word32 -> Word32 -> Word32-> Maybe s

    {- allocate S cap, return id != 0 to Array with Capacity cap AND S where id is taken (new array contains zero) -}
    allocate :: s -> Word32 -> Maybe (s, Word32)

    {- swap S arr0 arr1, return S, st. arr0 and arr1 has been swapped -}
    swap     :: s -> Word32 -> Word32 -> Maybe s

    {- copy S arr idx, return S, st. arr has been copied to idx -}
    copy     :: s -> Word32 -> Word32 -> Maybe s

    {- return S where arr has been freed -}
    free     :: s -> Word32 -> Maybe s

    {- load S arr, return S such that Array 0 is swapped with Array arr -}
    load     :: s -> Word32 -> Maybe s

