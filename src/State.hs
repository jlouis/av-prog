module State (State (..)) where

class State s where
    {- init () generate new S -}
    empty     :: s

    {- index S arr off, return val, the value stored in Array arr, Offset off -}
    index    :: s -> Int -> Int -> Maybe Int

    {- update S arr off val -> return S, st. index S arr off == val -}
    update   :: s -> Int -> Int -> Int-> Maybe s

    {- allocate S cap, return id != 0 to Array with Capacity cap AND S where id is taken (new array contains zero) -}
    allocate :: s -> Int -> Maybe (s, Int)

    {- swap S arr0 arr1, return S, st. arr0 and arr1 has been swapped -}
    swap     :: s -> Int -> Int -> Maybe s

    {- copy S arr idx, return S, st. arr has been copied to idx -}
    copy     :: s -> Int -> Int -> Maybe s

    {- return S where arr has been freed -}
    free     :: s -> Int -> Maybe s

    {- load S arr, return S such that Array 0 is swapped with Array arr -}
    load     :: s -> Int -> Maybe s

