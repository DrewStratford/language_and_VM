  
> module Store
>        (Store
>        , code
>        , pc
>        , emptyStore
>        , newFrame
>        , popFrame
>        , get
>        , set
>        , jump
>        , jumpLabel
>        , jumpReturn
>        , incPc
>        ) where
>
> import Data.List(lookup)
> import qualified Data.List.NonEmpty as N

A version of the Store with the with stacks of the variable scopes and the program counter of the
previous program counters. This allows us to implement scoping for procedure calls. We use a non
empty list for the stacks as they will always have at least one value.

> type StackFrame a b = [(a,b)]

> data Store a b c = Store
>  { stack  :: N.NonEmpty (StackFrame a b)
>  , pc     :: N.NonEmpty Int
>  , code   :: [c]
>  } deriving (Show)

> emptyStore pc code = Store ([] N.:| []) (pc N.:| []) code
  

> newFrame store = store{stack = [] N.<| stack store}
  
> popFrame store = store{stack = newStack}
>   where newStack = case stack store of
>           (_ N.:| [])          -> stack store
>           (_ N.:| (f :frames)) -> f N.:| frames

> setFrame :: Eq a => a -> b -> StackFrame a b -> StackFrame a b
> setFrame v x [] = [(v,x)]
> setFrame v x (c@(u,_):s)
>   | v == u = (v,x):s
>   | otherwise = c : setFrame v x s

> set v x store =
>   let (frame N.:| frames) = stack store
>       newFrame            = setFrame v x frame
>   in store{ stack = newFrame N.:| frames}


> get :: Eq v => v -> Store v a c -> Either String a
> get v store =
>   let loc = v `lookup` N.head (stack store)
>   in case loc of
>         (Just c) -> Right c
>         _        -> Left "Couldn't find variable"


Returns the code N commands away, calculated relative to the program counter

> jump :: Int -> Store v a c -> (Store v a c, [c])
> jump n store =
>   let (topPc N.:| pcs) = pc store
>       newPc = topPc + n
>       newCode = drop newPc $ code store
>   in (store {pc = newPc N.:| pcs }, newCode)

Jumps until we find a stmt equal to the label. Also pushes a new stack frame.

> jumpLabel :: Eq c => c -> Store v a c -> (Store v a c, [c])
> jumpLabel label store =
>   let newPc = length (code store) - length newCode
>       newCode = dropWhile (/= label) $ code store
>   in (newFrame store{pc = newPc N.<| pc store}, newCode)

returns to the last position jumped from

> jumpReturn store = case pc store of

When calling return in the main section we finish the program by returning no new code.

>   (_ N.:| []) -> (store, [])

Other wise just jump to the old program counter and reset the pc stack.

>   (_ N.:| (old:pcs)) ->
>      let newCode = drop (old + 1) $ code store
>          newPc   = (old + 1) N.:| pcs
>      in (popFrame store{ pc = newPc}, newCode)

Increments the program counter by one as would occur for normal intstructions.

> incPc store = store {pc = topPc + 1 N.:| pcs}
>   where (topPc N.:| pcs) = pc store
