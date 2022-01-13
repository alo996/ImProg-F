module MF where
    import Declarations
    import Tokenizer
    import Parser
    import Compiler
    import Store
    import MiniMF

    value :: Store HeapCell -> Int -> Either String HeapCell
    value heap adr1 = case access heap adr1 of
        Right (IND adr2) -> value heap adr2
        Right hpc        -> Right hpc
        Left error       -> Left error

    unwind :: State -> State
    unwind s = case value (heap s) (sp s) of
        Right (APP adr1 adr2) -> case save (stack s) (StackCell adr1) (sp s + 1) of
            Right nstack -> s {sp = sp s + 1, stack = nstack}
            Left error  -> ErrorState error   
        Right hpc             -> s {pc = pc s + 1}
        Left error            -> ErrorState error

    call :: State -> State
    call s = case value (heap s) (sp s) of
        Right (DEF f n adr) -> case save (stack s) (StackCell (pc s)) (sp s + 1) of
            Right nstack -> s {pc = adr, sp = sp s + 1, stack = nstack}
            Left error   -> ErrorState error
        Right (PRE op 2)    -> case save (stack s) (StackCell (pc s + 1)) (sp s + 1) of 
            Right nstack -> s {pc = 4, sp = sp s + 1, stack = nstack}
            Left error   -> ErrorState error
        Right (PRE If 3) -> case save (stack s) (StackCell (pc s + 1)) (sp s + 1) of
            Right nstack -> s{pc = 13, sp = sp s + 1, stack = nstack}
            Left error   -> ErrorState error
        Right (PRE op 1) -> case save (stack s) (StackCell (pc s + 1)) (sp s + 1) of
            Right nstack -> s {pc = 21, sp = sp s + 1, stack = nstack}
            Left error   -> ErrorState error           
        Left error          -> ErrorState error
    
    return'' :: State -> State
    return'' s = case access (stack s) (sp s - 1) of
        Right (StackCell adr) -> case access (stack s) (sp s) of
            Right (StackCell adr1) -> case save (stack s) (StackCell adr1) (sp s - 1) of
                Right nstack -> s {pc = adr, sp = sp s - 1, stack = nstack}
                Left error   -> ErrorState error
            Left error             -> ErrorState error
        Left error            -> ErrorState error

    pushpre :: State -> Keyword -> State
    pushpre s kw = case arity' kw of
        Just n  -> case newPRE (heap s) kw n of
            Right (adr, nheap) -> case save (stack s) (StackCell adr) (sp s + 1) of
                Right nstack -> s{pc = pc s + 1, sp = sp s + 1, stack = nstack}
                Left error -> ErrorState error
            Left error         -> ErrorState error
        Nothing -> ErrorState "Stelligkeit von Keyword konnte nicht ermittelt werden!"

    arity' :: Keyword -> Maybe Int
    arity' kweywrdds = case kweywrdds of
        And    -> Just 2
        Or     -> Just 2
        Equals -> Just 2
        Less   -> Just 2
        Plus   -> Just 2
        Minus  -> Just 1
        Times  -> Just 2
        Divide -> Just 2
        Not    -> Just 2
        _      -> Nothing
        
    newPRE :: Store HeapCell -> Keyword -> Int -> Either String (Int, Store HeapCell)
    newPRE heap kw n = case arity' kw of
        Just n  -> return (depth heap, push heap (PRE kw n))
        Nothing -> Left "Compile error: 'arity' called with illegal keyword."        