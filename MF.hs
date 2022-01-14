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
        Right hcell      -> Right hcell
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
        Right (PRE If 3)    -> case save (stack s) (StackCell (pc s + 1)) (sp s + 1) of
            Right nstack -> s{pc = 13, sp = sp s + 1, stack = nstack}
            Left error   -> ErrorState error
        Right (PRE op 1)    -> case save (stack s) (StackCell (pc s + 1)) (sp s + 1) of
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
                Left error   -> ErrorState error
            Left error         -> ErrorState error
        Nothing -> ErrorState "Stelligkeit von Keyword konnte nicht ermittelt werden!"

    funcUpdate :: State -> Int -> State
    funcUpdate s arg = case access (stack s) (sp s) of -- wir schauen ob stack[T]=addr1 existiert
        Right (StackCell addr1) -> case access (stack s) (sp s - arg - 2) of -- wir schauen ob stack[t - n - 2]=addr2 existiert
            Right (StackCell addr2) -> let hcell = IND addr1 in case save (heap s) hcell addr2 of -- wir schauen, ob wir (IND addr1) an heap[addr2] speichern können
                Right heap -> s {pc = pc s, heap = heap}
                Left error -> ErrorState error 
            Left error              -> ErrorState error
        Left error              -> ErrorState error 

    opUpdate :: State -> State
    opUpdate s = case access (stack s) (sp s) of 
        Right (StackCell addr1) -> case access (heap s) addr1 of
            Right hcell -> case access (stack s) (sp s - 2) of
                Right (StackCell addr2) -> case save (heap s) hcell addr2 of
                    Right heap -> case access (stack s) (sp s - 2) of
                        Right aux -> case access (stack s) (sp s - 1) of
                            Right scell -> case save (stack s) scell (sp s - 2) of
                                Right nstack -> case save nstack aux (sp s - 1) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                                    Left error   -> ErrorState error
                                Left error             -> ErrorState error
                            Left error            -> ErrorState error 
                        Left error            -> ErrorState error
                    Left error            -> ErrorState error
                Left error            -> ErrorState error
            Left error            -> ErrorState error
        Left error            -> ErrorState error

    operator :: State -> Int -> State
    operator s op = case op of
        1 -> case access (stack s) (sp s - 2) of
            Right (StackCell addr1) -> case value (heap s) addr1 of
                Right (PRE op 1) -> case access (stack s) (sp s - 1) of
                    Right scell -> case save (stack s) scell (sp s - 2) of
                        Right stack -> case access stack (sp s) of
                            Right (StackCell addr2) -> case value (heap s) addr2 of
                                Right (VALBool w) -> case op of
                                    Not -> if not w
                                                then let tuple = newVAL (heap s) 1 1 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    Left error           -> ErrorState error
                                                else let tuple = newVAL (heap s) 1 0 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    Left error           -> ErrorState error
                                    _   -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                Right (VALNum w)   -> case op of
                                    Minus -> let tuple = newVAL (heap s) 0 (-w) in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                        _     -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                    _         -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                Left error -> ErrorState error
                            Left error  -> ErrorState error
                        Left error  -> ErrorState error
                Left error               -> ErrorState error
        _ -> ErrorState "error zum testen"