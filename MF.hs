module MF where
    import Declarations
    import Tokenizer
    import Parser
    import Compiler
    import Store
    import MiniMF
    import Data.Bits
    
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
        _                   -> ErrorState "error in 'call'"
    
    return'' :: State -> State
    return'' s = case access (stack s) (sp s - 1) of
        Right (StackCell adr) -> case access (stack s) (sp s) of
            Right (StackCell adr1) -> case save (stack s) (StackCell adr1) (sp s - 1) of
                Right nstack -> s {pc = adr, sp = sp s - 1, stack = nstack}
                Left error   -> ErrorState error
            Left error             -> ErrorState error
        Left error            -> ErrorState error

    pushpre :: State -> Keyword -> State
    pushpre s kw = let tuple = newPRE (heap s) kw (arity' kw) in
        case save (stack s) (StackCell (fst tuple)) (sp s + 1) of
                Right stack -> s{pc = pc s + 1, sp = sp s + 1, stack = stack, heap = snd tuple}
                Left error   -> ErrorState error

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
                                    Left error           -> ErrorState error
                                    _                    -> ErrorState "Error in 'opUpdate'"
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
                                    Not -> if fromEnum w == 1
                                                then let tuple = newVAL (heap s) 1 1 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    Left error           -> ErrorState error
                                                else let tuple = newVAL (heap s) 1 0 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    Left error           -> ErrorState error
                                    _   -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                Right (VALNum w)   -> case op of
                                    Minus -> let tuple = newVAL (heap s) 0 (- w) in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                        _     -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                    _         -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                Left error -> ErrorState error
                            Left error  -> ErrorState error
                        Left error  -> ErrorState error
                Left error               -> ErrorState error
        2 -> case access (stack s) (sp s - 3) of
            Right (StackCell addr) -> case value (heap s) addr of
                Right (PRE op 2) -> case access (stack s) (sp s - 2) of
                    Right scell -> case save (stack s) scell (sp s - 4) of
                        Right stack -> case access stack (sp s - 1) of
                            Right (StackCell addr2) -> let hcell1 = value (heap s) addr2 in
                                case access stack (sp s) of
                                    Right (StackCell addr3) -> let hcell2 = value (heap s) addr3 in
                                        case hcell1 of
                                            Right (VALBool bool1) -> case hcell2 of
                                                Right (VALBool bool2) -> case op of
                                                    And    -> let tuple = newVAL (heap s) 1 (bool1 .&. bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error 
                                                    Or     -> let tuple = newVAL (heap s) 1 (bool1 .|. bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Equals -> let tuple = newVAL (heap s) 1 (0 .&. xor bool1 bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Less   -> let tuple = newVAL (heap s) 1 (fromEnum $ bool1 < bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    _      -> ErrorState "Type error."
                                                Left error            -> ErrorState error
                                            Right (VALNum num1) -> case hcell2 of
                                                Right (VALNum num2) -> case op of
                                                    Equals -> let tuple = newVAL (heap s) 1 (fromEnum $ num1 == num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Less   -> let tuple = newVAL (heap s) 1 (fromEnum $ num1 < num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Plus   -> let tuple = newVAL (heap s) 0 (num1 + num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Times  -> let tuple = newVAL (heap s) 0 (num1 * num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Divide -> let tuple = newVAL (heap s) 0 (num1 `div` num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right stack -> s {sp = sp s - 3, stack = stack, heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    _      -> ErrorState "Type error."
                                                _                   -> ErrorState "Type error."
                                            Left error          -> ErrorState error
                                    Left error          -> ErrorState error
                            Left error          -> ErrorState error
                    Left error          -> ErrorState error
            Left error          -> ErrorState error
        3 -> case access (stack s) (sp s) of
            Right (StackCell addr) -> case value (heap s) addr of
                Right (VALBool bool) -> case bool of
                    1  -> case access (stack s) (sp s - 4) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case save (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case access stack (sp s - 1) of
                                    Right scell -> case save stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {sp = sp s - 3, stack = Stack $(take $ sp s - 3) scells}
                                        Left error           -> ErrorState error
                                    Left error  -> ErrorState error
                                Left error  -> ErrorState error
                            Left error  -> ErrorState error
                        Left error              -> ErrorState error
                    0 -> case access (stack s) (sp s - 5) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case save (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case access stack (sp s - 1) of
                                    Right scell -> case save stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {sp = sp s - 3, stack = Stack $(take $ sp s - 3) scells}
                                        Left error           -> ErrorState error
                                    Left error  -> ErrorState error
                                Left error  -> ErrorState error
                            Left error  -> ErrorState error
                        Left error              -> ErrorState error
                    _ -> ErrorState "Type error"
                Left error           -> ErrorState error
            Left error              -> ErrorState error
        _ -> ErrorState "type error"