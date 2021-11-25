module ParserGeneratorLibrary where
    
    import Declarations
    
    newtype Parser token a = P ([token] -> (Either String a, [token]))
    parse :: Parser token a -> [token] -> (Either String a, [token])
    parse (P function) = function