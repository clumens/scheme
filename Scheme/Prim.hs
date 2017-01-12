{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Prim(primEnv,
                   unop)
 where

import Scheme.Exceptions
import Scheme.LispVal(Eval(..), IFunc(IFunc), LispVal(..), showVal)

import           Control.Monad(foldM)
import           Control.Monad.IO.Class(liftIO)
import           Data.Char(chr, ord)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory(doesFileExist)
import           System.IO(Handle, IOMode(..), withFile)

type Prim   = [(T.Text, LispVal)]
type Unary  = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

--
-- THE PRIMITIVE ENVIRONMENT
--

-- This environment contains all the scheme functions that must be implemented in Haskell.
-- They are the most basic operations supported and are what everything else is built upon.
-- As much as possible, the standard library should be implemented in scheme itself.  This
-- environment should be the absolute minumum.
primEnv :: Prim
primEnv = [ -- Basic math.
            -- NEEDS TESTS
            ("+",       mkF $ binopFold (numOp (+)) (Number 0)),
            -- NEEDS TESTS
            ("-",       mkF $ binop     (numOp (-))),
            -- NEEDS TESTS
            ("*",       mkF $ binopFold (numOp (*)) (Number 1)),
            -- NEEDS TESTS (floats)
            ("<",       mkF $ numCmp    (<)),
            -- NEEDS TESTS (floats)
            ("<=",      mkF $ numCmp    (<=)),
            -- NEEDS TESTS (floats)
            (">",       mkF $ numCmp    (>)),
            -- NEEDS TESTS (floats)
            (">=",      mkF $ numCmp    (>=)),
            -- NEEDS TESTS (floats)
            ("=",       mkF $ numCmp    (==)),
            ("div",     mkF $ binop     (intOp div)),
            ("mod",     mkF $ binop     (intOp mod)),

            -- Booleans.
            -- NEEDS TESTS
            ("boolean=?",   mkF $ binop eqBoolean),

            -- Equivalence.
            -- NEEDS TESTS
            ("eqv?",    mkF $ binop     equivalent),

            -- Type predicates.
            ("boolean?",    mkF $ unop isBoolean),
            ("char?",       mkF $ unop isCharacter),
            -- NEEDS TESTS
            ("condition?",  mkF $ unop isCondition),
            ("integer?",    mkF $ unop isInteger),
            ("list?",       mkF $ unop isList),
            ("number?",     mkF $ unop isNumber),
            ("real?",       mkF $ unop isReal),
            ("procedure?",  mkF $ unop isProcedure),
            ("string?",     mkF $ unop isString),

            -- Characters.
            -- NEEDS TESTS
            ("char->integer", mkF $ unop charToInteger),
            -- NEEDS TESTS
            ("integer->char", mkF $ unop integerToChar),

            -- Strings.
            -- NEEDS TESTS
            ("list->string",    mkF $ unop $ return . listToString),
            ("string->list",    mkF $ unop $ return . stringToList),

            -- Lists.
            ("append",  mkF $ binopFold append (List [])),
            ("cons",    mkF Scheme.Prim.cons),
            ("car",     mkF Scheme.Prim.car),
            ("cdr",     mkF Scheme.Prim.cdr),

            -- Quotation.
            ("quote",   mkF quote),

            -- Errors.
            ("raise",   mkF $ unop raise),

            -- Code.
            -- NEEDS TESTS
            ("print-ast",   mkF $ return . String . T.pack . show),

            -- IO.
            -- NEEDS TESTS
            ("write",           mkF $ unop $ return . String . showVal),
            -- NEEDS TESTS
            ("file-exists?",    mkF $ unop fileExists),
            -- FIXME: Replace these with real versions.
            ("slurp",           mkF $ unop slurp) ]

--
-- HELPERS
--

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF fn = Func (IFunc fn) Nothing

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x]     = op x
unop _ args     = return $ Raised "syntax-error" (numArgsMessage 1 args)

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = return $ Raised "syntax-error" (numArgsMessage 1 args)

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op _ [a, b]         = op a b
binopFold op farg args@(_:_)  = foldM op farg args
binopFold _ _ args@[]         = return $ Raised "syntax-error" (numArgsMessage 2 args)

--
-- NUMBERS
--

intOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
intOp _  err@(Raised _ _) _    = return err
intOp _  _ err@(Raised _ _)    = return err
intOp _  _          (Number 0) = return $ Raised "div-by-zero-error" divByZeroMessage
intOp op (Number x) (Number y) = return $ Number $ x `op` y
intOp _  x          (Number _) = return $ Raised "type-error" (typeErrorMessage "Number" x)
intOp _  (Number _) y          = return $ Raised "type-error" (typeErrorMessage "Number" y)
intOp _  x          _          = return $ Raised "type-error" (typeErrorMessage "Number" x)

numOp :: (Double -> Double -> Double) -> LispVal -> LispVal -> Eval LispVal
numOp _  err@(Raised _ _) _    = return err
numOp _  _ err@(Raised _ _)    = return err
numOp op (Number x) (Number y) = return $ Number $ truncate $ fromInteger x `op` fromInteger y
numOp op (Float x)  (Float y)  = return $ Float $ x `op` y
numOp op (Number x) (Float y)  = return $ Float $ fromInteger x `op` y
numOp op (Float x)  (Number y) = return $ Float $ x `op` fromInteger y
numOp _  x          (Number _) = return $ Raised "type-error" (typeErrorMessage "Number" x)
numOp _  x          (Float _)  = return $ Raised "type-error" (typeErrorMessage "Float" x)
numOp _  (Number _) y          = return $ Raised "type-error" (typeErrorMessage "Number" y)
numOp _  (Float _)  y          = return $ Raised "type-error" (typeErrorMessage "Float" y)
numOp _  x          _          = return $ Raised "type-error" (typeErrorMessage "Float or Number" x)

numCmp :: (Double -> Double -> Bool) -> [LispVal] -> Eval LispVal
numCmp fn args | length args < 2 = return $ Raised "syntax-error" (numArgsMessage 2 args)
               | otherwise       = loop fn args
 where
    loop :: (Double -> Double -> Bool) -> [LispVal] -> Eval LispVal
    loop op (a:b:rest) = cmpOne op a b >>= \case
                             Bool True -> loop op (b:rest)
                             x         -> return x
    loop _  _          = return $ Bool True

    cmpOne :: (Double -> Double -> Bool) -> LispVal -> LispVal -> Eval LispVal
    cmpOne _  err@(Raised _ _) _    = return err
    cmpOne _  _ err@(Raised _ _)    = return err
    cmpOne op (Number x) (Number y) = return $ Bool $ fromInteger x `op` fromInteger y
    cmpOne op (Float x)  (Float y)  = return $ Bool $ x `op` y
    cmpOne op (Number x) (Float y)  = return $ Bool $ fromInteger x `op` y
    cmpOne op (Float x)  (Number y) = return $ Bool $ x `op` fromInteger y
    cmpOne _  x          (Number _) = return $ Raised "type-error" (typeErrorMessage "Number" x)
    cmpOne _  x          (Float _)  = return $ Raised "type-error" (typeErrorMessage "Float" x)
    cmpOne _  (Number _) y          = return $ Raised "type-error" (typeErrorMessage "Number" y)
    cmpOne _  (Float _)  y          = return $ Raised "type-error" (typeErrorMessage "Float" y)
    cmpOne _  x          _          = return $ Raised "type-error" (typeErrorMessage "Float or Number" x)

--
-- EQUIVALENCE
--

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp _  err@(Raised _ _ ) _ = return err
eqOp _  _ err@(Raised _ _)  = return err
eqOp op (Bool x) (Bool y)   = return $ Bool $ op x y
eqOp _  x        (Bool _)   = return $ Raised "type-error" (typeErrorMessage "Bool" x)
eqOp _  (Bool _) y          = return $ Raised "type-error" (typeErrorMessage "Bool" y)
eqOp _  x        _          = return $ Raised "type-error" (typeErrorMessage "Bool" x)

equivalent :: LispVal -> LispVal -> Eval LispVal
equivalent err@(Raised _ _ ) _              = return err
equivalent _ err@(Raised _ _ )              = return err
equivalent (Atom   x) (Atom   y)            = return . Bool $ x == y
equivalent x@(Bool _)      y@(Bool _)       = eqBoolean x y
equivalent x@(Character _) y@(Character _)  = eqCharacter x y
equivalent (Float x)  (Float y)             = return . Bool $ x == y
equivalent (List [])  (List [])             = return $ Bool True
equivalent Nil        Nil                   = return $ Bool True
equivalent (Number x) (Number y)            = return . Bool $ x == y
equivalent (String x) (String y)            = return . Bool $ x == y
equivalent _          _                     = return $ Bool False

--
-- TYPE MANIPULATIONS
--

isBoolean :: LispVal -> Eval LispVal
isBoolean err@(Raised _ _ ) = return err
isBoolean (Bool _)          = return $ Bool True
isBoolean _                 = return $ Bool False

eqBoolean :: LispVal -> LispVal -> Eval LispVal
eqBoolean = eqOp (==)

isCharacter :: LispVal -> Eval LispVal
isCharacter err@(Raised _ _) = return err
isCharacter (Character _)    = return $ Bool True
isCharacter _                = return $ Bool False

isCondition :: LispVal -> Eval LispVal
isCondition (Condition _ _)  = return $ Bool True
isCondition _                = return $ Bool False

eqCharacter :: LispVal -> LispVal -> Eval LispVal
eqCharacter x y = charCmp (==) [x, y]

charCmp :: (Char -> Char -> Bool) -> [LispVal] -> Eval LispVal
charCmp fn args | length args < 2 = return $ Raised "syntax-error" (numArgsMessage 2 args)
                | otherwise       = loop fn args
 where
    loop :: (Char -> Char -> Bool) -> [LispVal] -> Eval LispVal
    loop op (a:b:rest) = cmpOne op a b >>= \case
                             Bool True  -> loop op (b:rest)
                             x          -> return x
    loop _  _          = return $ Bool True

    cmpOne :: (Char -> Char -> Bool) -> LispVal -> LispVal -> Eval LispVal
    cmpOne _  err@(Raised _ _) _            = return err
    cmpOne _  _ err@(Raised _ _)            = return err
    cmpOne op (Character x) (Character y)   = return $ Bool $ x `op` y
    cmpOne _  x             (Character _)   = return $ Raised "type-error" (typeErrorMessage "Char" x)
    cmpOne _  (Character _) y               = return $ Raised "type-error" (typeErrorMessage "Char" y)
    cmpOne _  x          _                  = return $ Raised "type-error" (typeErrorMessage "Char" x)

isList :: LispVal -> Eval LispVal
isList err@(Raised _ _ ) = return err
isList (List _)          = return $ Bool True
isList _                 = return $ Bool False

isProcedure :: LispVal -> Eval LispVal
isProcedure err@(Raised _ _) = return err
isProcedure (Func _ _)       = return $ Bool True
isProcedure _                = return $ Bool False

isString :: LispVal -> Eval LispVal
isString err@(Raised _ _)   = return err
isString (String _)         = return $ Bool True
isString _                  = return $ Bool False

-- The most basic - all numeric types pass.
isNumber :: LispVal -> Eval LispVal
isNumber err@(Raised _ _)   = return err
isNumber (Float _)          = return $ Bool True
isNumber (Number _)         = return $ Bool True
isNumber _                  = return $ Bool False

-- Everything but complex numbers (which we don't support) pass.
isReal :: LispVal -> Eval LispVal
isReal (Float _)  = return $ Bool True
isReal (Number _) = return $ Bool True
isReal _          = return $ Bool False

-- Only integers pass.
isInteger :: LispVal -> Eval LispVal
isInteger (Number _) = return $ Bool True
isInteger _          = return $ Bool False

--
-- CHARACTERS
--

charToInteger :: LispVal -> Eval LispVal
charToInteger err@(Raised _ _)  = return err
charToInteger (Character c)     = return $ Number $ toInteger $ ord c
charToInteger x                 = return $ Raised "type-error" (typeErrorMessage "Char" x)

integerToChar :: LispVal -> Eval LispVal
integerToChar err@(Raised _ _)  = return err
integerToChar (Number n)        = return $ Character $ chr $ fromIntegral n
integerToChar x                 = return $ Raised "type-error" (typeErrorMessage "Number" x)

--
-- STRINGS
--

listToString :: LispVal -> LispVal
listToString err@(Raised _ _) = err
listToString (List l) = let
    doit accum []                   = String accum
    doit _     (err@(Raised _ _):_) = err
    doit accum (Character c:rest)   = doit (accum `T.append` T.singleton c) rest
    doit _ (x:_)                    = Raised "type-error" (typeErrorMessage "Character" x)
 in
    doit "" l
listToString x = Raised "type-error" (typeErrorMessage "List" x)

stringToList :: LispVal -> LispVal
stringToList err@(Raised _ _)   = err
stringToList (String s)         = List $ map Character (T.unpack s)
stringToList x                  = Raised "type-error" (typeErrorMessage "String" x)

--
-- LISTS
--

append :: LispVal -> LispVal -> Eval LispVal
append err@(Raised _ _) _ = return err
append _ err@(Raised _ _) = return err
append (List x) (List y)  = return $ List $ x ++ y
append (List _) y         = return $ Raised "type-error" (typeErrorMessage "List" y)
append x        (List _)  = return $ Raised "type-error" (typeErrorMessage "List" x)
append x        _         = return $ Raised "type-error" (typeErrorMessage "List" x)

cons :: [LispVal] -> Eval LispVal
cons [err@(Raised _ _), _]  = return err
cons [x, List yList]        = return $ List $ x:yList
cons [err@(Raised _ _)]     = return err
cons [c]                    = return $ List [c]
cons []                     = return $ List []
cons x                      = return $ Raised "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in cons: " : map showVal x)

car :: [LispVal] -> Eval LispVal
car [List []]                   = return $ Raised "list-error" (listErrorMessage "car")
car [List (err@(Raised _ _):_)] = return err
car [List (x:_)]                = return x
car [err@(Raised _ _)]          = return err
car [x]                         = return $ Raised "type-error" (typeErrorMessage "List" x)
car []                          = return Nil
car x                           = return $ Raised "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in car: " : map showVal x)

cdr :: [LispVal] -> Eval LispVal
cdr [List (err@(Raised _ _):_)] = return err
cdr [List (_:xs)]               = return $ List xs
cdr [List []]                   = return $ Raised "list-error" (listErrorMessage "cdr")
cdr [err@(Raised _ _)]          = return err
cdr [x]                         = return $ Raised "type-error" (typeErrorMessage "List" x)
cdr []                          = return Nil
cdr x                           = return $ Raised "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in cdr: " : map showVal x)

--
-- QUOTATION
--

quote :: [LispVal] -> Eval LispVal
quote [List xs]   = return $ List $ Atom "quote" : xs
quote [xp]        = return $ List $ Atom "quote" : [xp]
quote x           = return $ Raised "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in quote: " : map showVal x)

--
-- ERRORS
--

raise :: LispVal -> Eval LispVal
raise (Condition ty msg) = return $ Raised ty msg
raise err@(Raised _ _)   = return err
raise x                  = return $ Raised "type-error" (typeErrorMessage "Error" x)

--
-- IO
--

fileExists :: LispVal  -> Eval LispVal
fileExists err@(Raised _ _) = return err
fileExists (Atom atom)      = fileExists $ String atom
fileExists (String txt)     = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val              = return $ Raised "type-error" (typeErrorMessage "String" val)

slurp :: LispVal  -> Eval LispVal
slurp err@(Raised _ _)  = return err
slurp (String txt)      = liftIO $ withFile (T.unpack txt) ReadMode (readTextFile txt)
slurp val               = return $ Raised "type-error" (typeErrorMessage "String" val)

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile _ handle =
    fmap String (TIO.hGetContents handle)
