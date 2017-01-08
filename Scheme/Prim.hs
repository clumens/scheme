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
            ("<",       mkF $ numCmp    (<)),
            ("<=",      mkF $ numCmp    (<=)),
            (">",       mkF $ numCmp    (>)),
            (">=",      mkF $ numCmp    (>=)),
            ("=",       mkF $ numCmp    (==)),
            ("div",     mkF $ binop     (numOp div)),
            ("mod",     mkF $ binop     (numOp mod)),

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
            ("list?",       mkF $ unop isList),
            ("number?",     mkF $ unop isNumber),
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
unop _ args     = return $ Error "syntax-error" (numArgsMessage 1 args)

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = return $ Error "syntax-error" (numArgsMessage 1 args)

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op _ [a, b]         = op a b
binopFold op farg args@(_:_)  = foldM op farg args
binopFold _ _ args@[]         = return $ Error "syntax-error" (numArgsMessage 2 args)

--
-- NUMBERS
--

numCmp :: (Integer -> Integer -> Bool) -> [LispVal] -> Eval LispVal
numCmp fn args | length args < 2 = return $ Error "syntax-error" (numArgsMessage 2 args)
               | otherwise       = loop fn args
 where
    loop :: (Integer -> Integer -> Bool) -> [LispVal] -> Eval LispVal
    loop op (a:b:rest) = cmpOne op a b >>= \case
                             Bool True  -> loop op (b:rest)
                             x          -> return x
    loop _  _          = return $ Bool True

    cmpOne :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
    cmpOne op (Number x) (Number y) = return $ Bool $ x `op` y
    cmpOne _  x          (Number _) = return $ Error "type-error" (typeErrorMessage "Number" x)
    cmpOne _  (Number _) y          = return $ Error "type-error" (typeErrorMessage "Number" y)
    cmpOne _  x          _          = return $ Error "type-error" (typeErrorMessage "Number" x)

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _  x          (Number _) = return $ Error "type-error" (typeErrorMessage "Number" x)
numOp _  (Number _) y          = return $ Error "type-error" (typeErrorMessage "Number" y)
numOp _  x          _          = return $ Error "type-error" (typeErrorMessage "Number" x)

--
-- EQUIVALENCE
--

equivalent :: LispVal -> LispVal -> Eval LispVal
equivalent (Atom   x) (Atom   y)            = return . Bool $ x == y
equivalent x@(Bool _)      y@(Bool _)       = eqBoolean x y
equivalent x@(Character _) y@(Character _)  = eqCharacter x y
equivalent (List [])  (List [])             = return $ Bool True
equivalent Nil        Nil                   = return $ Bool True
equivalent (Number x) (Number y)            = return . Bool $ x == y
equivalent (String x) (String y)            = return . Bool $ x == y
equivalent _          _                     = return $ Bool False

--
-- TYPE MANIPULATIONS
--

isBoolean :: LispVal -> Eval LispVal
isBoolean (Bool _)  = return $ Bool True
isBoolean _         = return $ Bool False

eqBoolean :: LispVal -> LispVal -> Eval LispVal
eqBoolean = eqOp (==)

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp _  x        (Bool _) = return $ Error "type-error" (typeErrorMessage "Bool" x)
eqOp _  (Bool _) y        = return $ Error "type-error" (typeErrorMessage "Bool" y)
eqOp _  x        _        = return $ Error "type-error" (typeErrorMessage "Bool" x)

isCharacter :: LispVal -> Eval LispVal
isCharacter (Character _) = return $ Bool True
isCharacter _             = return $ Bool False

isCondition :: LispVal -> Eval LispVal
isCondition (Error _ _) = return $ Bool True
isCondition _           = return $ Bool False

eqCharacter :: LispVal -> LispVal -> Eval LispVal
eqCharacter x y = charCmp (==) [x, y]

charCmp :: (Char -> Char -> Bool) -> [LispVal] -> Eval LispVal
charCmp fn args | length args < 2 = return $ Error "syntax-error" (numArgsMessage 2 args)
                | otherwise       = loop fn args
 where
    loop :: (Char -> Char -> Bool) -> [LispVal] -> Eval LispVal
    loop op (a:b:rest) = cmpOne op a b >>= \case
                             Bool True  -> loop op (b:rest)
                             x          -> return x
    loop _  _          = return $ Bool True

    cmpOne :: (Char -> Char -> Bool) -> LispVal -> LispVal -> Eval LispVal
    cmpOne op (Character x) (Character y) = return $ Bool $ x `op` y
    cmpOne _  x             (Character _) = return $ Error "type-error" (typeErrorMessage "Char" x)
    cmpOne _  (Character _) y             = return $ Error "type-error" (typeErrorMessage "Char" y)
    cmpOne _  x          _                = return $ Error "type-error" (typeErrorMessage "Char" x)

isList :: LispVal -> Eval LispVal
isList (List _)     = return $ Bool True
isList _            = return $ Bool False

isNumber :: LispVal -> Eval LispVal
isNumber (Number _) = return $ Bool True
isNumber _          = return $ Bool False

isProcedure :: LispVal -> Eval LispVal
isProcedure (Func _ _)  = return $ Bool True
isProcedure _           = return $ Bool False

isString :: LispVal -> Eval LispVal
isString (String _) = return $ Bool True
isString _          = return $ Bool False

--
-- CHARACTERS
--

charToInteger :: LispVal -> Eval LispVal
charToInteger (Character c) = return $ Number $ toInteger $ ord c
charToInteger x             = return $ Error "type-error" (typeErrorMessage "Char" x)

integerToChar :: LispVal -> Eval LispVal
integerToChar (Number n) = return $ Character $ chr $ fromIntegral n
integerToChar x          = return $ Error "type-error" (typeErrorMessage "Number" x)

--
-- STRINGS
--

listToString :: LispVal -> LispVal
listToString (List l) = let
    doit accum []                   = String accum
    doit accum (Character c:rest)   = doit (accum `T.append` T.singleton c) rest
    doit _ (x:_)                    = Error "type-error" (typeErrorMessage "Character" x)
 in
    doit "" l
listToString x        = Error "type-error" (typeErrorMessage "List" x)

stringToList :: LispVal -> LispVal
stringToList (String s) = List $ map Character (T.unpack s)
stringToList _          = List []

--
-- LISTS
--

append :: LispVal -> LispVal -> Eval LispVal
append (List x) (List y) = return $ List $ x ++ y
append (List _) y        = return $ Error "type-error" (typeErrorMessage "List" y)
append x        (List _) = return $ Error "type-error" (typeErrorMessage "List" x)
append x        _        = return $ Error "type-error" (typeErrorMessage "List" x)

cons :: [LispVal] -> Eval LispVal
cons [x, List yList]    = return $ List $ x:yList
cons [c]                = return $ List [c]
cons []                 = return $ List []
cons x                  = return $ Error "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in cons: " : map showVal x)

car :: [LispVal] -> Eval LispVal
car [List []]     = return Nil
car [List (x:_)]  = return x
car [x]           = return $ Error "type-error" (typeErrorMessage "List" x)
car []            = return Nil
car x             = return $ Error "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in car: " : map showVal x)

cdr :: [LispVal] -> Eval LispVal
cdr [List (_:xs)] = return $ List xs
cdr [List []]     = return Nil
cdr [x]           = return $ Error "type-error" (typeErrorMessage "List" x)
cdr []            = return Nil
cdr x             = return $ Error "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in cdr: " : map showVal x)

--
-- QUOTATION
--

quote :: [LispVal] -> Eval LispVal
quote [List xs]   = return $ List $ Atom "quote" : xs
quote [xp]        = return $ List $ Atom "quote" : [xp]
quote x           = return $ Error "undefined-error" (undefinedErrorMessage $ T.concat $ "Error in quote: " : map showVal x)

--
-- IO
--

fileExists :: LispVal  -> Eval LispVal
fileExists (Atom atom)  = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val          = return $ Error "type-error" (typeErrorMessage "String" val)

slurp :: LispVal  -> Eval LispVal
slurp (String txt) = liftIO $ withFile (T.unpack txt) ReadMode (readTextFile txt)
slurp val          = return $ Error "type-error" (typeErrorMessage "String" val)

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile _ handle =
    fmap String (TIO.hGetContents handle)
