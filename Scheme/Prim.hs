{-# LANGUAGE OverloadedStrings #-}

module Scheme.Prim(primEnv,
                   unop)
 where

import Scheme.Exceptions(LispException(..))
import Scheme.LispVal(Eval(..), IFunc(IFunc), LispVal(..), showVal)

import           Control.Exception(throw)
import           Control.Monad(foldM)
import           Control.Monad.IO.Class(liftIO)
import           Data.Monoid((<>))
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
            ("+",       mkF $ binopFold (numOp (+)) (Number 0)),
            ("-",       mkF $ binop     (numOp (-))),
            ("*",       mkF $ binopFold (numOp (*)) (Number 1)),
            ("even?",   mkF $ unop      (numBool even)),
            ("odd?" ,   mkF $ unop      (numBool odd)),

            -- Booleans.
            ("<",       mkF $ binop     (numCmp (<))),
            ("<=",      mkF $ binop     (numCmp (<=))),
            (">",       mkF $ binop     (numCmp (>))),
            ("<=",      mkF $ binop     (numCmp (>=))),
            ("==",      mkF $ binop     (numCmp (==))),
            ("and",     mkF $ binopFold (eqOp   (&&)) (Bool True)),
            ("or",      mkF $ binopFold (eqOp   (||)) (Bool False)),

            -- Equivalence.
            ("eqv?",    mkF $ binop     equivalent),

            -- Type predicates.
            ("boolean?",    mkF $ unop isBoolean),
            ("list?",       mkF $ unop isList),
            ("number?",     mkF $ unop isNumber),
            ("procedure?",  mkF $ unop isProcedure),
            ("string?",     mkF $ unop isString),

            -- Strings.
            ("++",      mkF $ binopFold (strOp (<>)) (String "")),

            -- Lists.
            ("cons",    mkF Scheme.Prim.cons),
            ("car",     mkF Scheme.Prim.car),
            ("cdr",     mkF Scheme.Prim.cdr),
            ("quote",   mkF quote),

            -- Code.
            ("print-ast",   mkF $ return . String . T.pack . show),

            -- IO.
            ("write",           mkF $ unop $ return . String . showVal),
            ("file-exists?",    mkF $ unop fileExists),
            -- FIXME: Replace these with real versions.
            ("slurp",           mkF $ unop slurp) ]

--
-- HELPERS
--

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = PrimitiveFunc . IFunc

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x]    = op x
unop _ args    = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = throw $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op _ [a, b]         = op a b
binopFold op farg args@(_:_)  = foldM op farg args
binopFold _ _ args@[]         = throw $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool _ x           = throw $ TypeMismatch "Number" x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _  x          (Number _) = throw $ TypeMismatch "Number" x
numOp _  (Number _) y          = throw $ TypeMismatch "Number" y
numOp _  x          _          = throw $ TypeMismatch "Number" x

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp _  x          (String _) = throw $ TypeMismatch "String " x
strOp _  (String _) y          = throw $ TypeMismatch "String " y
strOp _  x          _          = throw $ TypeMismatch "String " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp _  x        (Bool _) = throw $ TypeMismatch "Bool" x
eqOp _  (Bool _) y        = throw $ TypeMismatch "Bool" y
eqOp _  x        _        = throw $ TypeMismatch "Bool" x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x  y
numCmp _  x          (Number _) = throw $ TypeMismatch "Number" x
numCmp _  (Number _) y          = throw $ TypeMismatch "Number" y
numCmp _  x          _          = throw $ TypeMismatch "Number" x

equivalent :: LispVal -> LispVal -> Eval LispVal
equivalent (Atom   x) (Atom   y) = return . Bool $ x == y
equivalent (Number x) (Number y) = return . Bool $ x == y
equivalent (String x) (String y) = return . Bool $ x == y
equivalent (Bool   x) (Bool   y) = return . Bool $ x == y
equivalent (List [])  (List [])  = return $ Bool True
equivalent Nil        Nil        = return $ Bool True
equivalent _          _          = return $ Bool False

--
-- TYPE PREDICATES
--

isBoolean :: LispVal -> Eval LispVal
isBoolean (Bool _)  = return $ Bool True
isBoolean _         = return $ Bool False

isList :: LispVal -> Eval LispVal
isList (List _)     = return $ Bool True
isList _            = return $ Bool False

isNumber :: LispVal -> Eval LispVal
isNumber (Number _) = return $ Bool True
isNumber _          = return $ Bool False

isProcedure :: LispVal -> Eval LispVal
isProcedure (PrimitiveFunc _)   = return $ Bool True
isProcedure (Func _ _)          = return $ Bool True
isProcedure (Lambda _ _)        = return $ Bool True
isProcedure _                   = return $ Bool False

isString :: LispVal -> Eval LispVal
isString (String _) = return $ Bool True
isString _          = return $ Bool False

--
-- LISTS
--

cons :: [LispVal] -> Eval LispVal
cons [x, List yList]    = return $ List $ x:yList
cons [c]                = return $ List [c]
cons []                 = return $ List []
cons x                  = throw $ Unknown $ T.concat $ ["Error in cons: "] ++ map showVal x

car :: [LispVal] -> Eval LispVal
car [List []]     = return Nil
car [List (x:_)]  = return x
car []            = return Nil
car x             = throw $ Unknown $ T.concat $ ["Error in car: "] ++ map showVal x

cdr :: [LispVal] -> Eval LispVal
cdr [List (_:xs)] = return $ List xs
cdr [List []]     = return Nil
cdr []            = return Nil
cdr x             = throw $ Unknown $ T.concat $ ["Error in cdr: "] ++ map showVal x

quote :: [LispVal] -> Eval LispVal
quote [List xs]   = return $ List $ Atom "quote" : xs
quote [xp]        = return $ List $ Atom "quote" : [xp]

--
-- IO
--

fileExists :: LispVal  -> Eval LispVal
fileExists (Atom atom)  = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val          = throw $ TypeMismatch "read expects string, instead got: " val

slurp :: LispVal  -> Eval LispVal
slurp (String txt) = liftIO $ withFile (T.unpack txt) ReadMode (readTextFile txt)
slurp val          = throw $ TypeMismatch "read expects string, instead got: " val

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile _ handle =
    TIO.hGetContents handle >>= return . String
