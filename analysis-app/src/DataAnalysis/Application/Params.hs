{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Parameter parsing/template haskell code.

module DataAnalysis.Application.Params where

import           Control.Applicative
import           Control.Monad.Error
import           Data.Char
import           Data.Data
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.Read

-- | A parameter specification.
data Parameter a = Parameter
  { paramName :: !Text
  , paramType :: !Text
  , paramMin  :: !(Maybe a)
  , paramMax  :: !(Maybe a)
  , paramDesc :: !Text
  , paramDef  :: !(Maybe a)
  } deriving (Show)

-- | Parse a parameter spec from tokens.
fromTokens :: (Read a,Show a,Typeable a) => [Token] -> Either String (Parameter a)
fromTokens xs@(Token name:Spaces{}:Token typ:_) =
  Parameter
    <$> pure name
    <*> pure typ
    <*> opt parseRead "min"
    <*> opt parseRead "max"
    <*> text "desc"
    <*> opt parseRead "default"
  where opt :: (Text -> Either String a) -> Text -> Either String (Maybe a)
        opt cont name = do mv <- optional (text name)
                           case mv of
                             Nothing -> return Nothing
                             Just t -> fmap Just (cont t)
        text :: Text -> Either String Text
        text name =
          maybe (Left ("Couldn't find key: " <> T.unpack name))
                Right
                (listToMaybe (mapMaybe match xs))
          where match tok =
                  case tok of
                    Token t ->
                      case T.span (/='=') t of
                        (key,value)
                          | key == name && T.isPrefixOf "=" value -> Just (T.drop 1 value)
                        _ -> Nothing
                    _ -> Nothing
        parseRead :: (Typeable a,Read a) => Text -> Either String a
        parseRead t = r
          where r = case reads (T.unpack t) of
                      [(v,"")] -> Right v
                      _ -> Left ("Unable to parse value of type " <> (show (typeOf r)))
fromTokens _ = Left "Parameter format: <name> <type> [arg1=value1 argn=valuen …]"

-- | A token used by the parser.
data Token = Spaces !Int -- ^ @Spaces n@ are @n@ consecutive spaces.
           | Token Text  -- ^ @Token tok@ is token @tok@ already unquoted.
  deriving (Show, Eq)

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | "--" `T.isPrefixOf` t = [] -- Comment until the end of the line.
    | "#" `T.isPrefixOf` t = [] -- Also comment to the end of the
                                -- line, needed for a CPP bug (#110)
    | T.head t == '"' = quotes (T.tail t) id
    | T.head t == '(' = parens 1 (T.tail t) id
    | isSpace (T.head t) =
        let (spaces, rest) = T.span isSpace t
         in Spaces (T.length spaces) : tokenize rest

    -- support mid-token quotes and parens
    | Just (beforeEquals, afterEquals) <- findMidToken t
    , not (T.any isSpace beforeEquals)
    , Token next : rest <- tokenize afterEquals =
        Token (T.concat [beforeEquals, "=", next]) : rest

    | otherwise =
        let (token, rest) = T.break isSpace t
         in Token token : tokenize rest
  where
    findMidToken t' =
        case T.break (== '=') t' of
            (x, T.drop 1 -> y)
                | "\"" `T.isPrefixOf` y || "(" `T.isPrefixOf` y -> Just (x, y)
            _ -> Nothing

    quotes t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated quoted string starting with " : front []
        | T.head t' == '"' = Token (T.concat $ front []) : tokenize (T.tail t')
        | T.head t' == '\\' && T.length t' > 1 =
            quotes (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` "\\\"") t'
             in quotes y (front . (x:))
    parens count t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated parens string starting with " : front []
        | T.head t' == ')' =
            if count == (1 :: Int)
                then Token (T.concat $ front []) : tokenize (T.tail t')
                else parens (count - 1) (T.tail t') (front . (")":))
        | T.head t' == '(' =
            parens (count + 1) (T.tail t') (front . ("(":))
        | T.head t' == '\\' && T.length t' > 1 =
            parens count (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` "\\()") t'
             in parens count y (front . (x:))
