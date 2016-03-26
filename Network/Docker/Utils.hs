module Network.Docker.Utils where

import           Control.Lens hiding (makeLenses)
import           Data.Char
import qualified Data.List as List
import           Language.Haskell.TH

stripUnderscore :: String -> String
stripUnderscore ('_':xs) = xs
stripUnderscore xs = error $ List.intercalate " "
                     [ "stripUnderscore:"
                     , xs
                     , "does not start with an underscore"
                     ]

fixInvalidNames :: LensRules -> LensRules
fixInvalidNames rules = rules & lensField %~
                   (\lf -> \typeName fieldNames fieldName ->
                              substNames <$> lf typeName fieldNames fieldName)
  where
    substNames (TopName name) = TopName (fixName name)
    substNames (MethodName className methodName) =
        MethodName className (fixName methodName)
    fixName :: Name -> Name
    fixName name =
        let nb = nameBase name
        in case List.lookup nb subst of
            Nothing -> name
            Just replace -> mkName replace
      where
        subst = [ ("type", "type'")
                , ("default", "default'")
                ]

toClassNames :: LensRules -> LensRules
toClassNames rules =
  rules & lensField %~ (\lf -> \typeName fieldNames fieldName ->
                          classNames <$> lf typeName fieldNames fieldName)
  where
    classNames mn@MethodName{} = mn
    classNames (TopName name) =
      let base = nameBase name
          className =  "Has" ++ upcase base
      in MethodName (mkName className) (mkName  base)
    upcase (c:cs) = toUpper c : cs
    upcase [] = error "upcase: empty string"

fieldRules :: LensRules
fieldRules = toClassNames . fixInvalidNames $
               camelCaseFields & lensField .~ underscoreNoPrefixNamer


makeLenses :: Name -> DecsQ
makeLenses = makeLensesWith fieldRules
