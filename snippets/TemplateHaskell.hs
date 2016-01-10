{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskell where

import Data.List (intercalate)
import Language.Haskell.TH

emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = "empty"|]

listFields :: Name -> Q [Dec]
listFields name = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
    let names = map (\(name, _, _) -> name) fields
    let showField :: Name -> Q Exp
        showField name = let s = nameBase name in [|\x -> s ++ " = " ++ show ($(varE name) x)|]
    let showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Show $(conT name) where show x = intercalate ", " (map ($ x) $showFields)|]
