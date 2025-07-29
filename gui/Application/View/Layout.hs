module Application.View.Layout where

import IHP.ViewPrelude

layout :: Html -> Html
layout inner = [hsx|<html><body>{inner}</body></html>|]
