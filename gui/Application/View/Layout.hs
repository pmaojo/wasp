module Application.View.Layout where

import IHP.ViewPrelude

layout :: Html -> Html
layout inner = [hsx|
    <html>
        <body>
            <nav><a href="/Admin">Admin</a></nav>
            {inner}
        </body>
    </html>
|]
