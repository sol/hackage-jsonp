# About

> Parse the [Hackage][] upload log and write the latest version of each package
> into a JSON/JSONP file.

Up-to-date versions of both JSON and JSONP are repeatedly generated at
<http://www.typeful.net/~tbot/hackage/>.  The JSONP callback is hard-coded to
`hackagePackageVersionsCallback`.

## Examples

### Haskell (aeson/syb)

```haskell
import           Data.Maybe
import qualified Data.Map as Map
import           Network.URI
import           Network.HTTP
import           Data.Aeson.Generic

url :: String
url = "http://www.typeful.net/~tbot/hackage/latest-package-versions.json"

-- | Return latest version of a package.
--
-- >>> getVersion "hspec"
-- Just "0.9.2"
getVersion :: String -> IO (Maybe String)
getVersion name = do
  r <- simpleHTTP request >>= getResponseBody
  return (decode r >>= Map.lookup name)
  where
    request = (mkRequest GET . fromJust . parseURI) url
```

### HTML/JavaScript

[JSONP][] allows a javascript client to relax the same origin policy and retrieve
the above JSON by providing the callback function
`hackagePackageVersionsCallback`.

#### `index.html`

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Example</title>
  </head>
  <body>
    <p>
      The latest version of hspec is <span id="result">???</span>.
    </p>
    <script src="http://code.jquery.com/jquery-1.7.2.min.js"></script>
    <script src="custom.js"></script>
  </body>
</html>
```

#### `custom.js`

```javascript
$(document).ready(function() {

  /* fetch latest packages versions, the result will be passed to
   * hackagePackageVersionsCallback(..) */
  $.getScript("http://www.typeful.net/~tbot/hackage/latest-package-versions.jsonp");
});

function hackagePackageVersionsCallback(response) {

  /* output latest version of hspec */
  var version = $(response).attr("hspec");
  $("#result").text(version);
}
```

[Hackage]: http://hackage.haskell.org/packages/hackage.html
[JSONP]: http://en.wikipedia.org/wiki/JSONP
