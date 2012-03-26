# About

> Parse [Hackage][] upload log and write the latest version of each package
> into a [JSONP][] file.

An up-to-date version is repeatedly generated at
<http://www.typeful.net/~tbot/hackage-package-versions.jsonp>.  The callback is
hard-coded to `hackagePackageVersionsCallback`.

## Example

### `index.html`

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

### `custom.js`

```javascript
$(document).ready(function() {

  /* fetch latest packages versions, the result will be passed to
   * hackagePackageVersionsCallback(..) */
  $.getScript("http://www.typeful.net/~tbot/hackage-package-versions.jsonp");
});

function hackagePackageVersionsCallback(response) {

  /* output latest version of hspec */
  var version = $(response).attr("hspec");
  $("#result").text(version);
}
```

[Hackage]: http://hackage.haskell.org/packages/hackage.html
[JSONP]: http://en.wikipedia.org/wiki/JSONP
