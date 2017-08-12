# Work with URLs

This library has two primary modules:

  - `Url.Builder` to build URLs for HTTP requests
  - `Url.Parser` to parse URLs for “routing” in single-page apps (SPAs)

Check out those modules for specific usage examples!

<br>

## What is a URL?

A URL is defined by Tim Breners-Lee in [this document](https://tools.ietf.org/html/rfc3986). It is worth reading, but I will try to share some highlights. He shares an example like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

And here are some facts that I found surprising:

  - **ASCII only** &mdash; Behavior with other encodings is undefined in the spec. So a browser may handle a non-ASCII character one way, while this library handles it another. No one is wrong. It is just unspecified. I would stick to ASCII to be safe.

  - **Escaping** &mdash; The spec uses the term [percent-encoded](https://tools.ietf.org/html/rfc3986#section-2.1) to reduce confusion with other escape mechanisms. The format is `%XX` allowing two hex digits. According to [this table of ASCII characters](http://ascii.cl/), you could encode `?` as `%3F` and `=` as `%3D`. There are many subtlties though, so I recommend reading [this](https://en.wikipedia.org/wiki/Percent-encoding) for more information!

<br>

> **Note:** The difference between a URI and a URL is kind of subtle. [This post](https://danielmiessler.com/study/url-uri/) explains the difference nicely. I decided to call this library `elm-lang/url` because it is primarily concerned with HTTP which does need actual locations.

<br>

## Related Work

The API in `Url.Parser` is quite distinctive. I first saw the general idea in Chris Done&rsquo;s [formatting][] library. Based on that, Noah and I outlined the API you see in `Url.Parser`. Noah then found Rudi Grinberg&rsquo;s [post][] about type safe routing in OCaml. It was exactly what we were going for. We had even used the names `s` and `(</>)` in our draft API! In the end, we ended up using the “final encoding” of the EDSL that had been left as an exercise for the reader. Very fun to work through!

[formatting]: http://chrisdone.com/posts/formatting
[post]: http://rgrinberg.com/posts/primitive-type-safe-routing/
