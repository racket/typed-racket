# Typed Racket

[![Build Status](https://travis-ci.org/racket/typed-racket.svg?branch=master)](https://travis-ci.org/racket/typed-racket)
[![Racket](https://img.shields.io/badge/-Made%20with%20Racket-darkred?logo=racket)](https://racket-lang.org)
[![Discourse users](https://img.shields.io/discourse/users?label=Discuss%20on%20Racket%20Discourse&logo=racket&server=https%3A%2F%2Fracket.discourse.group)](https://racket.discourse.group/)
[![Racket Discord](https://img.shields.io/discord/571040468092321801?label=Chat%20on%20Racket%20Discord&logo=racket)](https://discord.gg/6Zq8sH5)

Typed Racket is Racket's gradually-typed sister language which lets you add
statically-checked type annotations to your programs. For more information,
see the [Typed Racket Guide](http://docs.racket-lang.org/ts-guide/index.html).

Installation
------------

Typed Racket is bundled in the default Racket distribution, which you can download
from Racket's [download page](http://download.racket-lang.org/).

You can also manually install it from the main [package catalog](http://pkgs.racket-lang.org/)
with the following command:

  `raco pkg install typed-racket`

Documentation
-------------

  * Guide: http://docs.racket-lang.org/ts-guide/index.html
  * Reference: http://docs.racket-lang.org/ts-reference/index.html

The documentation is also bundled in your local copy of Typed Racket.

Directory Guide
---------------

The directory `rfcs` holds requests for changes to Typed Racket.

The directory `source-syntax` allows the `source-syntax` collection to report
errors in macro-expanded code by mapping each location in the expanded code to
the right location in the source file.

The directory `typed-racket` bundles together the Racket content of the two
directories `typed-racket-doc` and `typed-racket-lib`.

The directory `typed-racket-compatibility` provides Typed Racket under old names
for backwards compatibility.

The directory `typed-racket-doc` contains Scribble documentation.

The directory `typed-racket-lib` houses the actual implementation of Typed Racket.

The directory `typed-racket-more` has typed interfaces for additional libraries.

The directory `typed-racket-test` has test cases for Typed Racket.

License
-------

Racket, including these packages, is free software, see [LICENSE]
for more details.

By making a contribution, you are agreeing that your contribution
is licensed under the [Apache 2.0] license and the [MIT] license.

[MIT]: https://github.com/racket/racket/blob/master/racket/src/LICENSE-MIT.txt
[Apache 2.0]: https://www.apache.org/licenses/LICENSE-2.0.txt
[pull request]: https://github.com/racket/typed-racket/pulls
[issue]: https://github.com/racket/typed-racket/issues
[development mailing list]: https://lists.racket-lang.org
[LICENSE]: LICENSE
